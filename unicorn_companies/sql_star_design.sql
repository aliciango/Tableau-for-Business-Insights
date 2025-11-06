DROP DATABASE SLEEKMART_OMS;
-- ==============================================================
-- DATA IMPORTING
-- ==============================================================
--CREATE DATABASE DB_UNICORN;


-- ==============================================================
-- DATA IMPORTING FROM CSV
-- ==============================================================
CREATE OR REPLACE VIEW companies_ls_22 AS (
SELECT
  *
FROM
  "DB_UNICORN"."PUBLIC"."UNICORNS_22");

CREATE OR REPLACE VIEW companies_ls_23 AS (
SELECT
  *
FROM
  "DB_UNICORN"."PUBLIC"."UNICORNS_23");

CREATE OR REPLACE VIEW companies_ls_24 AS (
SELECT
  *
FROM
  "DB_UNICORN"."PUBLIC"."UNICORNS_24");


-- ==============================================================
-- VIEW_INVESTORS
-- ==============================================================
CREATE OR REPLACE VIEW view_investors AS (
SELECT
    company_cleaned,
    select_investors,
    funding,
    funding_currency
FROM companies_ls_22);


-- ==============================================================
-- VIEW_COMPANIES
-- Information about companies, enriched with image and link info
-- ==============================================================
CREATE OR REPLACE VIEW view_companies AS (
SELECT distinct
    c22.company_cleaned,
    c22.year_founded,
    c22.country,
    c22.continent,
    c22.industry,
    c24.img_src,
    c24.company_link
FROM companies_ls_22 AS c22
INNER JOIN companies_ls_24 AS c24
    ON c22.company_cleaned = c24.company_cleaned);


-- ==============================================================
-- VIEW_TIMES_TO_UNICORN
-- Information about time taken for each company to become a unicorn
-- ==============================================================
CREATE OR REPLACE VIEW view_times_to_unicorn AS (
SELECT
    DISTINCT company_cleaned,
    DATE_PART('year', date_joined) - year_founded AS years_to_unicorn
FROM companies_ls_22);


-- ==============================================================
-- VIEW_STATUS_CHANGES
-- Tracks company unicorn status changes over years
-- ==============================================================
CREATE OR REPLACE VIEW view_status_changes AS (
SELECT DISTINCT company_cleaned AS company, 2022 AS unicorn_record_year, valuation
FROM companies_ls_22
UNION ALL
SELECT DISTINCT company_cleaned AS company, 2023 AS unicorn_record_year, valuation
FROM companies_ls_23
UNION ALL
SELECT DISTINCT company_cleaned AS company, 2024 AS unicorn_record_year, valuation
FROM companies_ls_24);


-- ==============================================================
-- CREATE TABLE FOR TABLEAU
-- QUERIES
-- ==============================================================
-- Validate if all views are working correctly
SELECT * FROM VIEW_COMPANIES; 
SELECT * FROM VIEW_INVESTORS;
SELECT * FROM VIEW_TIMES_TO_UNICORN;
SELECT * FROM VIEW_STATUS_CHANGES;

-- pivot VIEW_STATUS_CHANGES
CREATE OR REPLACE VIEW view_companies_full AS
WITH pivoted_status_changes AS (
    SELECT 
        company,
        CASE WHEN unicorn_record_year = 2022 THEN valuation END AS val_2022,
        CASE WHEN unicorn_record_year = 2023 THEN valuation END AS val_2023,
        CASE WHEN unicorn_record_year = 2024 THEN valuation END AS val_2024
    FROM view_status_changes
)
SELECT 
    company, 
    -- COMPANY-LEVEL VALUATION CHANGE 
    SUM(val_2022) AS val_2022, 
    SUM(val_2023) AS val_2023, 
    SUM(val_2024) AS val_2024,
    ROUND( (SUM(val_2023) - SUM(val_2022))*100::numeric / SUM(val_2022),2) as pct_22_23,
    ROUND( (SUM(val_2024) - SUM(val_2023))*100::numeric / SUM(val_2023),2) as pct_23_24
FROM pivoted_status_changes
GROUP BY company
HAVING 
    COUNT(val_2022) >= 1 
    AND COUNT(val_2023) >= 1 
    AND COUNT(val_2024) >= 1;
-- companies that have information for all three years: 681 companies


-- ==============================================================
-- EXECUTIVE SUMMARY
-- Industry growth over time
-- ==============================================================
SELECT 
    -- companies table
    c.industry
    -- companies_full table
    -- row-level calculation of valuation change for each company
    , ROUND( AVG(cf.pct_22_23),2) avg_industry_pct_22_23
    , ROUND( AVG(cf.pct_23_24),2) avg_industry_pct_23_24
FROM VIEW_COMPANIES c
INNER JOIN VIEW_COMPANIES_FULL cf ON c.company_cleaned = cf.company
GROUP BY c.industry
ORDER BY avg_industry_pct_22_23 DESC;

-- ==============================================================
-- EXECUTIVE SUMMARY
-- Comparing the growth of emerging markets: Brazil, Russia, India, China, and South Africa
-- with that of established markets
-- ==============================================================
WITH markets AS (
    SELECT 
        company_cleaned as company, 
        CASE 
            WHEN country IN ('Brazil', 'Russia', 'India', 'China', 'South Africa') 
            THEN 'emerging market' 
            ELSE 'established market' 
        END AS market_cat
    FROM view_companies
) 
SELECT  
    m.market_cat,
    -- average percentage change per industry
    ROUND(AVG(cf.pct_22_23), 2) AS avg_industry_pct_22_23,
    ROUND(AVG(cf.pct_23_24), 2) AS avg_industry_pct_23_24
FROM markets m
INNER JOIN view_companies_full cf 
    ON m.company = cf.company
GROUP BY m.market_cat
ORDER BY avg_industry_pct_22_23 DESC;


-- ==============================================================
-- EXECUTIVE SUMMARY
-- Investor's performance YoY, using two different growth metrics: # of portfolio companies, total portfolio valuation
-- ============================================================
-- join prep: portfolio companies by each investor each year
-- actually the number of portfolio cannot be used as a growth metrics because the number of portfolio companies even shrinked in 23 and 24 dataset, which is due to the data insufficiency
-- ==========================
-- NUMBER OF PORTFOLIO COMPS FOR EACH INVESTOR
SELECT 
    c22.select_investors AS investor
    , COUNT(c22.company_cleaned) cnt_portfol_comp_22
FROM companies_ls_22 c22
group by c22.select_investors 
ORDER BY cnt_portfol_comp_22 DESC
LIMIT 5;

SELECT 
    c23.select_investors AS investor
    , COUNT(c23.company_cleaned) cnt_portfol_comp_23
FROM companies_ls_23 c23
ORDER BY cnt_portfol_comp_23 DESC;

SELECT 
    c24.select_investors AS investor
    , COUNT(c24.company_cleaned) cnt_portfol_comp_24
FROM companies_ls_24 c24
ORDER BY cnt_portfol_comp_24 DESC;

-- ==========================
-- the total valuation of companies that have data in all three years
-- TOTAL PORTFOLIO VALUATION 

CREATE OR REPLACE VIEW total_portfol_vals AS (
    WITH valid_comp_investors AS (
        SELECT 
            c22.select_investors AS investor
            , c22.company_cleaned AS company
        FROM companies_ls_22 c22
            INNER JOIN companies_ls_23 c23 ON c22.company_cleaned = c23.company_cleaned AND c22.select_investors = c23.select_investors
            INNER JOIN companies_ls_24 c24 ON c24.company_cleaned = c22.company_cleaned AND c23.select_investors = c24.select_investors)
        
    SELECT 
        v.investor
        , SUM(cf.val_2022) total_portfol_val_22
        , SUM(cf.val_2023) total_portfol_val_23
        , SUM(cf.val_2024) total_portfol_val_24
    FROM valid_comp_investors v
    INNER JOIN view_companies_full cf 
        ON v.company = cf.company
    GROUP BY v.investor
    ORDER BY total_portfol_val_22 DESC
);

-- TOTAL PORTFOLIO VALUATION % CHANGE YoY
SELECT 
    *
    , ROUND( (total_portfol_val_23 - total_portfol_val_22)*100::numeric / total_portfol_val_22,2) as pct_portfol_val_22_23
    , ROUND( (total_portfol_val_24 - total_portfol_val_23)*100::numeric / total_portfol_val_23,2) as pct_portfol_val_22_23,
FROM total_portfol_vals;


-- ============================================================
-- COMPANY PERFORMANCE DEEP DIVE
-- Portfol companies that has lower-than-average valuation of its industry
-- using 2022 dataset
-- ============================================================
-- note that the data is highly skewed, right_skewed with lots of high number outliers.

WITH medians AS (
    SELECT
        DISTINCT 
        industry,
        company_cleaned AS company,
        valuation AS company_val,
        ROUND(MEDIAN(valuation) OVER (PARTITION BY industry), 2) AS industry_med_val
    FROM companies_ls_22
)
SELECT
    v.select_investors AS investor,
    m.company,
    m.company_val,
    CASE 
        WHEN m.company_val >= m.industry_med_val THEN 'higher' 
        ELSE 'lower' 
    END AS ind_benchmark
FROM medians m
RIGHT JOIN VIEW_INVESTORS v ON v.company_cleaned = m.company
ORDER BY investor;


-- ============================================================
-- REGIONAL EXPOSURE AND RISK DEEP DIVE
-- How many percentage of our portfolio is concentrated in one geographic region / continent
-- ============================================================
WITH investor_continents AS (
    SELECT 
        select_investors, 
        SUM(valuation) AS continent_sum_val
    FROM companies_ls_22
    GROUP BY select_investors
)
SELECT
    c22.select_investors,
    c22.continent,
    ROUND( (SUM(c22.valuation)*100)::numeric / c.continent_sum_val, 2) AS continent_pct
FROM companies_ls_22 c22
LEFT JOIN investor_continents c 
    ON c22.select_investors = c.select_investors
GROUP BY 
    c22.select_investors,
    c22.continent,
    c.continent_sum_val
ORDER BY 
    c22.select_investors,
    c22.continent;

SELECT *
FROM view_companies_full;
    
