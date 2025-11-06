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
-- ONE BIG TABLE
-- ==============================================================
SELECT company, val_2022, val_2023, val_2024, pct_22_23, pct_23_24 FROM VIEW_COMPANIES_FULL;
SELECT company_cleaned AS company, select_investors as investor, funding, funding_currency FROM VIEW_INVESTORS; -- MANY TO MANY
SELECT company_cleaned AS company, year_founded, country, industry, img_src, company_link FROM VIEW_COMPANIES; -- ONE TO ONE
SELECT company, unicorn_record_year, valuation as valuation_at_record FROM VIEW_STATUS_CHANGES; -- ONE TO ONE
SELECT company_cleaned AS company, years_to_unicorn FROM view_times_to_unicorn; -- ONE TO ONE


CREATE OR REPLACE VIEW one_big_table AS (
SELECT 
    c.company,
    c.val_2022,
    c.val_2023,
    c.val_2024,
    c.pct_22_23,
    c.pct_23_24,
    
    -- From VIEW_INVESTORS (many-to-many)
    inv.select_investors AS investor,
    inv.funding,
    inv.funding_currency,
    
    -- From VIEW_COMPANIES (one-to-one)
    comp.year_founded,
    comp.country,
    comp.continent,
    comp.industry,
    comp.img_src,
    comp.company_link,
    
    -- From VIEW_STATUS_CHANGES (one-to-one)
    st.unicorn_record_year,
    st.valuation AS valuation_at_record,
    
    -- From view_times_to_unicorn (one-to-one)
    ttu.years_to_unicorn

FROM VIEW_COMPANIES_FULL AS c

-- many-to-many join (investors)
LEFT JOIN VIEW_INVESTORS AS inv
    ON c.company = inv.company_cleaned

-- one-to-one joins
LEFT JOIN VIEW_COMPANIES AS comp
    ON c.company = comp.company_cleaned

LEFT JOIN VIEW_STATUS_CHANGES AS st
    ON c.company = st.company

LEFT JOIN view_times_to_unicorn AS ttu
    ON c.company = ttu.company_cleaned
);

-- ==============================================================
-- ONE BIG TABLE
-- ==============================================================

SELECT COUNT(DISTINCT company) FROM one_big_table; -- return 681 companies, which matches the number of distinct companies that have information for all three years

SELECT * FROM ONE_BIG_TABLE;


SELECT DISTINCT COUNTRY, CONTINENT FROM COMPANIES_LS_22;


SELECT *
FROM one_big_table
WHERE LOWER(investor) LIKE 'and sequoia%';

-- ==============================================================
-- ONE BIG TABLE WITH PORTFOLIO COMPANY AS AN ARRAY
-- ==============================================================

CREATE OR REPLACE VIEW companies_under_management AS (
  SELECT 
      investor,
      ARRAY_AGG(DISTINCT company) AS companies_as_array
  FROM one_big_table
  GROUP BY investor
);
SELECT * FROM companies_under_management;

SELECT investor
FROM companies_under_management
WHERE ARRAY_CONTAINS('SPACEX'::variant, companies_as_array);

