import pandas as pd
import numpy as np

df_2022_copy = pd.read_csv('pivoted_2022.csv')
df_2023_copy = pd.read_csv('Cleaned_Unicorn_Companies_2023.csv')
df_2024_copy = pd.read_csv('Cleaned_Unicorn_Companies_2024.csv')


df_2022 = df_2022_copy.copy()
df_2023 = df_2023_copy.copy()
df_2024 = df_2024_copy.copy()

# change name of the companies so that they become distinct from each other
## 2022 dataset update
# add a company_cleaned column
df_2022['company_cleaned'] = df_2022['company'].str.upper()
updates = [
    ("BOLT", "Tallinn", "BOLT TALLINN"),
    ("BRANCH", "Redwood City", "BRANCH REDWOOD CITY"),
    ("FABRIC", "Bellevue", "FABRIC BELLEVUE"),
    ("FABRIC", "New York", "FABRIC NEW YORK")
]

# Use a loop to save time
for old_name, city_name, new_name in updates:
    df_2022.loc[
        (df_2022["company_cleaned"] == old_name) & (df_2022["city"] == city_name),
        "company_cleaned"
    ] = new_name

# validate
df_2022.query('(company=="Bolt")& (city=="Tallinn")')




# do the same for 2023 dataset
# Define all replacements as (company_cleaned, city, new_value)
updates_2023 = [
    ("BOLT", "San Francisco", "BOLT SAN FRANCISCO"),
    ("BOLT", "Tallinn", "BOLT TALLINN"),
    ("BRANCH", "Redwood City", "BRANCH REDWOOD CITY"),
    ("BRANCH", "Columbus", "BRANCH COLUMBUS"),
    ("FABRIC", "Bellevue", "FABRIC BELLEVUE"),
    ("FABRIC", "New York", "FABRIC NEW YORK")
]

# Apply updates in a loop
for old_name, city_name, new_name in updates_2023:
    df_2023.loc[
        (df_2023["company_cleaned"] == old_name) & (df_2023["city"] == city_name),
        "company_cleaned"
    ] = new_name
# validate
df_2022.query('(company=="Bolt")& (city=="San Francisco")')




# as 2024 dataset miss the columns for city, we have to use the index to access the row values and update them
df_2024.query('(company_cleaned == "BRANCH") | (company_cleaned == "FIGURE")').sort_values(by='company_cleaned')
df_2024.columns

# Define all replacements as (company_cleaned, substring_in_company_link, new_value)
updates_2024 = [
    ("BRANCH", "branch-metrics", "BRANCH REDWOOD CITY"),
    ("BRANCH", "branch-financial", "BRANCH COLUMBUS"),
    ("FIGURE", "figure-66e9", "FIGURE SAN FRANCISCO"),
    ("FIGURE", "figure-b5dc", "FIGURE SAN JOSE")
]

# Apply updates in a loop
for old_name, link_substr, new_name in updates_2024:
    df_2024.loc[
        (df_2024["company_cleaned"] == old_name) &
        (df_2024["company_link"].str.contains(link_substr, case=False, na=False)),
        "company_cleaned"
    ] = new_name

# validate
df_2024.query('(company=="Branch") & (company_link=="https://www.crunchbase.com/organization/branch-metrics")')

df_2023.drop('date_joined', axis=1)
df_2022.columns

# set index to False because if the index column is included, the header/column name array will have a missing value for the index column
df_2022.to_csv('./cleaned_data/cleaned_unicorn_companies_2022.csv',index=False)
df_2023.to_csv('./cleaned_data/cleaned_unicorn_companies_2023.csv', index=False)
df_2024.to_csv('./cleaned_data/cleaned_unicorn_companies_2024.csv', index=False)

df_2022['select_investors']