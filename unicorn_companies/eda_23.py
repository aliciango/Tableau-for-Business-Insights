import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

df_2022_copy = pd.read_csv('Cleaned_Unicorn_Companies.csv')
df_2023_copy = pd.read_csv('unicorn_companies_2023.csv')

df_2022 = df_2022_copy.copy()
df_2023 = df_2023_copy.copy()

######################
## A. Data Discovery and Initial Cleaning
######################
# A. Column Standardization: map 2023 columns' names to match 2022 columns' names
df_2023.columns = ['company', 'valuation', 'date_joined', 'country/region', 'city', 'industry', 'select_investors']
df_2023.columns
df_2023.shape # 1206 x 7
df_2023.info()
df_2023.head(5)


# A. drop null values in valuation and company columns of 2023 dataset
df_2023 = df_2023[(df_2023['valuation'].notnull()) & (df_2023['company'].notnull())]
df_2023.shape  # 1059 x 7, some companies have same names but are actually different companies

# A. string cleaning
# Clean 'company' column: strip leading and trailing spaces
df_2022['company_cleaned'] = df_2022['company'].str.strip().str.upper()
df_2023['company_cleaned'] = df_2023['company'].str.strip().str.upper()

# A. find duplicates in company names in 2023 dataset
duplicate_companies_23 = df_2023[df_2023.duplicated(subset=['company'], keep=False)]
duplicate_companies_23.sort_values(by='company')
# 6 companies have same name
# Bolt, San Francisco
# Bolt, Tallinn
# Branch, Redwood City
# Branch, Columbus
# Fabric, Bellevue
# Fabric, New York
# companies have same name but different date_joined, join the tables later by using multi-index ('company', 'city')


# A. missing values handling
# Identified missing values using df.isnull().sum()
# Filled missing values in city and select_investors with "Unknown".
df_2023['city'] = df_2023['city'].fillna('Unknown')
df_2023['select_investors'] = df_2023['select_investors'].fillna('Unknown')
df_2023.isnull().sum()


# A. data types correction and mappig data of 2022 to match 2023
df_2022['valuation'] = df_2022['valuation'].astype('int64')
df_2023['valuation'] = df_2022['valuation'].astype('int64')


######################
## B. First Normal Form  (1NF) Transformation
######################
# splitting investors data
# Step 1: Split the investors column into lists
df_23_1nf = df_2023.copy()
df_23_1nf['select_investors'] = df_2023['select_investors'].str.split(',')

# Step 2: Explode the list into separate rows (1NF)
df_23_1nf = df_23_1nf.explode('select_investors')

# Step 3: Clean up whitespace
df_23_1nf['select_investors'] = df_23_1nf['select_investors'].str.strip()
df_23_1nf.head(10)

df_23_1nf['select_investors'].nunique() # 1380 unique investors
investor_counts_23 = df_23_1nf['select_investors'].value_counts()
investor_counts_23.head(10)

######################
## C. Feature Engineering
# ######################
df_23_1nf['date_joined_cleaned'] = pd.to_datetime(df_23_1nf['date_joined'], format='%d-%m-%Y').dt.strftime('%Y-%m-%d').astype('datetime64[ns]')
df_23_1nf['years_to_unicorn'] = 2023 - df_23_1nf['date_joined_cleaned'].dt.year

# validate: whether the years to unicorn column has negative values
df_23_1nf.query('years_to_unicorn < 0') # no negative values

######################
## D. Data Export
# ######################
df_23_1nf.info()
df_23_1nf.to_csv('Cleaned_Unicorn_Companies_2023.csv', index=False)
df_2022.to_csv('Cleaned_Unicorn_Companies_2022.csv', index=False)