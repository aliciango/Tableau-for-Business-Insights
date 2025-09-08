# Load required libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
install.packages("gridExtra")


# Load data
df <- read.csv("marketing_data.csv")

#######################
# Check structure and summary
str(df)
summary(df) 
dim(df) # 2240 x 28

# Change data structure for ID, Year_Birth
df$ID <- as.character(df$ID)

  
# Change data structure for Education & Marital_Status
table(df$Education)
df$Education <- as.factor(df$Education)
table(df$Marital_Status)
df$Marital_Status <- as.factor(df$Marital_Status)
table(df$Country)
df$Country <- as.factor(df$Country)

# Change data str for Dt_Customer
df$Dt_Customer <- as.Date(df$Dt_Customer)

# Check again the structure
str(df)

#######################
# 1. INCOME - Handle extreme outliers
# Visualize income distribution
p1 <- ggplot(df, aes(x = Income)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  ggtitle("Income Distribution") +
  theme_minimal()
p2 <- ggplot(df, aes(y = Income)) +
  geom_boxplot(fill = "lightgreen") +
  ggtitle("Income Boxplot") +
  theme_minimal()
grid.arrange(p1, p2, ncol = 2)

# Identify extreme income outliers
# function check_distribution
check_distribution <- function(data, var) {
  x <- data[[var]]
  
  if (!is.numeric(x)) {
    stop("The selected variable is not numeric.")
  }
  summary(x)
}

# function calc_whisker
calc_whiskers <- function(x) {
  x <- x[!is.na(x)]   # remove NAs
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR_val <- IQR(x)
  
  lower_whisker <- Q1 - 1.5 * IQR_val
  upper_whisker <- Q3 + 1.5 * IQR_val
  
  return(list(
    lower_whisker = lower_whisker,
    upper_whisker = upper_whisker
  ))
}
calc_whiskers(df$Income)

# The percentage of records that are higher or lower than upper bound => remove outliers 
df_clean <- df %>%
  filter(Income >= -14525.5 | Income <= 118350.5)
# count(df_clean) / count(df) #0.0035, not a significant number



#######################
# 2. YEAR_BIRTH - Handle unrealistic birth years
# Visualize 
p3 <- ggplot(df_clean, aes(x = Year_Birth)) +
  geom_histogram(bins = 30, fill = "lightcoral", color = "black") +
  ggtitle("Year of Birth Distribution")
p3
check_distribution(df, "Year_Birth")
calc_whiskers(df$Year_Birth)
# Filter outliers
df_clean <- df_clean %>%
  filter(Year_Birth >= 1932 | Year_Birth <= 2004)



#######################
# 3. SPENDING COLUMNS - Handle outliers
spending_columns <- c("MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts", 
                      "MntSweetProducts", "MntGoldProds")
# Visualizing
for (col in spending_columns) {
  p <- ggplot(df_clean, aes_string(x = col)) +
    geom_bar(fill = "lightblue") +
    ggtitle(paste(col, "Distribution")) +
    theme_minimal()
  print(p)
}
# a lot of the spending columns are right-skewed
# should rescale them later before using them in the model

# Apply to all spending columns
for (col in spending_columns) {
  df_clean <- handle_spending_outliers(df_clean, col)
}


#######################
# 4. BINARY COLUMNS - Check distribution (already fine, just visualize)
binary_columns <- c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", 
                    "AcceptedCmp4", "AcceptedCmp5", "Response", "Complain")

# Visualize 
for (col in binary_columns) {
  p <- ggplot(df_clean, aes_string(x = col)) +
    geom_bar(fill = "lightblue") +
    ggtitle(paste(col, "Distribution")) +
    theme_minimal()
  print(p)
}


#######################
# Save cleaned dataset
write.csv(df_clean, "marketing_data_cleaned.csv", row.names = FALSE)
