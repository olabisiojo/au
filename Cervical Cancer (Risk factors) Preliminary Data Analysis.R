#R Script for the analysis of CCRS dataset (AU Research Methods Course SP25)_APril 2025
#..................

#Installing Packages to import the Excel File
install.packages("readxl")

library(readxl)

#Setting up working directory
setwd("C:/Users/BisiOJO/Documents/AU/Semesters/SP25/CCRF")

#Reading data and the specific Sheet
data <- read_excel("CCRF_SPSS.xlsx")

data <- read_excel("CCRF_SPSS.xlsx", sheet = "Cervical Cancer (Risk Factors)")

#Viewing data inside R
head(data)

View(data)

str(data)

summary(data)

describe(data)

#-------------------------------
#DATA PREPARATION and CLEANING
#---------------------------
# 3. Missing Value Analysis (original data [CCRF_SPSS.xlsx])
# Check for missing values
missing_values <- colSums(is.na(data))
missing_values_df <- data.frame(
  Variable = names(missing_values),
  Missing_Count = missing_values,
  Missing_Percent = round(missing_values / nrow(data) * 100, 2)
)
print(missing_values_df[order(-missing_values_df$Missing_Count),])

# Visualize missing data
gg_miss_var(data) +
  labs(title = "Missing Values by Variable")
#---------------------------


##other Data wrangling for analysis
library(tidyverse)
library(readxl)

# Load dataset
library(tidyverse)
library(readxl)

# Load dataset

library(tidyverse)
library(readxl)

# Load dataset
ccrf <- read_excel("CCRF_SPSS.xlsx") %>% 
  # First convert character columns containing "?" to NA, then convert to numeric
  mutate(across(where(is.character), ~na_if(., "?"))) %>% 
  type_convert(col_types = cols(.default = col_number())) %>% 
  
  # Remove columns with >50% missing values
  select(-c("STDs: Time since first diagnosis", "STDs: Time since last diagnosis")) %>% 
  
  # Remove rows with any remaining NA values
  drop_na() %>% 
  
  # Create age groups with explicit NA handling
  mutate(AgeGroup = case_when(
    between(Age, 18, 30) ~ "18-30",
    between(Age, 31, 45) ~ "31-45",
    Age > 45 ~ "46+",
    TRUE ~ NA_character_
  )) %>% 
  
  # Create binary variables
  mutate(across(c(Smokes, `Hormonal Contraceptives`, STDs), 
                ~if_else(. == 1, 1, 0, missing = 0), .names = "{col}_Binary")) %>% 
  
  # Create composite STD history variable
  mutate(STD_History = ifelse(rowSums(select(., matches("^STDs:"))) > 0, 1, 0)) %>% 
  
  # Handle outliers using IQR method
  mutate(across(where(is.numeric), 
                ~ifelse(. > quantile(., 0.75, na.rm = TRUE) + 1.5*IQR(., na.rm = TRUE) | 
                          . < quantile(., 0.25, na.rm = TRUE) - 1.5*IQR(., na.rm = TRUE), 
                        NA, .))) %>% 
  drop_na()

# Save cleaned data
write_csv(ccrf, "cleaned_cervical_data.csv")

#reload and view the cleaned data

ccrf_new <- read.csv("cleaned_cervical_data.csv")

head(ccrf_new)

view(ccrf_new)

glimpse(c)

summary(ccrf_new)

str(ccrf_new)

#----------------------------------------------------
# Load necessary libraries
library(tidyverse)    # For data manipulation and visualization
library(skimr)        # For comprehensive summary statistics
library(naniar)       # For missing data visualization
library(corrplot)     # For correlation visualization
library(ggplot2)      # For advanced plotting
library(gridExtra)    # For arranging multiple plots


# 1. Basic Data Exploration
# Structure of the dataset
str(ccrf_new)

# View first few rows
head(ccrf_new)

# Dimensions of the dataset
dim(ccrf_new)

# Column names
names(ccrf_new)

# 2. Summary Statistics
# Overall summary
summary(ccrf_new)

# Comprehensive summary using skimr
skim(ccrf_new)

#----------------------------------------------------------
# 3. Missing Value Analysis
# Check for missing values
missing_values <- colSums(is.na(ccrf_new))
missing_values_df <- data.frame(
  Variable = names(missing_values),
  Missing_Count = missing_values,
  Missing_Percent = round(missing_values / nrow(ccrf_new) * 100, 2)
)
print(missing_values_df[order(-missing_values_df$Missing_Count),])

# Visualize missing data
gg_miss_var(ccrf_new) +
  labs(title = "Missing Values by Variable")
#---------------------------------------------------------

# Prepare to generate Descriptive Statistics
# Load required packages
library(dplyr)
library(readr)
library(psych)     # For describe()
library(summarytools) # For dfSummary()


# Load the dataset
data <- read.csv("cleaned_cervical_data.csv")

# View the structure of the dataset
str(data)

#glimpse(data)

# General summary for all variables
dfSummary(data)

# 1. Descriptive Statistics (Numerical Variables)

# Load necessary libraries
library(readr)
library(dplyr)

# Read data
data <- read.csv("cleaned_cervical_data.csv")

# Identify numerical columns (integer or double)
num_vars <- sapply(data, is.numeric)
numerical_data <- data[, num_vars]

# Compute descriptive statistics for each numerical variable
num_summary <- data.frame(
  Variable = character(),
  Count = integer(),
  Mean = double(),
  SD = double(),
  Min = double(),
  Q1 = double(),
  Median = double(),
  Q3 = double(),
  Max = double(),
  stringsAsFactors = FALSE
)

for (col in names(numerical_data)) {
  x <- numerical_data[[col]]
  num_summary <- rbind(num_summary, data.frame(
    Variable = col,
    Count = sum(!is.na(x)),
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  ))
}

# Print the summary table
print("Descriptive Statistics for Numerical Variables:")
print(num_summary, row.names = FALSE)

# Export the summary table to CSV 
write.csv(num_summary, "numerical_summary.csv", row.names = FALSE)
# Alternatively, with readr:
# write_csv(num_summary, "numerical_summary.csv")



#Descriptive Statistics for (Categorical Variables)

# Load necessary libraries
#library(readr)
#library(dplyr)

# Read the data
#data <- read.csv("cleaned_cervical_data.csv")

# Identify categorical variables (character, factor, or numeric with few unique values)
is_categorical <- function(x) {
  is.character(x) | is.factor(x) | (is.numeric(x) & length(unique(x)) <= 5)
}
cat_vars <- sapply(data, is_categorical)
categorical_data <- data[, cat_vars]

# Create summary table: variable, level, count, percent
cat_summary <- data.frame(
  Variable = character(),
  Level = character(),
  Count = integer(),
  Percent = numeric(),
  stringsAsFactors = FALSE
)

n_total <- nrow(data)

for (col in names(categorical_data)) {
  tbl <- table(categorical_data[[col]], useNA = "ifany")
  levels <- names(tbl)
  counts <- as.integer(tbl)
  percents <- round(100 * counts / n_total, 2)
  cat_summary <- rbind(
    cat_summary,
    data.frame(
      Variable = col,
      Level = levels,
      Count = counts,
      Percent = percents,
      stringsAsFactors = FALSE
    )
  )
}

# Print the summary table
print("Descriptive Statistics for Categorical Variables:")
print(cat_summary, row.names = FALSE)

# Save the summary table to CSV
write.csv(cat_summary, "categorical_summary.csv", row.names = FALSE)
# Or, with readr:
# write_csv(cat_summary, "categorical_summary.csv")


#VISUALIZATIONS - DESCRIPTIVE STATISTICS (CATEGORICAL VARIABLES)
# Basic bar chart for categorical variables
library(ggplot2)

# For Hormonal.Contraceptives_Binary
ggplot(data, aes(x = Hormonal.Contraceptives_Binary)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Hormonal Contraceptives Usage", 
       x = "Hormonal Contraceptives", y = "Count") +
  theme_minimal()


# For AgeGroup
ggplot(data, aes(x = AgeGroup)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution by Age Group", 
       x = "Age Group", y = "Count") +
  theme_minimal()


##VISUALIZATIONS - DESCRIPTIVE STATISTICS (NUMERICAL VARIABLES)
#------------------------------

# Load necessary libraries
library(ggplot2)

# Read in the data
data <- read.csv("cleaned_cervical_data.csv")

# Histogram for Age
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency") +
  theme_minimal()

# Bar chart for Number of Sexual Partners
# Data from the categorical summary
partners <- c(1, 2, 3, 4)
counts <- c(86, 105, 73, 23)

# Create a bar plot
barplot(
  counts,
  names.arg = partners,
  col = "skyblue",
  border = "black",
  main = "Distribution of Number of Sexual Partners",
  xlab = "Number of Sexual Partners",
  ylab = "Count"
)

# Histogram for First Sexual encounter
# Load necessary libraries
library(ggplot2)

# Read the data
#data <- read.csv("cleaned_cervical_data.csv")

# Histogram for First.sexual.intercourse
ggplot(data, aes(x = First.sexual.intercourse)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Age at First Sexual Intercourse",
    x = "Age at First Sexual Intercourse",
    y = "Frequency"
  ) +
  theme_minimal()

# Boxplot for First.sexual.intercourse (optional)
ggplot(data, aes(y = First.sexual.intercourse)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Boxplot of Age at First Sexual Intercourse",
    y = "Age at First Sexual Intercourse"
  ) +
  theme_minimal()

# Number of pregnancies
# Load necessary library
library(ggplot2)

# Read the data
#data <- read.csv("cleaned_cervical_data.csv")

# Plot histogram for Num.of.pregnancies
ggplot(data, aes(x = Num.of.pregnancies)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Number of Pregnancies",
    x = "Number of Pregnancies",
    y = "Frequency"
  ) +
  theme_minimal()

#Hormonal Contraceptive use
# Load necessary libraries
library(ggplot2)
library(readr)

# Read the data
#data <- read_csv('cleaned_cervical_data.csv')

# Plot the distribution of Hormonal.Contraceptives
ggplot(data, aes(factor(Hormonal.Contraceptives))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Hormonal Contraceptives Use",
       x = "Hormonal Contraceptives (0 = No, 1 = Yes)",
       y = "Count") +
  theme_minimal()


#Homononal Contraceptive Use -years
# Load necessary library
library(ggplot2)

# Read the data
#data <- read.csv("cleaned_cervical_data.csv")

# Plot histogram for Hormonal.Contraceptives..years.
ggplot(data, aes(x = Hormonal.Contraceptives..years.)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Years of Hormonal Contraceptives Use",
    x = "Years of Hormonal Contraceptives Use",
    y = "Frequency"
  ) +
  theme_minimal()

)

#---------------------
# hormonal Contraceptive- Binary
# Read the data
#data <- read.csv("cleaned_cervical_data.csv")

# Create a bar plot for Hormonal.Contraceptives_Binary
barplot(
  table(data$Hormonal.Contraceptives_Binary),
  main = "Distribution of Hormonal.Contraceptives_Binary",
  xlab = "Hormonal.Contraceptives_Binary",
  ylab = "Count",
  col = c("skyblue", "orange"),
  names.arg = c("0", "1")
)

#ANALYSIS OF STRUCTURAL BARRIERS - cHI sQAURE AND LOGISTIC REGRESSION
#1. Cultural Stigma: 

# Load the data
data <- read.csv("cleaned_cervical_data.csv")

# Create a contingency table of Number.of.sexual.partners vs AgeGroup
contingency_table <- table(data$Number.of.sexual.partners, data$AgeGroup)

# Print the contingency table for inspection
print(contingency_table)

# Run the Chi-square test of independence
chi_result <- chisq.test(contingency_table)

# Print the test results
print(chi_result)


# 2. Healthcare engagement
# Load data
#data <- read.csv("cleaned_cervical_data.csv")

# Create contingency table
contraceptive_table <- table(data$Hormonal.Contraceptives_Binary, data$AgeGroup)

# Perform chi-square test
chi_result <- chisq.test(contraceptive_table)

# Print results
print("Contingency Table:")
print(contraceptive_table)
cat("\nChi-square Test Results:\n")
print(chi_result)

# Check expected counts for test validity
cat("\nExpected Counts:\n")
print(round(chi_result$expected, 1))

#3.Access Disparities: 
#Linear Regression
model <- glm(Hormonal.Contraceptives_Binary ~ ordered(AgeGroup), 
             family = binomial, data = data)
summary(model)


#----------------------------------

