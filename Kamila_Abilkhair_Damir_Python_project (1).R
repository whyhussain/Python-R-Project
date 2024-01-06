# R Project for Data Science
# Made by Amangeldinova Kamila, Torekhanuly Abilkaiyr, and Dairov Damir.
# Overview of the chosen dataset
# The dataset is called "*Billionaires Statistics*".
# It contains statistics on the world's billionaires, 
# including information about their businesses, industries, 
# and personal details. It provides insights into the wealth distribution,
# business sectors, and demographics of billionaires worldwide.
# It is retrieved from Kaggle: 
# https://www.kaggle.com/datasets/nelgiriyewithana/billionaires-statistics-dataset/
# This dataset can be used to answer the main question about being 
# a billionaire:
# What are the factors that affect the total money of a billionaire?
# How the country, category/industry, gender and age impact 
# on having more finalWorth of a billionaire?
# This dataset can be used to answer the main question about being 
# a billionaire:
# What are the factors that affect the total money of a billionaire?
# How the country, category/industry, gender and age impact 
# on having more finalWorth of a billionaire?


# 1. Obtaining data
# Importing libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(readr)
library(RColorBrewer)
#loading the dataset
setwd('/Users/abdulvhair/Downloads')
df <- read_csv('Billionaires Statistics Dataset.csv')
head(df, 3)
#Description of every feature:
#1. rank: The ranking of the billionaire in terms of wealth.
#2. finalWorth: The final net worth of the billionaire in U.S. dollars.
#3. category: The category or industry in which the billionaire's 
#business operates.
#4. personName: The full name of the billionaire.
#5. age: The age of the billionaire.
#6. country: The country in which the billionaire resides.
#7. city: The city in which the billionaire resides.
#8. source: The source of the billionaire's wealth.
#9. industries: The industries associated with the billionaire's 
#business interests.
#10. countryOfCitizenship: The country of citizenship of the billionaire.
#11. organization: The name of the organization or company associated 
#with the billionaire.
#12. selfMade: Indicates whether the billionaire is 
#self-made (True/False).
#13. status: "D" represents self-made billionaires 
#(Founders/Entrepreneurs) and "U" indicates inherited or unearned wealth.
#14. gender: The gender of the billionaire.
#15. birthDate: The birthdate of the billionaire.
#16. lastName: The last name of the billionaire.
#17. firstName: The first name of the billionaire.
#18. title: The title or honorific of the billionaire.
#19. date: The date of data collection.
#20. state: The state in which the billionaire resides.
#21. residenceStateRegion: The region or state of residence of 
#the billionaire.
#22. birthYear: The birth year of the billionaire.
#23. birthMonth: The birth month of the billionaire.
#24. birthDay: The birth day of the billionaire.
#25. cpi_country: Consumer Price Index (CPI) for the billionaire's 
#country.
#26. cpi_change_country: CPI change for the billionaire's country.
#27. gdp_country: Gross Domestic Product (GDP) for the billionaire's 
#country.
#28. gross_tertiary_education_enrollment: Enrollment in 
#tertiary education in the billionaire's country.
#29. gross_primary_education_enrollment_country: Enrollment in 
#primary education in the billionaire's country.
#30. life_expectancy_country: Life expectancy in the billionaire's country.
#31. tax_revenue_country_country: Tax revenue in the billionaire's country.
#32. total_tax_rate_country: Total tax rate in the billionaire's country.
#33. population_country: Population of the billionaire's country.
#34. latitude_country: Latitude coordinate of the billionaire's country.
#35. longitude_country: Longitude coordinate of the billionaire's country.


# 2. Scrubbing / Cleaning our data

summary(df)

# checking if there's null data:
null_data <- colSums(is.na(df))

# showing null data as a percentage out of the total
null_data_percentage <- (null_data / nrow(df)) * 100

# showing null data and its percentage in a new dataframe
null_df <- data.frame(null_data = null_data, percentage = null_data_percentage)
print(null_df)

# remove columns without missing data to see which columns 
# we need don't need to analyse
null_df[null_df$null_data_percentage > 0]

#As we can see from the above table, the columns "organization", 
#"title", "state", and "residenceStateRegion" have a lot of 
#missing values (more than 70%). That is why we will drop 
#them because they will not give an important information for 
#the analysis.

columns_to_drop <- c('organization', 'title', 'state', 'residenceStateRegion')
df <- df[, !names(df) %in% columns_to_drop]

# We will also remove 'firstName' and 'lastName' because 
# we already have full 'personName'.
# We have missing values in 'birthDay', 'birthMonth', 
# and 'birthYear' columns, so we cannot fill already 
# given 'birthDate' column, this is the reason we need to drop them.
# We also drop these not needed for analysis columns:
#'life_expectancy_country' and 'cpi_change_country', 
# 'tax_revenue_country_country', 'cpi_country', 'city'
# We already have 'country', so we do not need longitude and latitude.
# 'indurstries' and 'category' are the same columns, so we drop 
# one of these 2.
# We do not need 'date' because it is considered only for 2023 year, 
# so it's onlye for one year.
# we do not know where the business of billionaire's located, 
# that is why we do not need 'gdp_country'.

columns_to_drop <- c('firstName', 'lastName',
                     'birthDay', 'birthMonth', 'birthYear',
                     'life_expectancy_country', 'cpi_change_country',
                     'tax_revenue_country_country', 'total_tax_rate_country',
                     'cpi_country', 'city',
                     'latitude_country', 'longitude_country',
                     'industries', 'date', 'gdp_country',
                     'status')
df <- df[, !names(df) %in% columns_to_drop]

names(df)

# let's make sure that df has dropped these columns 
# by checking its shape now:
dim(df)

#We need to consider the duplicate values too to remove 
#unnecessary data.

#Remove duplicates:
unique(df)
cat(sprintf("There are %d duplicate values in the data\n", sum(duplicated(df))))

#Missing values in 'country' column were filled with countryOfCitizenship.

df$country[is.na(df$country)] <- df$countryOfCitizenship

# Displaying summary statistics
summary(df)

#let's see what is happening in missing value df again:

# checking if there's null data:
null_data <- colSums(is.na(df))

# showing null data as a percentage out of the total
null_data_percentage <- (null_data / nrow(df)) * 100

# showing null data and its percentage in a new dataframe
null_df <- data.frame(null_data = null_data, percentage = null_data_percentage)
print(null_df)


#Feature Engineering for Cleaning the data further


#We have missing values in the 'age' column, to fill them we can 
#count based on the birthDate. Before doing it, we need to 
#convert the birthDate column to datetime.

# converting 'birthDate' to datetime
df$birthDate <- as.Date(df$birthDate, format="%Y-%m-%d")

# calculating age from 'birthDate'
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# filling missing values in 'age' with the calculated age from 'birthDate'
df$age[is.na(df$age)] <- current_year - as.numeric(format(df$birthDate[is.na(df$age)], "%Y"))

#After using it to fill 'age', we do not need it, so we drop it.

df <- df[, !(names(df) %in% c("birthDate"))]

#We will check now if there are missing values in the rows:

# counting the number of missing values in each row
missing_rows <- rowSums(is.na(df))

# counting the total number of rows with at least one missing value
total_missing_rows <- sum(missing_rows > 0)

cat(sprintf('Total rows with missing values: %d\n', total_missing_rows))


#Null count analysis on the last dataset

cat("Number of missing values in each column:\n")
print(colSums(is.na(df)))

if (!require("VIM")) install.packages("VIM")
library(VIM)

# Create a missingness matrix
p <- aggr(df, prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE)


# 3. EDA (Explorative Data Analysis)

#Now let's see what columns do we have to analyse:
dim(df)
names(df)

# data type analysis
barplot(table(sapply(df, class)), horiz=TRUE, main="Count of Each Data Type",
        xlab="Count", ylab="Data Types", col="lightblue", border="black")
str(df)
summary(df)

# Set up the plotting area with the desired size
par(mfrow = c(1, 1), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
# Adjust the margin and outer margin for better layout

# Create the histogram

hist(df$rank, main = "Histogram", xlab = "rank", ylab = "", col = "lightblue", border = "black", xlim = c(min(df$rank), max(df$rank)))


hist(df$finalWorth, main = "Histogram", xlab = "finalWorth", ylab = "", col = "lightblue", border = "black", xlim = c(min(df$finalWorth), max(df$finalWorth)))

histAge <- df$age[is.finite(df$age)]

par(mfrow = c(1, 1), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

hist(histAge, main = "Histogram", xlab = "Age", ylab = "", col = "lightblue", border = "black")

histGross_tertiary_education_enrollment <- df$gross_tertiary_education_enrollment[is.finite(df$gross_tertiary_education_enrollment)]

hist(histGross_tertiary_education_enrollment, main = "Histogram", xlab = "Gross_tertiary_education_enrollment", ylab = "", col = "lightblue", border = "black")

histPopulation_country <- df$population_country[is.finite(df$population_country)]

hist(histPopulation_country, main = "Histogram", xlab = "Population_country", ylab = "", col = "lightblue", border = "black")



df_1 <- df[, sapply(df, is.numeric)]


# Remove rows with missing values
df_1 <- na.omit(df_1)

# Remove rows with any NaN values
df_1 <- df_1[apply(df_1, 1, function(row) all(is.finite(row))), ]

# Create a heatmap
heatmap(cor(df_1), annot = TRUE, col = colorRampPalette(c("green", "yellow", "red"))(100), main = "Correlation Heatmap", cexRow = 1.5, cexCol = 1.5)

# Calculate the percentage of males and females
gender_counts <- table(df$gender) / length(df$gender) * 100

# Print the result
cat("Percentage % of Males and Females:\n")
print(round(gender_counts, 2))



# Create a scatter plot
fig1 <- plot_ly(data = df, x = ~rank, y = ~finalWorth, color = ~gender, type = "scatter", mode = "markers") %>%
  layout(title = "Billionaires' rank vs. finalWorth by gender",
         xaxis = list(type = "log", title = "rank (log scale)"),
         yaxis = list(type = "log", title = "finalWorth (log scale)"))

# Show the plot
fig1


# Calculate the top 10 countries with the most billionaires
top_countries <- head(names(sort(table(df$country), decreasing = TRUE)), 10)

# Filter the data for the top 10 countries
df_top_countries <- df[df$country %in% top_countries, ]

# Group by country and summarize the count
df_grouped <- df_top_countries %>%
  group_by(country) %>%
  summarize(count = n())

# Explicitly set the order of countries based on the count in descending order
country_order <- arrange(df_grouped, desc(count))$country

# Create a grouped bar plot with explicitly set country order
fig2 <- plot_ly(data = df_grouped, x = ~country, y = ~count,
                type = "bar") %>%
  layout(title = "Top 10 countries with Most Billionaires",
         xaxis = list(title = "Country", categoryorder = "array", categoryarray = country_order),
         yaxis = list(title = "Number of Billionaires"))

# Show the plot
fig2


age_column <- df$age

age_column



# Create a box plot
fig3 <- plot_ly(data = df, x = ~gender, y = ~age, type = "box",
                boxpoints = "all", jitter = 0.3, pointpos = -1.8,
                marker = list(color = c('#f779b8', '#7eeef2'))) %>%
  layout(title = "Age Distribution of Billionaires by Gender",
         xaxis = list(title = "Gender"),
         yaxis = list(title = "Age"))

# Show the plot
fig3




# Calculate the percentage of finalWorth for each category
df_percent <- transform(df, percent_finalWorth = finalWorth / sum(df$finalWorth) * 100)

# Create a treemap with a single level
fig4 <- plot_ly(data = df_percent, labels = ~category,
               parents = ~rep("", nrow(df_percent)),
               values = ~percent_finalWorth, type = "treemap",
               marker = list(colors = brewer.pal(nlevels(df$category), "Set1"))) %>%
  layout(margin = list(t = 50, l = 0, r = 0, b = 0),
         title = 'Wealth Distribution by Categories')


# Show the plot
fig4


# Get the top 10 categories with the most billionaires and convert to data frame
categories <- head(sort(table(df$category), decreasing = TRUE), 10) %>%
  as.data.frame() %>%
  arrange(Freq)

# Create a horizontal bar chart
fig5 <- plot_ly(data = categories,
                x = ~Freq, y = ~Var1, type = "bar",
                orientation = "h",
                marker = list(color = brewer.pal(8, "Set2"))) %>%
  layout(title = "Top 10 Categories with the most billionaires",
         xaxis = list(title = "Count"),
         yaxis = list(title = "Category"))

# Show the plot
fig5
