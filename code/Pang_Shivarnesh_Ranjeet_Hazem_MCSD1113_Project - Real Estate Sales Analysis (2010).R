# Install necessary libraries
install.packages("ggplot2")
install.packages("scales")
library(scales)
library(ggplot2)

# Load the dataset
url <- "https://raw.githubusercontent.com/HazemFenneer/MCSD1113---Statistics-project/main/dataset/Real_Estate_Sales_2010.csv"
data <- read.csv(url)
head(data)

# Explore the dataset
cat("Exploratory Data Analysis:\n")

#Drop missing values from the rows
clean_data <- subset(data, !is.na(Property.Type) & !is.na(Residential.Type))
clean_data <- data[apply(data != "Nan", 1, all), ]


# Check the structure and size of the dataset
cat("Dataset information:\n")
str(clean_data)
cat("Number of rows:", nrow(clean_data), "\n")
cat("Number of columns:", ncol(clean_data), "\n")

# Descriptive statistics
cat("Descriptive statistics of the dataset:\n")
summary(clean_data)

# Visualizations - Part 1
cat("Visualizations - Part 1:\n")

# Bar chart - Assessed Value
barplot(clean_data$Assessed.Value, main="Assessed Value", xlab="Serial Number", ylab="Assessed Value")

# Pie chart - Property Type
install.packages("scales")
library(scales)

# Assuming df_clean$Property.Type contains the property types
property_table <- table(clean_data$Property.Type)

# Plot the pie chart with percentages
pie(property_table, main="Property Type", labels = paste(names(property_table), ": ", round(prop.table(property_table)*100), "%", sep = ""))

# Stem and leaf plot - Sale Amount
stem(clean_data$Sale.Amount)

# Histogram - Sales Ratio
hist(data$Sales.Ratio, main="Sales Ratio", xlab="Sales Ratio", ylab="Frequency")

# Box plot - Assessed Value vs Sale Amount
boxplot(data$Assessed.Value, data$Sale.Amount, main="Assessed Value vs Sale Amount", names=c("Assessed Value", "Sale Amount"))

# Inference Analysis - Part 2
cat("Inferential Analysis - Part 2:\n")

#Inferential Statistics

# Hypothesis testing - 1-sample t-test
# Perform 1-sample t-test
# Null hypothesis: The mean assessed value is $100,000
# Alternative hypothesis: The mean assessed value is not $100,000

# Extracting the Assessed Value column
assessed_value <- clean_data$Assessed.Value

# Define the hypothesized value
hypothesized_value <- 100000

# Perform the t-test
t_test_result <- t.test(assessed_value, mu = hypothesized_value)

# Print the result
print(t_test_result)

# Perform goodness-of-fit test
# Null hypothesis: The distribution of Property Types matches a hypothesized distribution
# Alternative hypothesis: The distribution of Property Types does not match the hypothesized distribution

chisq_fit_test <- chisq.test(table(clean_data$Property.Type))
cat("Chi-squared goodness-of-fit test result:\n")
print(chisq_fit_test)


# Perform Chi-Square test of independence
# Null hypothesis: There is no association between Property Type and Residential Type
# Alternative hypothesis: There is an association between Property Type and Residential Type

# Extract the Property Type and Residential Type columns
property_type <- clean_data$Property.Type
residential_type <- clean_data$Residential.Type

# Create a contingency table
contingency_table <- table(property_type, residential_type)

# Perform the Chi-Square test
chi_sq_test_result <- chisq.test(contingency_table)

# Print the result
print(chi_sq_test_result)


# Perform correlation analysis
# Null hypothesis: There is no correlation between Assessed Value and Sale Amount
# Alternative hypothesis: There is a correlation between Assessed Value and Sale Amount

# Extract the Assessed Value and Sale Amount columns
assessed_value <- clean_data$Assessed.Value
sale_amount <- clean_data$Sale.Amount

# Compute the correlation coefficient
correlation_coefficient <- cor(assessed_value, sale_amount)

# Print the correlation coefficient
print(correlation_coefficient)

# Perform hypothesis test for correlation
cor_test_result <- cor.test(assessed_value, sale_amount)

# Print the result
print(cor_test_result)

# Create a scatter plot
ggplot(clean_data, aes(x = Assessed.Value, y = Sale.Amount)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation between Assessed Value and Sale Amount",
       x = "Assessed Value",
       y = "Sale Amount") +
  theme_minimal()

# Perform linear regression analysis
# Null hypothesis: There is no linear relationship between Assessed Value and Sale Amount
# Alternative hypothesis: There is a linear relationship between Assessed Value and Sale Amount

# Fit a linear regression model
regression_model <- lm(Sale.Amount ~ Assessed.Value, data = clean_data)

# Print the summary of the regression model
summary(regression_model)

# Create a scatter plot
ggplot(data, aes(x=Assessed.Value, y=Sale.Amount)) +
  geom_point() +
  labs(x="Assessed Value", y="Sale Amount", title="Scatter plot of Assessed Value vs Sale Amount") +
  theme_minimal()
# Perform ANOVA
# Null hypothesis: There are no significant differences in Sale Amount among different Property Types
# Alternative hypothesis: There are significant differences in Sale Amount among different Property Types

# Fit ANOVA model
anova_model <- aov(Sale.Amount ~ Property.Type, data = clean_data)

# Print the summary of the ANOVA model
summary(anova_model)

# Visualize ANOVA results with boxplot
ggplot(clean_data, aes(x = Property.Type, y = Sale.Amount)) +
  geom_boxplot() +
  labs(x = "Property Type", y = "Sale Amount", title = "ANOVA: Sale Amount by Property Type")

