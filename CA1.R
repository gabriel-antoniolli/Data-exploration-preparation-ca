install.packages("caret")

library(ggplot2)  
library(dplyr)    
library(tidyr)   
library(ggplot2)
library(caret)

# a)

## Definition of Data types

### Categorical Variables:
 #### "Country/Region", "WHO Region"

### Discrete Variables:
  #### "Confirmed", "Deaths" ,"Recovered", "Active", "New cases", "New deaths", "New recovered"

### Continuous Variables:
  #### "Date"

### Importing data frame and assiging it to a variable 
covid_data <- read.csv('full_grouped.csv', header = TRUE)

#### Checking dataset for missing values
missing_values <- colSums(is.na(covid_data))
barplot(missing_values, col = "red", main = "Missing Values")

#### Checking data frame structure
str(covid_data)

#### plot for categorical data
barplot(table(covid_data$WHO.Region),cex.names= 0.7, las=2, col = "lightgreen", main = "WHO Region Distribution - Categorical data plot")

### plot for numerical data
par(mfrow = c(3,3))  # Arrange plots in a 3x3 grid 
# loop through each numerical variable
for (variable in c("Confirmed", "Deaths", "Recovered", "Active", "New.cases", "New.deaths", "New.recovered")) {
  hist(covid_data[[variable]], col = "orange", main = paste(variable, "Distribution"), xlab = variable, ylab = "Frequency")
}

#### plot for continues data 
plot(as.Date(covid_data$Date), covid_data$Confirmed, type = "l", col = "blue",
     xlab = "Date", ylab = "Confirmed Cases", main = "COVID-19 Confirmed Cases Over Time - Continuos data")


# b) 

#Selecting numerical variables
numerical_variables <- c("Confirmed", "Deaths", "Recovered", "Active", "New.cases", "New.deaths", "New.recovered")

# Find the mean
for (variable in numerical_variables) {
  result.mean <- mean(covid_data[[variable]], na.rm = TRUE)
  print(paste("Mean of", variable, ":", result.mean))
}

# Find the median 
for (variable in numerical_variables) {
  result.median <- median(covid_data[[variable]], na.rm = TRUE)
  print(paste("Median of", variable, ":", result.median))
}

# Find the minimum 
for (variable in numerical_variables) {
  result.min <- min(covid_data[[variable]], na.rm = TRUE)
  print(paste("Minimum of", variable, ":", result.min))
}

# Find the maximum
for (variable in numerical_variables) {
  result.max <- max(covid_data[[variable]], na.rm = TRUE)
  print(paste("Maximum of", variable, ":", result.max))
}

# Find the standard deviation 
for (variable in numerical_variables) {
  result.sd <- sd(covid_data[[variable]], na.rm = TRUE)
  print(paste("Standard Deviation of", variable, ":", result.sd))
}

#c)

# min-max normalization 
min_max_normalized <- predict(preProcess(covid_data[, numerical_variables], method = "range"), covid_data[, numerical_variables])
print(min_max_normalized)

# z-score standardization
z_score_standardized <- scale(covid_data[, numerical_variables])
print(z_score_standardized)

# robust scalar
robust_scaled <- predict(preProcess(covid_data[, numerical_variables], method = c("center", "scale")), covid_data[, numerical_variables])
print(robust_scaled)

#d) 

covid_data$Date <- as.Date(covid_data$Date, format = "%Y-%m-%d")  # Adjust the format according to data format

# Line plot visualization
ggplot(covid_data, aes(x = Date, y = Deaths)) +
  geom_line(color = "blue") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(x = "Date", y = "Deaths", title = "Trend of Deaths Over Time")



# Scatter plot visualization
ggplot(covid_data, aes(x = Recovered, y = Deaths)) +
  geom_point(color = "blue") +
  labs(x = "Recovered Cases", y = "Deaths", title = "Deaths vs Recovered Cases")

######## improve the plot ############

# Heatmap visualization





