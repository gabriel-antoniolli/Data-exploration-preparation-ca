library(ggplot2)  
library(dplyr)    
library(tidyr)   
library(ggplot2) 

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






