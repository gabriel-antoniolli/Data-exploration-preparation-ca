library(ggplot2)  
library(dplyr)    
library(tidyr)   
library(ggplot2) 

# a)

## Data types

### Categorical Variables:
 #### "Country/Region", "WHO Region"

### Discrete Variables:
  #### "Confirmed", "Deaths" ,"Recovered", "Active", "New cases", "New deaths", "New recovered"

### Continuous Variables:
  #### "Date"

### Importing data frame
covid_data <- read.csv('full_grouped.csv', header = TRUE)


### Checking missing values

#### Check for missing values
missing_values <- colSums(is.na(covid_data))

#### Print missing values
print(missing_values)

#### Plot missing values
barplot(missing_values, col = "red", main = "Missing Values")


#### Checking data frame structure
str(covid_data)



### visualization of categorical

#### Extract the counts for each country
country_counts <- table(covid_data$Country.Region)


#### Sort countries by count in descending order
sorted_countries <- names(country_counts[order(-country_counts)])

#### Creating bar plot for the first 10 countries, for demonstration of data type
barplot(country_counts[sorted_countries[1:10]], col = "skyblue",
        main = "First 10 countries", las= 2, cex.names = 0.7)


#### creating bar plot for visualization of WHO Region variable which is categorical
barplot(table(covid_data$WHO.Region),cex.names= 0.7, las=2, col = "lightgreen", main = "WHO Region Distribution")



### Discrete Variables
par(mfrow = c(3,3))  # Arrange plots in a 3x3 grid

#### Loop through each numeric variable
for (variable in c("Confirmed", "Deaths", "Recovered", "Active", "New.cases", "New.deaths", "New.recovered")) {
  hist(covid_data[[variable]], col = "orange", main = paste(variable, "Distribution"), xlab = variable, ylab = "Frequency")
}

### Visualization for continuous data

#### Convert the 'Date' column to a Date object
covid_data$Date <- as.Date(covid_data$Date)

#### Create a line plot for the number of cases over time
plot(covid_data$Date, covid_data$Confirmed, type = "l", col = "blue",
     xlab = "Date", ylab = "Confirmed Cases", main = "COVID-19 Confirmed Cases Over Time")


# b)






