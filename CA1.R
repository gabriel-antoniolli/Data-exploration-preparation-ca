# Load the CSV file into a dataframe
file_path <- 'full_grouped.csv'
df <- read.csv(file_path)

# Display
head(df)


library(ggplot2)  
library(dplyr)    
library(tidyr)   
library(ggplot2) 

#Categorical Variables:
 # "Country/Region"
 # "WHO Region"

#Discrete Variables:
  "Confirmed"
  "Deaths"
  "Recovered"
  "Active"
  "New cases"
  "New deaths"
  "New recovered"

#Continuous Variables:
  "Date"

# importing data frame
covid_data <- read.csv('full_grouped.csv', header = TRUE)

# Data Transformation
#--- luiza transform data here

str(covid_data)








#visualisation of categorical
# Assuming your data frame is named 'covid_data'
# Extract the counts for each country
country_counts <- table(covid_data$Country.Region)


# Sort countries by count in descending order
sorted_countries <- names(country_counts[order(-country_counts)])
# Creating bar plot for the first 10 countries, for demonstration of data type
barplot(country_counts[sorted_countries[1:10]], col = "skyblue",
        main = "First 10 countries", las= 2, cex.names = 0.7)

# creating bar plot for visualization of WHO Region variable which is categorical
barplot(table(covid_data$WHO.Region),cex.names= 0.7, las=2, col = "lightgreen", main = "WHO Region Distribution")



# Convert the 'Date' column to a Date object
covid_data$Date <- as.Date(covid_data$Date)

# Plotting time series of Confirmed cases
plot(covid_data$Date, covid_data$Confirmed, type = "l", col = "blue",
     xlab = "Date", ylab = "Confirmed Cases",
     main = "COVID-19 Confirmed Cases Over Time")

