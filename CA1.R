# Load the CSV file into a dataframe
file_path <- 'full_grouped.csv'
df <- read.csv(file_path)

# Display
head(df)


library(ggplot2)  
library(dplyr)    
library(tidyr)   
library(ggplot2) 


categorical <- c('Country/Region', 'WHO Region')
discrete_variable <- c('ConfDeaths', 'Recovered', 'Active', 'New cases', 'New deaths', 'New recovered')
continuous_variable <- c('Date')