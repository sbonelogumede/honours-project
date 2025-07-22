# Load the necessary libraries
library(ggplot2)
library(readr)

gold_data <- read_csv("../data/XAU_ZAR Historical Data.csv")
head(gold_data)
# Convert the 'Date' column to Date format
gold_data$Date <- as.Date(gold_data$Date, format="%Y-%m-%d")
x_range <- range(gold_data_clean$Date, na.rm = TRUE)


gold_data_clean <- gold_data[!is.infinite(gold_data$Price), ]
x_range <- range(gold_data_clean$Date)
y_range <- range(gold_data_clean$Price)


plot(gold_data_clean$Price ~ gold_data_clean$Date, 
     xlim = x_range, 
     ylim = y_range)
