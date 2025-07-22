# Clear the environment.
rm(list = ls())

# Attach the required packages.
required_packages <- c("corrplot", "dplyr", "ggplot2", "readxl")
for (package in required_packages) {
   if (!requireNamespace(package = package, quietly = TRUE)) {
      install.packages(pkgs = package, quiet = TRUE)
   }
   require(package = package, character.only = TRUE)
}

# Import the data.
years <- c(2019, 2020)
data_storage <- list(NA, NA)

for(i in 1:2){
   file_names <- c(
      paste(years[i], "NO2_CCT.xls", sep="_"),
      paste(years[i], "PM10_CCT.xls", sep="_"),
      paste(years[i], "SO2_CCT.xls", sep="_"),
      paste("Wind_direction_and_speed_", years[i], ".xlsx", sep=""),
      paste("Wind_direction_and_speed_", years[i], ".xlsx", sep="")
   )
   
   cell_ranges <- matrix(
      data = c(c("E6:E8742", "E6:E8742", "H6:H8742", "N6:N8742", "O6:O8742"),
               c("G6:G8742", "G6:G8742", "I6:I8742", "N6:N8742", "O6:O8742")), 
      nrow = 2, ncol = 5, byrow = TRUE)
   
   temp <- read_excel(
      path = paste("..", "data", file_names[1], sep = "/"),
      range = "A6:A8742",
      col_names = FALSE) %>%
      pull(1) %>%
      as.POSIXct(format = "%d/%m/%Y %H:%M")
   
   for (j in 1:5) {
      temp <- cbind(temp, read_excel(
         path = paste("..", "data", file_names[j], sep = "/"),
         range = cell_ranges[i, j],
         col_names = FALSE,
         col_types = "numeric",
         na = c("Down", "InVld", "NoData", "Zero", "<Samp")
      ))
   }
   colnames(temp) <- c("DateTime", "NO2", "PM10", "SO2", "Direction", "Speed")
   data_storage[[i]] <- temp
}

# Create train and test splits.
train <- data_storage[[1]]
test <- data_storage[[2]]

# Calculate the pairwise correlation between variables.
cor_matrix <- train %>%
   select(-DateTime) %>%
   cor(use = "na.or.complete", method = "pearson")
corrplot(corr = cor_matrix, method = "color", type = "lower")

# Scatter plot of the response variable.
ggplot(data = train, mapping = aes(x = DateTime, y = NO2))+
   geom_point(color = "steelblue", size = 2, pch = 18)+
   theme_minimal()

# Histogram of the response variable.
ggplot(data = train, mapping = aes(x = NO2))+
   geom_histogram(mapping = aes(y = ..density..), 
                  color = "lightskyblue", fill = "steelblue")+
   labs(x=expression(NO[2]))+
   theme_minimal()

# Histogram of the log of the response variable.
ggplot(data = train, mapping = aes(x = log(NO2)))+
   geom_histogram(mapping = aes(y = ..density..), 
                  color = "lightskyblue", fill = "steelblue")+
   labs(x=expression(log(NO[2])))+
   theme_minimal()