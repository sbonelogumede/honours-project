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
   
   outer_temp <- read_excel(
      path = paste("..", "data", file_names[1], sep = "/"),
      range = "A6:A8742",
      col_names = FALSE) %>%
      pull(1) %>%
      as.POSIXct(format = "%d/%m/%Y %H:%M")
   
   for (j in 1:5) {
      # Read the column of interest from the data.
      inner_temp <- read_excel(
         path = paste("..", "data", file_names[j], sep = "/"),
         range = cell_ranges[i, j],
         col_names = FALSE,
         col_types = "text"
      )
      
      # Handle the case where x=0 gracefully.
      inner_temp <- inner_temp %>%
         mutate(across(everything(), ~ case_when(
            .x %in% c("Down", "InVld", "NoData", "<Samp") ~ NA_real_,
            .x == "Zero" ~ 0,
            TRUE ~ as.numeric(.x)
         )))
      
      outer_temp <- cbind(outer_temp, inner_temp)
   }
   colnames(outer_temp) <- c("Date", "NO2", "PM10", "SO2", "Direction", "Speed")
   data_storage[[i]] <- outer_temp
   outer_temp <- NA
}

# Create train and test splits.
train <- data_storage[[1]]
test <- data_storage[[2]]

# Calculate the pairwise correlation between variables.
cor_matrix <- train %>%
   select(-Date) %>%
   cor(use = "na.or.complete", method = "pearson")
p1 <- corrplot(corr = cor_matrix, method = "number", type = "lower")

# Scatter plot of the response variable.
p2 <- ggplot(data = train, mapping = aes(x = Date, y = NO2))+
   geom_point(color = "lightcoral", size = 2, pch = 18)+
   theme_minimal()

# Scatter plot of the log of the response variable.
p3 <- ggplot(data = train, mapping = aes(x = Date, y = log(NO2)))+
   geom_point(color = "lightseagreen", size = 2, pch = 18)+
   theme_minimal()

# Histogram of the response variable.
p4 <- ggplot(data = train, mapping = aes(x = NO2))+
   geom_histogram(mapping = aes(y = ..density..), 
                  color = "black", fill = "lightcoral")+
   labs(x=expression(NO[2]))+
   theme_minimal()

# Histogram of the log of the response variable.
p5 <- ggplot(data = train, mapping = aes(x = log(NO2)))+
   geom_histogram(mapping = aes(y = ..density..), 
                  color = "black", fill = "lightseagreen")+
   labs(x=expression(log(NO[2])))+
   theme_minimal()

# Display the plots.
p1
p2
p3
p4
p5

# Save the plots.
png(filename="../images/corrplot_2019.png", width=8, height = 6, res = 600, units = "in")
corrplot(corr = cor_matrix, method = "number", type = "lower")
dev.off()
ggsave(filename = "../images/no2_scatter_2019.png", plot = p2, width=8, height = 6, dpi = 600, units = "in")
ggsave(filename = "../images/log_no2_scatter_2019.png", plot = p3, width=8, height = 6, dpi = 600, units = "in")
ggsave(filename = "../images/no2_hist_2019.png", plot = p4, width=8, height = 6, dpi = 600, units = "in")
ggsave(filename = "../images/log_no2_hist_2019.png", plot = p5, width=8, height = 6, dpi = 600, units = "in")