# Clear the environment.
rm(list = ls())

# Attach the required packages.
required_packages <- c("corrplot", "dplyr", "ggplot2", "readxl", "zoo")
for (package in required_packages) {
   if (!requireNamespace(package = package, quietly = TRUE)) {
      install.packages(pkgs = package, quiet = TRUE)
   }
   require(package = package, character.only = TRUE)
}

# Declare important variables.
years <- c(2019, 2020)
data_storage <- list(NA, NA)
var_names <- c("Date", "NO2", "PM10", "SO2", "Direction", "Speed")
x_names <- matrix(data = c(
   c("Date", expression(NO[2]), expression(PM[10]), expression(SO[2]), 
     "Direction", "Speed"),
   c("Date", expression(log(NO[2])), expression(log(PM[10])), expression(log(SO[2])), 
     "log(Direction)", "log(Speed)")), 
   nrow = 2, ncol = 6, byrow = TRUE) 
color_names <- c("lightseagreen", "steelblue", "palegreen", "plum3", "lightsalmon", "khaki2")

# Import the data.
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
   colnames(outer_temp) <- var_names
   data_storage[[i]] <- outer_temp
   outer_temp <- NA
}

# Create train and test splits.
train <- data_storage[[1]]
test <- data_storage[[2]]

# Generate histogram plots.
for(j in 1:2){
   for(i in 2:6){
      if(j==1){
         x <- train[[var_names[i]]]
         filename = paste0("../images/", tolower(x = var_names[i]), "_hist_2019.png")
         
      } else {
         x <- log(train[[var_names[i]]])
         filename = paste0("../images/log_", tolower(x = var_names[i]), "_hist_2019.png")
      }
      
      png(filename = filename, width = 8, height = 6, res = 600, units = "in")
      
      hist(x = x, col = color_names[i], main = "", xlab = x_names[j, i], freq = F)
      abline(v = mean(x = x, na.rm = TRUE), col = "red", lwd = 2)
      abline(v = median(x = x, na.rm = TRUE), col = "black", lwd = 2)
      legend(x = "topright", legend = c("mean" , "median"), col = c("red", "black"), lwd = 2)
      
      dev.off()
   }
}

# Calculate the pairwise correlation between variables.
cor_matrix <- train %>%
   select(-Date) %>%
   cor(use = "na.or.complete", method = "pearson")

png(filename = "../images/corrplot_2019.png", 
    width = 8, 
    height = 6, 
    res = 600, 
    units = "in")

corrplot(corr = cor_matrix, method = "number", type = "lower")

dev.off()

no2_ma <- rollmean(x = train$NO2, k = 720, na.rm = TRUE, fill = NA)

# Scatter plot of the response variable.
p1 <- ggplot(data = train, mapping = aes(x = Date, y = NO2))+
   geom_point(color = color_names[2], size = 2, pch = 18)+
   geom_line(mapping = aes(y = no2_ma), color = "hotpink", linewidth = 2)+
   theme_minimal()+
   theme(panel.grid.minor = element_blank())

# Scatter plot of the log of the response variable.
p2 <- ggplot(data = train, mapping = aes(x = Date, y = log(NO2)))+
   geom_point(color = color_names[1], size = 2, pch = 18)+
   geom_line(mapping = aes(y = log(no2_ma)), color = "hotpink", linewidth = 2)+
   theme_minimal()+
   theme(panel.grid.minor = element_blank())

ggsave(filename = "../images/no2_scatter_2019.png", plot = p1, width = 8, height = 6, dpi = 600, units = "in")
ggsave(filename = "../images/log_no2_scatter_2019.png", plot = p2, width = 8, height = 6, dpi = 600, units = "in")

no2_mean <- mean(x = train$NO2, na.rm = TRUE)
no2_sd <- sd(x = train$NO2, na.rm = TRUE)

# qqnorm(y = log(train$NO2), 
#        main = expression("Q-Q plot for" ~~ log(NO[2])), 
#        col = "lightseagreen", 
#        lwd = 2)
