
# Attach the required packages.
require(package = "corrplot")
require(package = "dplyr")
require(package = "ggplot2")
require(package = "zoo")

# Load the dataset.
load(file = "../objects/extracted_data_storage.RData")

# Extract the training dataset.
X1 <- extracted_data[[1]]

# Declare variable names.
var_name <- c("DateTime", "NO2", "PM10", "SO2", "Speed")

# Declare variable names in a proper format.
x_name <- c("DateTime", 
            expression(NO[2] ~ "(" * mu * "g/m"^3 * ")"), 
            expression(PM[10] ~ "(" * mu * "g/m"^3 * ")"),
            expression(SO[2] ~ "(" * mu * "g/m"^3 * ")"), 
            expression("Wind Speed" ~ "(m/s)"))

# Calculate summary statistics.
S_mat <- rbind("Min." = sapply(X = X1, FUN = min, na.rm = TRUE),
               "1st Qu." = sapply(X = X1, FUN = quantile, 0.25, na.rm = TRUE),
               "Median" = sapply(X = X1, FUN = median, na.rm = TRUE),
               "Mean" = sapply(X = X1, FUN = mean, na.rm = TRUE),
               "Std." = sapply(X = X1, FUN = sd, na.rm = TRUE),
               "3rd Qu." = sapply(X = X1, FUN = quantile, 0.75, na.rm = TRUE),
               "Max." = sapply(X = X1, FUN = max, na.rm = TRUE),
               "NA's" = sapply(X = X1, FUN = function(column) sum(is.na(column))))

# Display summary statistics.
print(x = round(x = t(x = S_mat[, 2:5]), digits = 3))

# Plot scatter plots.
for(j in 2:5){
   filename <- paste0("../images/", tolower(x = var_name[j]), ".png")
   png(filename = filename, width = 6.5, height = 4, res = 300, units = "in")
   plot(x = X1$DateTime, y = X1[[var_name[j]]], 
        main = "", xlab = "Time", ylab = x_name[j],
        type = "l", col = "steelblue", cex = 0.5)
   dev.off()
}

# Plot a correlation plot.
png(filename = "../images/corrplot.png", width = 6.5, height = 4, res = 300, units = "in")
cor_mat <- cor(X1[, 2:5], use = "na.or.complete", method = "pearson")
corrplot(corr = cor_mat, method = "number", type = "lower")
dev.off()
