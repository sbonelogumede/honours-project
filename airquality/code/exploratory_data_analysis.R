
# Attach the required packages.
require(package = "corrplot")

# Load the datasets.
load(file = "../objects/extracted_data_storage.RData")
load(file = "../objects/imputed_data_storage.RData")

# Extract the datasets.
X1 <- extracted_data[[1]]
X2 <- imputed_data[[1]]

# Declare variable names.
var_name <- c("DateTime", "NO2", "PM10", "SO2", "Speed")

# Declare variable names in a proper format.
x_name <- c("DateTime", 
            expression(NO[2] ~ "(" * mu * "g/m"^3 * ")"), 
            expression(PM[10] ~ "(" * mu * "g/m"^3 * ")"),
            expression(SO[2] ~ "(" * mu * "g/m"^3 * ")"), 
            expression("Wind Speed" ~ "(m/s)"))

# Calculate summary statistics.
summary_statistics <- function(X){
   rbind("Min." = sapply(X = X, FUN = min, na.rm = TRUE),
         "1st Qu." = sapply(X = X, FUN = quantile, 0.25, na.rm = TRUE),
         "Median" = sapply(X = X, FUN = median, na.rm = TRUE),
         "Mean" = sapply(X = X, FUN = mean, na.rm = TRUE),
         "Std." = sapply(X = X, FUN = sd, na.rm = TRUE),
         "3rd Qu." = sapply(X = X, FUN = quantile, 0.75, na.rm = TRUE),
         "Max." = sapply(X = X, FUN = max, na.rm = TRUE),
         "NA's" = sapply(X = X, FUN = function(column) sum(is.na(column))))
}
S_mat1 <- summary_statistics(X = X1)
S_mat2 <- summary_statistics(X = X2)

# Display summary statistics.
print(x = round(x = t(x = S_mat1[, 2:5]), digits = 3))
print(x = round(x = t(x = S_mat2[, 2:5]), digits = 3))

# Sketch scatter plots.
draw_plots <- function(X, name){
   for(i in 2:5){
      filename <- paste0("../images/", name, "_", tolower(x = var_name[i]), ".png")
      png(filename = filename, width = 6.5, height = 4, res = 300, units = "in")
      plot(x = X$DateTime, y = X[[var_name[i]]], 
           main = "", xlab = "Time", ylab = x_name[i],
           type = "l", col = "steelblue", cex = 0.5)
      dev.off()
   }
}

# Save scatter plots.
draw_plots(X = X1, name = "extracted")
draw_plots(X = X2, name = "imputed")

# Plot a correlation plot.
png(filename = "../images/corrplot.png", width = 6.5, height = 4, res = 300, units = "in")
cor_mat <- cor(X1[, 2:5], use = "na.or.complete", method = "pearson")
corrplot(corr = cor_mat, method = "number", type = "lower")
dev.off()
