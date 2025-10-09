# Attach the required packages.
require(package = "GGally")
require(package = "ggplot2")
require(package = "ggcorrplot")

# Load the datasets.
load(file = "../objects/extracted_data_storage.RData")
load(file = "../objects/subset_data_storage.RData")

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
         "SD" = sapply(X = X, FUN = sd, na.rm = TRUE),
         "3rd Qu." = sapply(X = X, FUN = quantile, 0.75, na.rm = TRUE),
         "Max." = sapply(X = X, FUN = max, na.rm = TRUE),
         "#Tot." = sapply(X = X, FUN = function(column) sum(!is.na(column))),
         "#NA." = sapply(X = X, FUN = function(column) sum(is.na(column))))
}
S_mat1 <- summary_statistics(X = F_mat)
S_mat2 <- summary_statistics(X = X_mat)

# Display summary statistics.
print(x = round(x = t(x = S_mat1[, 2:5]), digits = 3))
print(x = round(x = t(x = S_mat2[, 2:5]), digits = 3))

# Plot scatter plots.
scatter_plot <- function(X, name){
   for(i in 2:5){
      p <- ggplot(data = data.frame(X = X$DateTime, Y = X[[var_name[i]]])) +
         geom_line(mapping = aes(x = X, y = Y, color = "observed")) +
         labs(title = NULL, x = "Time", y = x_name[i], color = NULL) +
         scale_color_manual(values = c("observed" = "black")) +
         theme_minimal()
      filename <- paste0("../images/", name, "_data_", tolower(x = var_name[i]), ".png")
      ggsave(filename = filename, plot = p, dpi = 600, width = 10, height = 6, units = "in")
   }
}

# Plot pairs plot.
pairs_plot <- function(X, name){
   p <- ggpairs(data = X[, 2:5]) + theme_minimal()
   filename <- paste0("../images/", name, "_data_pairsplot.png")
   ggsave(filename = filename, plot = p, dpi = 600, width = 10, height = 6, units = "in")
}

# Save scatter plots.
scatter_plot(X = F_mat, name = "extracted")
scatter_plot(X = X_mat, name = "subset")

# Save pairs plots.
pairs_plot(X = F_mat, name = "extracted")
pairs_plot(X = X_mat, name = "subset")
