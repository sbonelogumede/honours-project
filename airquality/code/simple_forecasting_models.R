# Attach the required packages.
require(package = "forecast")
require(package = "ggplot2")

# Load the dataset.
load(file = "../objects/window_data_storage.RData")

# Fit the model(s).
mean_fit <- meanf(y = y1, h = h)
naive_fit <- naive(y = y1, h = h)
snaive_fit <- snaive(y = y1, h = h)
drift_fit <- rwf(y = y1, h = h, drift = TRUE)

# Calculate the mean square error.
mean((mean_fit$mean - y2)^2)
mean((naive_fit$mean - y2)^2)
mean((snaive_fit$mean - y2)^2)
mean((drift_fit$mean - y2)^2)

# Plot the forecasts.
p2 <- p2 + autolayer(object = mean_fit, series = "Mean", PI = FALSE)
p2 <- p2 + autolayer(object = naive_fit, series = "Naive", PI = FALSE)
p2 <- p2 + autolayer(object = snaive_fit, series = "Snaive", PI = FALSE)
p2 <- p2 + autolayer(object = drift_fit, series = "Drift", PI = FALSE)
p2

# Save these objects.
save(X1, y1, n1, p1, X2, y2, n2, p2, p, h, file = "../objects/window_data_storage.RData")
