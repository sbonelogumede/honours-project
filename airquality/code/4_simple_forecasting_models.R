# Attach the required packages.
require(package = "forecast")
require(package = "ggplot2")

# Load the dataset.
load(file = "../objects/transformed_data_storage.RData")

# Fit the models.
mean_fit <- meanf(y = y1, h = h[1] * 24)
naive_fit <- naive(y = y1, h = h[1] * 24)
snaive_fit <- snaive(y = y1, h = h[1] * 24)
drift_fit <- rwf(y = y1, h = h[1] * 24, drift = TRUE)
arima_obj <- arima(x = y1, order = c(1, 0, 0))
arima_fit <- forecast(object = arima_obj, h = h[1] * 24)

# Plot the forecasts.
p2 <- p2 + autolayer(object = mean_fit, series = "Mean", PI = FALSE)
p2 <- p2 + autolayer(object = naive_fit, series = "Naive", PI = FALSE)
p2 <- p2 + autolayer(object = snaive_fit, series = "Snaive", PI = FALSE)
p2 <- p2 + autolayer(object = drift_fit, series = "Drift", PI = FALSE)
p2 <- p2 + autolayer(object = arima_fit, series = "AR(1)", PI = FALSE)

# Save the computed objects.
save(X, Y, X1, y1, n1, X2, y2, n2, p, h, p1, p2, rmse, mae, mape, 
	  file = "../objects/transformed_data_storage.RData")
