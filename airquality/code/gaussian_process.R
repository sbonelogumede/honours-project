# Attach the required packages.
require(package = "forecast")
require(package = "ggplot2")
require(package = "rstan")
require(package = "parallel")

# Load the dataset.
load(file = "../objects/window_data_storage.RData")

# Fit the model(s).
cores_number <- detectCores() - 2
stan_data <- list(X1 = X1, y1 = y1, n1 = n1, p = p, X2 = X2, n2 = n2)
stan_fit <- stan(file = "squared_exponential.stan", 
					  chains = 1,
					  cores = cores_number,
					  data = stan_data, 
					  seed = 2025,
					  verbose = FALSE)
stan_summary <- (summary(stan_fit))$summary

y2_hat <- stan_summary[grep("^y2", rownames(stan_summary)), "mean"] |>
	ts(start = start(x = y2), frequency = frequency(x = y2))

# Calculate the mean square error.
mean((y2 - y2_hat)^2)

# Plot the forecasts.
p2 <- p2 + autolayer(object = y2_hat, series = "Gaussian process")
p2

# Save these objects.
save(X1, y1, n1, p1, X2, y2, n2, p2, p, h, file = "../objects/window_data_storage.RData")
