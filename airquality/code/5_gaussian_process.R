# Attach the required packages.
require(package = "cmdstanr")
require(package = "parallel")

# Load the dataset.
file = load(file = "../objects/transformed_data_storage.RData")

# Find the total number of cores.
cores_number <- detectCores() - 2

# List the data for Stan.
stan_data <- list(X1 = X1, y1 = y1, n1 = n1, p = p, X2 = X2, n2 = n2)

# Compile the Stan model.
mod <- cmdstan_model(stan_file = "../code/squared_exponential.stan")

# Sample from a Gaussian process.
fit <- mod$sample(
	data = stan_data,
	chains = 4,
	parallel_chains = cores_number,
	seed = 2025,
	refresh = 0,
	show_messages = FALSE,
	show_exceptions = FALSE)

# fit is a CmdStanMCMC object.
y2_draws <- fit$draws("y2", format = "matrix")
y2_mean  <- colMeans(y2_draws)
y2_hat <- ts(y2_mean, start = start(y2), frequency = frequency(y2))

# Plot the forecasts.
p2 <- p2 + autolayer(object = y2_hat, series = "Gaussian process")

# Save the computed objects.
save(X, Y, X1, y1, n1, X2, y2, n2, p, h, p1, p2, rmse, mae, mape, 
	  file = "../objects/transformed_data_storage.RData")
