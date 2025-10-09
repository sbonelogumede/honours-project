# Attach the required packages.
require(package = "cmdstanr")
require(package = "forecast")
require(package = "ggplot2")
require(package = "parallel")

# Load the dataset.
load(file = "../objects/transformed_data_storage.RData")

# Number of cross-validation iterations.
n <- 1
# Cross-validation results storage.
R_list <- list()
# Integrated cross-validation results storage.
R_mat <- matrix(data = NA, nrow = 3, ncol = 6, dimnames = list(c("IRMSE", "IMAE", "IMPE"), 
					 c("Mean", "Naive", "Seasonal naive", "Drift", "AR(1)", "Gaussian process")))

for(i in 1:n){
	# Fit the models.
	mean_fit <- meanf(y = y1, h = h[1] * 24)
	naive_fit <- naive(y = y1, h = h[1] * 24)
	snaive_fit <- snaive(y = y1, h = h[1] * 24)
	drift_fit <- rwf(y = y1, h = h[1] * 24, drift = TRUE)
	arima_obj <- arima(x = y1, order = c(1, 0, 0))
	arima_fit <- forecast(object = arima_obj, h = h[1] * 24)
	
	stan_data <- list(X1 = X1, y1 = y1, n1 = n1, p = p, X2 = X2, n2 = n2)
	
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
	y2_hat <- as.numeric(y2_mean)
	
	# Calculate the mean square error.
	R_mat[1, 1] <- rmse(y = y2, y_hat = mean_fit)
	R_mat[1, 2] <- rmse(y = y2, y_hat = naive_fit$mean)
	R_mat[1, 3] <- rmse(y = y2, y_hat = snaive_fit$mean)
	R_mat[1, 4] <- rmse(y = y2, y_hat = drift_fit$mean)
	R_mat[1, 5] <- rmse(y = y2, y_hat = arima_fit$mean)
	R_mat[1, 6] <- rmse(y = y2, y_hat = y2_hat)
	
	# Calculate the mean absolute error.
	R_mat[2, 1] <- mae(y = y2, y_hat = mean_fit$mean)
	R_mat[2, 2] <- mae(y = y2, y_hat = naive_fit$mean)
	R_mat[2, 3] <- mae(y = y2, y_hat = snaive_fit$mean)
	R_mat[2, 4] <- mae(y = y2, y_hat = drift_fit$mean)
	R_mat[2, 5] <- mae(y = y2, y_hat = arima_fit$mean)
	R_mat[2, 6] <- mae(y = y2, y_hat = y2_hat)
	
	# Calculate the mean absolute percentage error.
	R_mat[3, 1] <- mape(y = y2, y_hat = mean_fit$mean)
	R_mat[3, 2] <- mape(y = y2, y_hat = naive_fit$mean)
	R_mat[3, 3] <- mape(y = y2, y_hat = snaive_fit$mean)
	R_mat[3, 4] <- mape(y = y2, y_hat = drift_fit$mean)
	R_mat[3, 5] <- mape(y = y2, y_hat = arima_fit$mean)
	R_mat[3, 6] <- mape(y = y2, y_hat = y2_hat)
	
	# Store the errors for the current iteration.
	R_list[[i]] <- R_mat
	
	# Increment the train set for the next iteration.
	X1 <- window(x = X, start = start(X1), end = end(X1) + c(1, 0))
	y1 <- window(x = Y, start = start(y1), end = end(y1) + c(1, 0))
	n1 <- dim(X1)[1]
	
	# Shift the test set for the next iteration.
	X2 <- window(x = X, start = end(X2) + c(1, -23), end = end(X2) + c(1, 0))
	y2 <- window(x = Y, start = end(y2) + c(1, -23), end = end(y2) + c(1, 0))
}

# Integrate the error estimates.
R_mat <- apply(X = simplify2array(x = R_list), MARGIN = c(1, 2), FUN = mean)

# Display the integrated error estimates.
print(x = t(round(x = R_mat, digits = 3)))
