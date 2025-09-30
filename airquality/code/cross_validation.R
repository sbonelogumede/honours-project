# Attach the required packages.
require(package = "forecast")
require(package = "knitr")
require(package = "rstan")
require(package = "parallel")

# Load the dataset.
load(file = "../objects/window_data_storage.RData")

h <- 1
n2 <- 3
R_list <- list()
cores_number <- detectCores() - 2
R_mat <- matrix(data = NA, nrow = 3, ncol = 5, 
					 dimnames = list(c("RMSE", "MAE", "MPE"), 
					 					 c("Mean", "Naive", "Snaive", "Drift", "Gaussian")))

for(i in 1:n2){
	# Fit the model(s).
	mean_fit <- meanf(y = y1, h = h)
	naive_fit <- naive(y = y1, h = h)
	snaive_fit <- snaive(y = y1, h = h)
	drift_fit <- rwf(y = y1, h = h, drift = TRUE)
	stan_data <- list(X1 = X1, y1 = y1, n1 = n1, p = p, 
							X2 = matrix(data = X2[i, ], nrow = 1), n2 = h)
	stan_fit <- stan(file = "squared_exponential.stan", 
						  chains = 1,
						  cores = cores_number,
						  data = stan_data, 
						  seed = 2025,
						  verbose = FALSE)
	stan_summary <- (summary(stan_fit))$summary
	y2_hat <- stan_summary[grep("^y2", rownames(stan_summary)), "mean"] |> as.numeric()
	
	# Calculate the mean square error.
	R_mat[1, 1] <- sqrt(mean((mean_fit$mean - y2[i])^2))
	R_mat[1, 2] <- sqrt(mean((naive_fit$mean - y2[i])^2))
	R_mat[1, 3] <- sqrt(mean((snaive_fit$mean - y2[i])^2))
	R_mat[1, 4] <- sqrt(mean((drift_fit$mean - y2[i])^2))
	R_mat[1, 5] <- sqrt(mean((y2_hat - y2[i])^2))
	
	# Calculate the mean absolute error.
	R_mat[2, 1] <- abs(mean_fit$mean - y2[i])
	R_mat[2, 2] <- abs(naive_fit$mean - y2[i])
	R_mat[2, 3] <- abs(snaive_fit$mean - y2[i])
	R_mat[2, 4] <- abs(drift_fit$mean - y2[i])
	R_mat[2, 5] <- abs(y2_hat - y2[i])
	
	# Calculate the mean absolute percentage error.
	R_mat[3, 1] <- mean((abs(mean_fit$mean - y2[i]) / abs(y2[i])) * 100)
	R_mat[3, 2] <- mean((abs(naive_fit$mean - y2[i]) / abs(y2[i])) * 100)
	R_mat[3, 3] <- mean((abs(snaive_fit$mean - y2[i]) / abs(y2[i])) * 100)
	R_mat[3, 4] <- mean((abs(drift_fit$mean - y2[i]) / abs(y2[i])) * 100)
	R_mat[3, 5] <- mean((abs(y2_hat - y2[i]) / abs(y2[i])) * 100)
	
	R_list[[i]] <- R_mat
}

R_mat <- apply(X = simplify2array(x = R_list), MARGIN = c(1, 2), FUN = mean)
kable(x = round(x = R_mat, digits = 3))
