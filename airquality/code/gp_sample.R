require(package = "parallel")
require(package = "rstan")
require(package = "zoo")

load(file = "../objects/data_storage.RData")
train <- data_storage[[2]]
test <- data_storage[[3]]

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

n <- nrow(train)
horizon <- c(24, 168, 720)
gp_mat <- matrix(data = NA, nrow = 2, ncol = length(horizon), 
						dimnames = list(c("RMSE", "MAE"), horizon))
gp_opt1 <- stan_model(file = 'fit_covar1.stan')
for(i in 1:length(horizon)){
	m <- horizon[i]
	x <- 1 : n
	y <- na.locf(na.approx(train$NO2[1:n], na.rm = FALSE), fromLast = TRUE) 
	x_predict <- (n + 1) : (n + m)
	y_predict <- na.locf(na.approx(test$NO2[1:m], na.rm = TRUE), fromLast = TRUE)
	
	truth <- list(x = c(x, x_predict), f = c(y, y_predict))
	data <- list(N_obs = length(x), x_obs = x, y_obs = y, N_predict = m, x_predict = x_predict)
	
	opt_fit <- optimizing(gp_opt1, data = data, seed = 5838298, hessian = FALSE)
	
	pred_data <- list(
		alpha = opt_fit$par[2], 
		rho = opt_fit$par[1], 
		sigma = opt_fit$par[3],
		N_obs = data$N_obs, 
		x_obs = data$x_obs, 
		y_obs = data$y_obs,
		N_predict = data$N_predict, 
		x_predict = data$x_predict
	)
	pred_opt_fit <- stan(
		file = 'fit_normal_anal.stan', 
		data = pred_data,
		iter = 1000, 
		warmup = 0, 
		chains = 1, 
		seed = 5838298, 
		refresh = 1000,
		algorithm = "Fixed_param"
	)
	
	save(pred_opt_fit, data, truth, file = paste0("../object/opt_fit_", m ,".RData"))
	
	extracted_samples <- extract(pred_opt_fit)
	f_predict_samples <- extracted_samples$f_predict
	forecast_means <- apply(X = f_predict_samples, MARGIN = 2, FUN = mean)
	
	gp_mat[1, i] <- round(sqrt(mean((y_predict - forecast_means)^2)), 2)
	gp_mat[2, i] <- round(mean(abs(y_predict - forecast_means)), 2)
}

print(x = gp_mat)
