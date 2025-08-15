require(package="colormap")
require(package="parallel")
require(package="rstan")
require(package="zoo")

load(file="../object/data_storage.RData")
train <- data_storage[[1]]
test <- data_storage[[2]]

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

util <- new.env()

par(family="CMU Serif", las=1, bty="l", cex.axis=1, cex.lab=1, cex.main=1,
	 xaxs="i", yaxs="i", mar = c(5, 5, 3, 5))

n <- 200 # train set observations
m <- 50 # test set observations

x <- 1 : n
y <- na.approx(train$NO2, na.rm = FALSE)[1 : n]
x_predict <- max(x) + 1 : m
y_predict <- na.approx(train$NO2[n + 1 : m], na.rm = FALSE)

truth <- list(x = c(x, x_predict), f = c(y, y_predict))
data = list(N_obs = n, x_obs = x, y_obs = y, N_predict=m, x_predict=x_predict)

gp_opt1 <- stan_model(file='fit_covar1.stan')
opt_fit <- optimizing(gp_opt1, data=data, seed=5838298, hessian=FALSE)

alpha <- opt_fit$par[2]
rho <- opt_fit$par[1]
sigma <- opt_fit$par[3]

sprintf('alpha = %s', alpha)
sprintf('rho = %s', rho)
sprintf('sigma = %s', sigma)

pred_data <- list(alpha=alpha, rho=rho, sigma=sigma,
						N_obs=data$N_obs, x_obs=data$x_obs, y_obs=data$y_obs,
						N_predict=data$N_predict, x_predict=data$x_predict)
pred_opt_fit <- stan(file='fit_normal_anal.stan', data=pred_data,
							iter=1000, warmup=0, chains=1, seed=5838298, refresh=1000,
							algorithm="Fixed_param")

source('gp_utility.R', local=util)

util$plot_gp_post_realizations(pred_opt_fit, data, truth, "Posterior Realizations")
util$plot_gp_post_quantiles(pred_opt_fit, data, truth, "Posterior Marginal Quantiles")
