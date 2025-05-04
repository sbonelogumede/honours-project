set.seed(1)

x <- train_time
d <- abs(outer(x, x, '-')) # Compute distance matrix d_{ij} = |x_i - x_j|

mx <- x^2 / 4
Sigma_SE <- exp(-d^2 / 2) # Squared exponential kernel

y <- mvtnorm::rmvnorm(1, mean=mx, sigma=Sigma_SE)

plot(
	x, 
	y, 
	ylim = c(-10, 10),
	xlab = 'Time',
	lwd = 2,
	col = 1
)

for (i in 1:5) {
	y = mvtnorm::rmvnorm(1, sigma=Sigma_SE)
	lines(x, y, col=i+1, lwd=2)
}

df <- EuStockMarkets[, 'FTSE'] # Load data
n <- length(df) # Number of observations in the data
train_end_index <- floor(0.7 * n) # (70/30)% split of the data

train <- window(df, end = time(df)[train_end_index]) # Training data
test <- window(df, start = time(df)[train_end_index + 1]) # Testing data
train_time <- as.numeric(time(train)) # Extract the time component for training data
test_time <- as.numeric(time(test)) # Extract the time component for testing data

data_list <- list(
	y_obs = train, 
	x_obs = train_time, 
	N_obs = length(train_time), 
	x2 = test_time, 
	N2 = length(test_time)
)

m1 <- cmdstan_model(stan_file='stan/presentaion.stan')

m1.fit <- m1$sample(
	data = data_list, 
	seed = 123, 
	chains = 4, 
	parallel_chains = 2
)

train_preds <- ?
	test_preds <- ?
	
	train_ts <- ts(train_preds, start=start(train), frequency=frequency(train))
test_ts <- ts(test_preds, start=start(test), frequency=frequency(test))

autoplot(df) +
	autolayer(train_ts, series='Train', size=1.5) +
	autolayer(test_ts, series='Test', size=1.5)

# Gaussian process parameters
alpha_true <- 15
rho_true <- 5
sigma_true <- 4

# Stan parameters
prior_data <- list(
	alpha = alpha_true, 
	rho = rho_true, 
	sigma = sigma_true, 
	N = n_train, 
	x = train_time 
)

# Train prior model
prior_model <- stan(
	file = 'stan_programs/presentation.stan', 
	data = prior_data, 
	warmup = 0, 
	iter = 4000, 
	chains = 1, 
	seed = 494838, 
	algorithm = 'Fixed_param', 
	refresh = 0, 
	verbose = FALSE
)

set.seed(1)
prior_samples <- (extract(prior_model)$f)[1:10, ]
p <- ggplot() + 
	geom_line(aes(x=train_time, y=as.numeric(train)), 
				 color='black')

for(i in 1:10){
	p <- p + 
		geom_line(aes(x=train_time, y=prior_samples[i, ]), 
					 color=scales::hue_pal()(10)[i], linewidth=2)
}

p + 
	labs(title='Prior realizations', x='Time', y='f')  +
	theme_minimal()

# Concatenate
x_predict <- c(train_time, test_time) 
N_predict <- length(x_predict) 
observed_idx <- 1:n_train 

# Stan parameters
data <- list(
	N_obs = n_train, 
	y_obs = as.numeric(train), 
	observed_idx = observed_idx, 
	N_predict = N_predict, 
	x_predict = x_predict, 
	alpha = alpha_true, 
	rho = rho_true, 
	sigma = sigma_true 
)

# Train posterior model
normal_fit <- stan(
	file = 'stan_programs/fit_normal.stan', 
	data = data, 
	warmup = 500, 
	iter = 4000, 
	chains = 8, 
	seed = 494838 
)
