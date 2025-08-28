require(package="forecast")
require(package="fpp3")
require(package="zoo")

load(file="../object/transformed_data_storage.RData")
full_data <- rbind(transformed_data[[1]], transformed_data[[2]], transformed_data[[3]])

no2_data <- ts(data=full_data$NO2, frequency=24)
no2_data <- na.approx(object=no2_data) # Run linear interpolation for NA.
train_no2 <- window(x=no2_data, start=c(1, 1), end=c(730, 24)) # Jan 2018 - Dec 2019
test_no2 <- window(x=no2_data, start=c(731, 1), end=c(762, 24)) # Jan 2020

nsdiffs(x=train_no2) # Seasonality test. 
ndiffs(x=train_no2) # Trend test.
train_diff_no2 <- diff(x=train_no2) # First-order differencing.

png("../img/stationary_no2.png", width=6.5, height=4, units="in", res=300)
plot(x=full_data$DateTime[1:17515], y=train_diff_no2, main="", 
	  xlab="Time", ylab=expression(ln(NO[2]) ~ "(" * mu * "g/m"^3 * ")"), 
	  col="steelblue", type="l", cex=0.5)
dev.off()

horizon <- c(24, 168, 720) # horizons: day, week, month
col_name <- c("Average", "Naive", "SNaive", "Drift", "ARIMA")
n <- length(horizon)
m <- length(col_name)
rmse_mat <- matrix(data=NA, nrow=n, ncol=m, dimnames=list(1:n, col_name))
mae_mat <- matrix(data=NA, nrow=n, ncol=m, dimnames=list(1:n, col_name))

for(i in 1:n){
	mean_model <- meanf(y = train_no2, h = horizon[i]) # Average model fit.
	naive_model <- naive(y = train_no2, h = horizon[i]) # Naive model fit.
	snaive_model <- snaive(y = train_no2, h = horizon[i]) # Seasonal naive model fit.
	drift_model <- rwf(y = train_no2, h = horizon[i], drift = TRUE) # Drift model fit.
	arima_model <- arima(x = train_no2, order = c(1, 1, 0))
	arima_fc <- forecast(object = arima_model, h = horizon[i])
	
	mean_p <- exp(x = mean_model$mean)
	naive_p <- exp(x = naive_model$mean)
	snaive_p <- exp(x = snaive_model$mean)
	drift_p <- exp(x = drift_model$mean)
	arima_p <- exp(x = arima_fc$mean)
	
	X <- list(mean_p, naive_p, snaive_p, drift_p, arima_p)
	test_set <- exp(x = window(x=test_no2, start=c(731, 1), end=c(731, h=horizon[i])))
	
	rmse_mat[i, ] <- round(sapply(X=X, FUN = function(pred){
			mean((pred-test_set)^2)
		}), 2)
	mae_mat[i, ] <- round(sapply(X=X, FUN = function(pred){
			mean(abs(pred-test_set))
		}), 2)
}

rmse_gp <- c(6.07, 8.73, 10.72)
mae_gp <- c(5.47, 7.29,  9.14)
rmse_performance <- cbind(Horizon=horizon, rmse_mat, GP=rmse_gp)
mae_performance <- cbind(Horizon=horizon, mae_mat, GP=mae_gp)
print(rmse_performance)
print(mae_performance)
