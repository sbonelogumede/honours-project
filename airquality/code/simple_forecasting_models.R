require(package="forecast")
require(package="fpp3")
require(package="gridExtra")
require(package="zoo")

load(file="../object/data_storage.RData")
full_data <- rbind(data_storage[[1]], data_storage[[2]])

no2_data <- ts(data=full_data$NO2, frequency=24)
no2_data <- na.approx(object=no2_data) # Run linear interpolation for NA.
train_no2 <- window(x=no2_data, start=c(1, 1), end=c(365, 24)) # Jan - Dec 2019
test_no2 <- window(x=no2_data, c(366, 1), end=c(397, 24)) # Jan 2020

ts_plots <- function(x, label="", x_lab="Time", y_lab="f(time)"){
	p1 <- autoplot(object=x) +
				autolayer(ma(x=x, order=718), series="MA718") +
				labs(title=label, x=x_lab, y=y_lab) +
				theme_minimal()
	p2 <- ggAcf(x=x) + labs(title=label) + theme_minimal()
	p3 <- ggPacf(x=x) + labs(title=label) + theme_minimal()
	arrangeGrob(p1, p2, p3, nrow=1)
}

train_no2[train_no2 == 0] <- 1 # Add some epsilon where NO2 = 0.
p1 <- ts_plots(x=train_no2, y_lab=expression(NO[2]))
ggsave("../img/no2_ts_2019.png", plot=p1, width=24, height=6, units="in", dpi=600)

BoxCox.lambda(x=train_no2, method="guerrero") # Stabilize the variance
train_log_no2 <- log(x=train_no2) # Stabilize the variance.
p2 <- ts_plots(x=train_log_no2, y_lab=expression(ln(NO[2])))
ggsave("../img/log_no2_ts_2019.png", plot=p2, width=24, height=6, units="in", dpi=600)

nsdiffs(x=train_log_no2) # Seasonality test. 
ndiffs(x=train_log_no2) # Trend test.
train_diff_log_no2 <- diff(x=train_log_no2) # First-order differencing.
p3 <- ts_plots(x=train_diff_log_no2, y_lab=expression(ln(NO[2])))
ggsave("../img/diff_log_no2_ts_2019.png", plot=p3, width=24, height=6, units="in", dpi=600)

horizon <- c(1, 24, 168, 720) # horizons: hour, day, week, month
n <- length(horizon)
col_name <- c("Average", "Naive", "SNaive", "Drift", "ARIMA")
model_performance <- matrix(data=NA, nrow=n, ncol=5, dimnames=list(horizon, col_name))
for(i in 1:n){
	average_pred <- meanf(y=train_no2, h=horizon[i]) # Average model fit.
	naive_pred <- naive(y=train_no2, h=horizon[i]) # Naive model fit.
	snaive_pred <- snaive(y=train_no2, h=horizon[i]) # Seasonal naive model fit.
	drift_pred <- rwf(y=train_no2, h=horizon[i], drift=TRUE) # Drift model fit.
	arima_model <- arima(x=train_no2, order=c(2, 1, 0), seasonal=list(order=c(2, 1, 0)))
	arima_pred <- forecast(object=arima_model, h=horizon[i])
	
	X <- list(average_pred, naive_pred, snaive_pred, drift_pred, arima_pred)
	test_set <- window(x=test_no2, start=c(366, 1), end=c(366, h=horizon[i]))
	
	model_performance[i, ] <- round(sapply(X=X, FUN = function(pred){
		return(accuracy(object=pred, x=test_set)[2, "RMSE"])}), 2)
	
	p <- autoplot(object = test_no2) + 
		autolayer(object=average_pred, series="Average", PI=FALSE) +
		autolayer(object=naive_pred, series="Naive", PI=FALSE) +
		autolayer(object=snaive_pred, series="Seasonal Naive", PI=FALSE) +
		autolayer(object=drift_pred, series="Drift", PI=FALSE) +
		autolayer(object=arima_pred, series="ARIMA", PI=FALSE) +
		labs(y=expression("Test set" ~ NO[2])) +
		theme_minimal()
	filename <- paste0("../img/h", horizon[i], ".png")
	ggsave(filename=filename, plot=p, width=8, height=6, units="in", dpi=600)
}

print(model_performance)
