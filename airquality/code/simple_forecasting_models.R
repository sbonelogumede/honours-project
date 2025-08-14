require(package="forecast")
require(package="fpp3")
require(package="gridExtra")
require(package="zoo")

load(file="../objects/data_storage.RData")
train <- data_storage[[1]]
test <- data_storage[[2]]

# Variables of interest.
x <- 1 : nrow(train)
train_no2 <- train$NO2

# Add some small epsilon where NO2 = 0.
train_no2[train_no2 == 0] <- 1
# Running linear interpolation for NA's.
train_no2 <- na.approx(object=train_no2)

ts_plots <- function(x, label="y = f(x)", xlab="x", ylab="y"){
	p1 <- autoplot(object=x) +
		autolayer(ma(x=x, order=718), series="MA718", linewidth=1.5) +
		ggtitle(label=label) +
		xlab(label="Time") +
		ylab(label=ylab) +
		guides(colour=guide_legend(title="Model")) +
		theme_minimal()
	p2 <- ggAcf(x=x) +
		labs(title=label) + 
		theme_minimal()
	p3 <- ggPacf(x=x) + 
		labs(title=label) + 
		theme_minimal()
	arrangeGrob(p1, p2, p3, nrow=1)
}

train_no2_ts <- ts(data=train_no2, frequency=24)
p1 <- ts_plots(x=train_no2_ts, 
					label=expression("Series:" ~ NO[2]), 
					ylab=expression(NO[2]))
ggsave(filename="../images/no2_ts_2019.png", plot=p1, width=24, height=6, 
		 units="in", dpi=600)

# Check if there is a need to stabilize variance.
BoxCox.lambda(x=train_no2_ts, method="guerrero")

# Stabilize variance.
train_log_no2_ts <- log(x=train_no2_ts)
p2 <- ts_plots(x=train_log_no2_ts, 
					label=expression("Series:" ~ ln(NO[2])), 
					ylab=expression(ln(NO[2])))
ggsave(filename="../images/log_no2_ts_2019.png", plot=p2, width=24, height=6, 
		 units="in", dpi=600)

# Check if there is a need for removing seasonality.
nsdiffs(x=train_log_no2_ts)

# Check if there is a need for removing trend.
ndiffs(x=train_log_no2_ts)

train_diff_log_no2_ts <- diff(x=train_log_no2_ts)
p3 <- ts_plots(x=train_diff_log_no2_ts, 
					label=expression("Series:" ~ ln(NO[2])), 
					ylab=expression(ln(NO[2])))
ggsave(filename="../images/diff_log_no2_ts_2019.png", plot=p3, width=24, height=6, 
		 units="in", dpi=600)

# Average model fit.
average_model <- meanf(y=train_no2_ts, h=3)
average_model

# Naive model fit.
naive_model <- naive(y=train_no2_ts, h=3)
naive_model

# Seasonal naive model fit.
snaive_model <- snaive(y=train_no2_ts, h=3)
snaive_model

# Drift model fit.
drift_model <- rwf(y=train_no2_ts, h=3, drift=T)
drift_model

# ARIMA model fit.
arima_model <- arima(x=train_no2_ts, order=c(0, 1, 1), seasonal=list(order=c(0, 1, 1)))
arima_preds <- predict(object=arima_model, h=3)
arima_preds
