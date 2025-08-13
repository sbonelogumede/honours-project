require(package="forecast")
require(package="fpp3")
require(package="zoo")

load(file="../objects/data_storage.RData")
train <- data_storage[[1]]
test <- data_storage[[2]]

# Variables of interest.
x <- 1 : nrow(train)
train_no2 <- train$NO2

# Add some small epsilon where NO2 = 0.
train_no2[train_no2 == 0] <- 0.001
# Running linear interpolation for NA's.
train_no2 <- na.approx(object=train_no2)

# Display the data.
par(mfrow=c(1, 3))
plot(x=x, y=train_no2, main="", xlab="Time", ylab=expression(NO[2]), 
	  cex=0.5, col="lightskyblue", pch=18)
ggAcf(x=train_no2) + labs(title="") + theme_minimal()
ggPacf(x=train_no2) + labs(title="") + theme_minimal()

# Check if there is a need to stabilize variance.
BoxCox.lambda(x=train_no2, method="guerrero")

# Stabilize variance.
log_train_no2 <- log(train_no2)

# Display the variance stabilized data.
plot(x=x, y=log_train_no2, main="", xlab="Time", ylab=expression(ln(NO[2])), 
	  cex=0.5, col="lightskyblue", pch=18, ylim=c(0, 5))
ggAcf(x=log_train_no2) + labs(title="") + theme_minimal()
ggPacf(x=train_no2) + labs(title="") + theme_minimal()

# Check if there is a need for removing seasonality.
nsdiffs(x=log_train_no2)

# Average model fit.
average_model <- meanf(y=train_no2, h=3)
average_model

# Naive model fit.
naive_model <- naive(y=train_no2, h=3)
naive_model

# Seasonal naive model fit.
snaive_model <- snaive(y=train_no2, h=3)
snaive_model

# Drift model fit.
drift_model <- rwf(y=train_no2, h=3, drift=T)
drift_model

# ARIMA model fit.
arima_model <- arima(x=train_no2, order=c(0, 1, 1), seasonal=list(order=c(0, 1, 1)))
arima_preds <- predict(object=arima_model, h=3)
arima_preds
