require(package="forecast")
require(package="fpp3")

load(file="../objects/data_storage.RData")
train <- data_storage[[1]]
test <- data_storage[[2]]

# Average model fit.
average_model <- meanf(y=train$NO2, h=3)
average_model

# Naive model fit.
naive_model <- naive(y=train$NO2, h=3)
naive_model

# Seasonal naive model fit.
snaive_model <- snaive(y=train$NO2, h=3)
snaive_model

# Drift model fit.
drift_model <- rwf(y=train$NO2, h=3, drift=T)
drift_model

# ARIMA model fit.
arima_model <- arima(x=train$NO2, order=c(0, 1, 1), seasonal=list(order=c(0, 1, 1)))
arima_preds <- predict(object=arima_model, h=3)
arima_preds

BoxCox.lambda(train$NO2)

ggAcf(train$NO2) + labs(title = "")+ theme_minimal()
ggPacf(train$NO2) + labs(title = "") + theme_minimal()
ggAcf(log(train$NO2)) + labs(title = "") + theme_minimal()
ggPacf(log(train$NO2)) + labs(title = "") + theme_minimal()
