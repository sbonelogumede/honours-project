# Load the required packages.
require(package = "forecast")
require(package = "fpp3")
require(package = "zoo")

# Load the data.
load(file = "../object/transformed_data_storage.RData")
feb2018 <- (transformed_data[[2]])[745:1416, ]
feb2018[, 2:5] <- na.approx(feb2018[, 2:5], na.rm = FALSE)
feb2018[, 2:5] <- na.locf(feb2018[, 2:5], na.rm = FALSE)
feb2018[, 2:5] <- na.locf(feb2018[, 2:5], fromLast = TRUE)

# Time series plot.
plot(x = feb2018$DateTime[1:192], 
	  y = feb2018$NO2[1:192], 
	  main = "", 
	  xlab = "Time", 
	  ylab = expression(log(NO[2]) ~ "(" * mu * "g/m"^3 * ")"),
	  type = "l",
	  col = "steelblue",
	  lwd = 2)

# Train set split.
n1 <- 168
X1 <- cbind("(Intercept)" = 1, Time = 1 : n1, feb2018[1 : n1, 3:5])
p <- dim(X1)[2]
y1 <- na.approx(feb2018$NO2[1 : n1])

# Test set split.
n2 <- 24
X2 <- cbind("(Intercept)" = 1, Time = (n1 + 1) : (n1 + n2), 
				feb2018[(n1 + 1) : (n1 + n2), 3:5])
y2 <- feb2018$NO2[(n1 + 1) : (n1 + n2)]

# Store the splits.
save(n1 = n1, p = p, X1 = X1, y1 = y1, n2 = n2, X2 = X2, y2 = y2, 
	  file = "../object/parameters.RData")

# Train the simple forecasting models.
mean_model <- meanf(y = y1, h = n2)
naive_model <- naive(y = y1, h = n2)
snaive_model <- snaive(y = y1, h = n2)
drift_model <- rwf(y = y1, h = n2, drift = TRUE)
arima_model <- arima(x = y1, order = c(1, 0, 0))
arima_fc <- forecast(object = arima_model, h = n2)

# Test the simple forecasting models.
mean((mean_model$mean - y2)^2, na.rm = TRUE)
mean((naive_model$mean - y2)^2, na.rm = TRUE)
mean((snaive_model$mean - y2)^2, na.rm = TRUE)
mean((drift_model$mean - y2)^2, na.rm = TRUE)
mean((arima_fc$mean - y2)^2, na.rm = TRUE)
