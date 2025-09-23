# Load the required packages.
require(package = "forecast")
require(package = "fpp3")
require(package = "zoo")

# Load the data.
load(file = "../objects/extracted_data_storage.RData")
extracted_data <- (extracted_data[[2]])[745:2160, ]
extracted_data[, 2:5] <- na.approx(extracted_data[, 2:5], na.rm = FALSE)
extracted_data[, 2:5] <- na.locf(extracted_data[, 2:5], na.rm = FALSE)
extracted_data[, 2:5] <- na.locf(extracted_data[, 2:5], fromLast = TRUE)

# Time series plot.
png(filename="../images/TRAIN.png", width=6.5, height=4, res=300, units="in")
plot(x = extracted_data$DateTime[1:672], 
	  y = extracted_data$NO2[1:672], 
	  main = "", 
	  xlab = "Time", 
	  ylab = expression(NO[2] ~ "(" * mu * "g/m"^3 * ")"),
	  type = "l",
	  col = "steelblue",
	  lwd = 2)
dev.off()

png(filename="../images/LOG_TRAIN.png", width=6.5, height=4, res=300, units="in")
plot(x = extracted_data$DateTime[1:672], 
	  y = log(extracted_data$NO2)[1:672], 
	  main = "", 
	  xlab = "Time", 
	  ylab = expression(log(NO[2]) ~ "(" * mu * "g/m"^3 * ")"),
	  type = "l",
	  col = "steelblue",
	  lwd = 2)
dev.off()

# Train set split.
n1 <- 672
X1 <- cbind("(Intercept)" = 1, Time = 1 : n1, extracted_data[1 : n1, 3:5])
p <- dim(X1)[2]
y1 <- na.approx(extracted_data$NO2[1 : n1])

# Test set split.


hs <- c(24, 168, 744)
RMSE <- matrix(data = NA, nrow = 3, ncol = 4, dimnames = list(hs, c("Average", "Naive", "Drift", "AR(1)")))
MAE <- matrix(data = NA, nrow = 3, ncol = 4, dimnames = list(hs, c("Average", "Naive", "Drift", "AR(1)")))

for(i in 1:3){
	n2 <- hs[i]
	X2 <- cbind("(Intercept)" = 1, Time = (n1 + 1) : (n1 + n2), 
					extracted_data[(n1 + 1) : (n1 + n2), 3:5])
	y2 <- extracted_data$NO2[(n1 + 1) : (n1 + n2)]
	
	# Store the splits.
	save(n1 = n1, p = p, X1 = X1, y1 = y1, n2 = n2, X2 = X2, y2 = y2, 
		  file = paste0("../object/", hs[i], "h_parameters.RData"))
	
	# Train the simple forecasting models.
	mean_model <- meanf(y = y1, h = n2)
	naive_model <- naive(y = y1, h = n2)
	snaive_model <- snaive(y = y1, h = n2)
	drift_model <- rwf(y = y1, h = n2, drift = TRUE)
	arima_model <- arima(x = log(y1), order = c(1, 0, 0))
	arima_fc <- forecast(object = arima_model, h = n2)
	
	# Test the simple forecasting models.
	RMSE[i, 1] <- mean((mean_model$mean - y2)^2, na.rm = TRUE) |> sqrt() |> round(digits = 3) 
	MAE[i, 1] <- mean(abs(mean_model$mean - y2), na.rm = TRUE) |> round(digits = 3)
	RMSE[i, 2] <- mean((naive_model$mean - y2)^2, na.rm = TRUE) |> sqrt() |> round(digits = 3)
	MAE[i, 2] <- mean(abs(naive_model$mean - y2), na.rm = TRUE) |> round(digits = 3)
	RMSE[i, 3] <- mean((drift_model$mean - y2)^2, na.rm = TRUE) |> sqrt() |> round(digits = 3) 
	MAE[i, 3] <- mean(abs(drift_model$mean - y2), na.rm = TRUE) |> round(digits = 3)
	RMSE[i, 4] <- mean((exp(x = arima_fc$mean) - y2)^2, na.rm = TRUE) |> sqrt() |> round(digits = 3)
	MAE[i, 4] <- mean(abs(exp(x = arima_fc$mean) - y2), na.rm = TRUE) |> round(digits = 3)
}

print(x = t(RMSE))

print(x = t(MAE))