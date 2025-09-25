
# Attach the required packages.
require(package = "forecast")
require(package = "fpp3")
require(package = "ggplot2")

# Load the dataset.
load(file = "../objects/imputed_data_storage.RData")

# Extract the dataset.
index <- 744 # Six months
X1 <- imputed_data[[1]][1:index, ] # Train split
X2 <- imputed_data[[1]][(index + 1) : (index + 744), ] # Test split

# Extract the response variable.
y1 <- X1$NO2 |> ts(frequency = 24)
y2 <- X2$NO2 |> ts(frequency = 24)

# Declare a vector of horizons of interest.
h_vec <- c(1, 24, 48, 168)
n <- length(x = h_vec)
m <- 4

# Results matrix.
R_mat <- matrix(data = NA, nrow = n, ncol = m, 
					 dimnames = list(h_vec, c("Mean", "Naive", "Snaive", "Drift")))

# Setup the plotting environment correctly.
par(mar = c(5, 4, 4, 8), xpd = TRUE)

for(i in 1:n) {
	# Fit the simple forecasting models.
	meanf_p <- meanf(y = y1, h = h_vec[i])$mean
	naive_p <- naive(y = y1, h = h_vec[i])$mean
	snaive_p <- snaive(y = y1, h = h_vec[i])$mean
	rwf_p <- rwf(y = y1, h = h_vec[i], drift = TRUE)$mean
	
	# Calculate the mean square error.
	R_mat[i, 1] <- MSE((meanf_p - y2[1:h_vec[i]]), na.rm = TRUE)
	R_mat[i, 2] <- MSE((naive_p - y2[1:h_vec[i]]), na.rm = TRUE)
	R_mat[i, 3] <- MSE((snaive_p - y2[1:h_vec[i]]), na.rm = TRUE)
	R_mat[i, 4] <- MSE((rwf_p - y2[1:h_vec[i]]), na.rm = TRUE)
	
	# Plot the time series and overlay the forecasts.
	plot(x = 1:h_vec[i], y = y2[1:h_vec[i]], type = "l", col = "black", main = "", 
		  xlab = "Time", ylab = expression(NO[2]), lwd = 2)
	lines(x = as.numeric(meanf_p), col = "lightseagreen", lwd = 2)
	lines(x = as.numeric(naive_p), col = "lightskyblue", lwd = 2)
	lines(x = as.numeric(snaive_p), col = "plum", lwd = 2)
	lines(x = as.numeric(rwf_p), col = "red", lwd = 2)
	legend(x = "topright", inset = c(-0.4, 0), xpd = TRUE,
			 legend = c("Mean", "Naive", "Snaive", "Drift"), 
			 col = c("lightseagreen", "lightskyblue", "plum", "red"), 
			 lty = 1, lwd = 2, bty = "n")
	
	# Finish rendering the revious plot properly.
	Sys.sleep(time = 1)
}

# Display the results
print(x = round(x = R_mat, digits = 3))
