# Attach the required packages.
require(package = "forecast")
require(package = "ggplot2")

# Load the dataset.
load(file = "../objects/subset_data_storage.RData")

# Number of observations in the dataset.
n <- dim(X_mat)[1]

# Construct the design matrix.
X <- cbind(Intercept = 1, Time = 1:n, X_mat[, -c(1, 2)]) |> 
	  ts(start = 1, frequency = 24)

# Extract the response variable.
Y <- X_mat[, 2] |> ts(start = 1, frequency = 24)

# Amount of time points for training <DD:HH>.
d <- c(3, 24)

# Amount of time points to forecast <DD:HH>.
h <- c(1, 0)

# Train set split.
X1 <- window(x = X, start = c(1, 1), end = d)
y1 <- window(x = Y, start = c(1, 1), end = d)
n1 <- dim(X1)[1]
p <- dim(X1)[2]

# Test set split.
X2 <- window(x = X, start = end(X1) + c(1, -23), end = end(X1) + h)
y2 <- window(x = Y, start = end(y1) + c(1, -23), end = end(y1) + h)
n2 <- dim(X2)[1]

# Train set time series plot.
p1 <- autoplot(object = y1) + labs(y = expression(NO[2])) + theme_minimal()

# Test set time series plot.
p2 <- autoplot(object = y2) + labs(y = expression(NO[2])) + theme_minimal()

# Define the root mean square error loss function.
rmse <- function(y, y_hat) { sqrt(x = mean(x = (y - y_hat)^2)) }

# Define the mean absolute error loss function.
mae <- function(y, y_hat) { mean(x = abs(x = y - y_hat)) }

# Define the mean absolute percentage error loss function.
mape <- function(y, y_hat) { mean(x = abs(x = (y - y_hat) / y) * 100) }

# Save the computed objects.
save(X, Y, X1, y1, n1, X2, y2, n2, p, h, p1, p2, rmse, mae, mape, 
	  file = "../objects/transformed_data_storage.RData")
