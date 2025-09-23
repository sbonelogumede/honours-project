
# Attach the required packages.
require(package = "forecast")
require(package = "fpp3")

# Load the dataset.
load(file = "../objects/extracted_data_storage.RData")

# Extract the dataset.
X1 <- extracted_data[[1]]
X2 <- extracted_data[[2]]

# Extract the response variable.
y1 <- X1$NO2
y2 <- X2$NO2

# Declare a vector of horizons of interest.
h_vec <- c(1, 24, 48)
n <- length(x = h_vec)
m <- 4

# Results matrix.
R_mat <- matrix(data = NA, nrow = n, ncol = m, 
					 dimnames = list(h_vec, c("Mean", "Naive", "SNaive", "Drift")))

for(i in 1:n) {
	# Fit the simple forecasting models.
	meanf_p <- meanf(y = y1, h = h_vec[i])$mean
	naive_p <- naive(y = y1, h = h_vec[i])$mean
	snaive_p <- snaive(y = y1, h = h_vec[i])$mean
	rwf_p <- rwf(y = y1, h = h_vec[i], drift = TRUE)$mean
	
	# Calculate the error.
	R_mat[i, 1] <- MSE((meanf_p - y2[1:h_vec[i]]), na.rm = TRUE)
	R_mat[i, 2] <- MSE((naive_p - y2[1:h_vec[i]]), na.rm = TRUE)
	R_mat[i, 3] <- MSE((snaive_p - y2[1:h_vec[i]]), na.rm = TRUE)
	R_mat[i, 4] <- MSE((rwf_p - y2[1:h_vec[i]]), na.rm = TRUE)
}

# Display the results
print(x = round(x = R_mat, digits = 3))
