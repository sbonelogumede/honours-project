
# Attach the required packages.
require(package = "imputeTS")

# Load the data.
load(file = "../objects/extracted_data_storage.RData")

# Extract the data.
X1 <- extracted_data[[1]]
X2 <- extracted_data[[2]]

# Factor out the date time column.
X11 <- X1[, 1]
X12 <- X1[, -1]
X21 <- X2[, 1]
X22 <- X2[, -1]

# Run Kalman filter interpolation.
X12 <- apply(X = X12, MARGIN = 2, FUN = function(column){
	na_kalman(x = column)
}) |> as.data.frame()
X22 <- apply(X = X22, MARGIN = 2, FUN = function(column){
	na_kalman(x = column)
}) |> as.data.frame()

# Add the date time in to the data.
X1 <- cbind(DateTime = X11, X12)
X2 <- cbind(DateTime = X21, X22)

# Store the imputed data.
imputed_data <- list(X1, X2)

# Save the imputed data into a file.
save(imputed_data, file = "../objects/imputed_data_storage.RData")
