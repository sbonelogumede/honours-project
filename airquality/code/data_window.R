# Attach the required packages.
require(package = "forecast")
require(package = "ggplot2")

# Load the dataset.
load(file = "../objects/imputed_data_storage.RData")

# Extract the dataset.
index <- 120
h <- 120

# Train split
X1 <- cbind(Intercept = 1, 
				Time = 1:index, 
				imputed_data[[1]][1:index, -c(1, 2)]) |> 
	ts(start = 1, frequency = 24)
head(x = X1)

# Test split
X2 <- cbind(Intercept = 1, 
				Time = (index + 1):(index + h), 
				imputed_data[[1]][(index + 1):(index + h), -c(1, 2)]) |> 
	ts(start = end(x = X1) + c(1, -23), frequency = 24)
head(x = X2)

# Extract the response variable.
y1 <- imputed_data[[1]][1:index, 2] |> 
	ts(start = 1, frequency = 24)
head(x = y1)

y2 <- imputed_data[[1]][(index + 1):(index + h), 2] |> 
	ts(start = end(x = y1) + c(1, -23), frequency = 24)
head(x = y2)

n1 <- dim(x = X1)[1]
n2 <- dim(x = X2)[1]
p <- dim(x = X1)[2]

p1 <- autoplot(object = y1) +
	labs(y = expression(NO[2])) +
	theme_minimal()
p1

p2 <- autoplot(object = y2) +
	labs(y = expression(NO[2])) +
	theme_minimal()
p2

# Save these objects.
save(X1, y1, n1, p1, X2, y2, n2, p2, p, h, file = "../objects/window_data_storage.RData")
