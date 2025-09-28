# Attach the required packages.
require(package = "mvtnorm")

# Load the dataset.
load(file = "../objects/window_data_storage.RData")

# Extract the parameters.
x_vec <- y1
n <- n1

# Calculate the squared distances.
dist_mat <- dist(x = x_vec, diag = TRUE, upper = TRUE) |> as.matrix()
dist_sq_mat <- dist_mat * dist_mat # Hadamard product

nll <- function(par) {
	par <- exp(x = par) # Non-negative parameters
	cov_mat <- par[1]^2 * exp(x = (- dist_sq_mat / (2 * par[2]^2))) 
	+ par[3]^2 * diag(nrow = n) + 1e-6 * diag(nrow = n)
	-dmvnorm(x = x_vec, mean = rep(0, n), sigma = cov_mat, log = TRUE)
}

optim_obj <- optim(par = c(log(1), log(1), log(0.1)), fn = nll)
