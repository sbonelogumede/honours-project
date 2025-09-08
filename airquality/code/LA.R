require(package = "mvtnorm")

h_vec <- c(24, 168, 744)
res_mat <- matrix(nrow = 3, ncol = 3, dimnames = list(h_vec, c("alpha", "rho", "sigma")))

for(i in 1:3){
	load(file = paste0("../object/", h_vec[i], "h_parameters.RData"))
	x_vec <- y1
	n <- n1
	dist_mat <- dist(x = x_vec, diag = TRUE, upper = TRUE) |> as.matrix()
	dist_sq_mat <- dist_mat * dist_mat # Hadamard product
	
	nll <- function(par) {
		par <- exp(x = par) # Non-negative parameters
		cov_mat <- par[1]^2 * exp(x = (- dist_sq_mat / (2 * par[2]^2))) 
		+ par[3]^2 * diag(nrow = n) + 1e-6 * diag(nrow = n)
		-dmvnorm(x = x_vec, mean = rep(0, n), sigma = cov_mat, log = TRUE)
	}
	
	optim_obj <- optim(par = c(log(1), log(1), log(0.1)), fn = nll)
	res_mat[i, ] <- exp(x = optim_obj$par)
}

res_mat
