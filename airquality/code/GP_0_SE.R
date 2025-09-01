require(package = "rstan")
require(package = "parallel")

h_vector <- c(24, 168, 744)
R <- matrix(data = NA, nrow = 3, ncol = 2, dimnames = list(h_vector, c("RMSE", "MAE")))

for(i in 1:3){
	load(file = paste0(h_vector[i], "h_parameters.RData"))
	parameters <- list(n1 = n1, p = p, X1 = X1, y1 = log(y1), n2 = n2, X2 = X2)
	coresNumber <- detectCores() - 2
	
	stan_fit <- stan(file = "GP_0_SE.stan", 
	                 cores = coresNumber,
	                 data = parameters, 
	                 verbose = FALSE)
	stan_summary <- (summary(stan_fit))$summary
	
	y2_hat <- stan_summary[grep("^y2", rownames(stan_summary)), "mean"] |> 
		as.vector() |> 
		exp()
	
	R[i, ] <- c(mean((y2 - y2_hat)^2, na.rm = TRUE) |> sqrt() |> round(digits = 3), 
					mean(abs(y2 - y2_hat), na.rm = TRUE) |> round(digits = 3))
}

print(x = R)
write.table(R, file = "GP_0_SE_R.txt", sep = "\t", quote = FALSE, col.names = NA)
