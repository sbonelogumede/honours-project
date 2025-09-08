require(package = "rstan")
require(package = "parallel")

h_vec <- c(24, 168, 744)
R <- matrix(data = NA, nrow = 3, ncol = 2, dimnames = list(h_vec, c("RMSE", "MAE")))

for(i in 1:3){
	load(file = paste0("../object/", h_vec[i], "h_parameters.RData"))
	par <- list(n1 = n1, p = p, X1 = X1, y1 = log(y1), n2 = n2, X2 = X2)
	cn <- detectCores() - 2
	
	stan_fit <- stan(file = "GP_0_WN.stan", cores = cn, data = par, verbose = F)
	stan_summary <- (summary(stan_fit))$summary
	
	y2_hat <- stan_summary[grep("^y2", rownames(stan_summary)), "mean"] |> 
		as.vector() |> 
		exp()
	R[i, ] <- c(mean((y2 - y2_hat)^2, na.rm = TRUE) |> sqrt() |> round(digits = 3), 
					mean(abs(y2 - y2_hat), na.rm = TRUE) |> round(digits = 3))
}

print(x = R)
write.table(R, file = "GP_0_WN_R.txt", sep = "\t", quote = FALSE, col.names = NA)
