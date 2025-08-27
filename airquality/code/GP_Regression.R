require(package = "rstan")
require(package = "parallel")

load(file = "../object/parameters.RData")
parameters <- list(n1 = n1, p = p, X1 = X1, y1 = y1, n2 = n2, X2 = X2)
coresNumber <- detectCores() - 2

stan_fit <- stan(file = "GP_Regression.stan", 
					  cores = coresNumber,
					  data = parameters, 
					  verbose = FALSE)
stan_summary <- (summary(stan_fit))$summary

y2_hat <- stan_summary[grep("^y2", rownames(stan_summary)), "mean"] |> as.vector()
mean((y2 - y2_hat)^2, na.rm = TRUE)
