rm(list = ls())

library(invgamma)

set.seed(seed = 2025)

alpha <- abs(x = rnorm(n = 1, mean = 0, sd = 1))
rho <- rinvgamma(n = 1, shape = 1)
sigma <- abs(x = rnorm(n = 1, mean = 0, sd = 1))

sq_exp <- function(par){
	x1 <- par[1]
	x2 <- par[2]
	alpha * exp(-0.5 * (abs(x1 - x2)/rho)^2) + sigma
}

lower <- -10
upper <- 10
n <- 1000
x1 <- seq(from = lower, to = upper, length.out = n)
x2 <- seq(from = lower, to = upper, length.out = n)
par_seq <- expand.grid(x1, x2)
par_seq <- par_seq |> as.matrix()
se_seq <- numeric(n^2)
for(i in 1:n^2){
	se_seq[i] <- sq_exp(par_seq[i, ])
}

plot(x=par_seq[, 1], y=se_seq, main="", xlab="x1", ylab="SE", col="darkorange", pch=18)