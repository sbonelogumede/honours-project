set.seed(seed = 2025)

library(package = "R2jags")
library(package = "runjags")
library(package = "tidyverse")
library(package = "tidybayes")
library(package = "fields")
library(package = "mvtnorm")

year <- sample(x = 1:200, size = 100) %>% sort
x <- year / 100
d <- fields::rdist(x) # Distance matrix

alpha <- 0.2 # Hyperparameter
phi <- 1 
sigma <- sqrt(x = 2)
K <- exp(-(phi^2)*(d^2)) # Kernel

plot(d[, 1], K[, 1],
	  type="l",
	  xlab="Year Difference",
	  ylab="Autocorrelation",
	  col="steelblue",
	  lwd=2)

g <- mvtnorm::rmvnorm(n=5, sigma=(alpha^2)*K) # mean defaults to 0
matplot(x, t(g), type="l", ylab="G", xlab="Year")

# Simulating data
sigma_y <- 0.3 
eps <- rnorm(n=100, mean=0, sd=sigma_y) # random noise 
g <- rmvnorm(n=1, sigma=(alpha^2)*K) # Data generating function 
y <- c(g) + eps # Simulated data = GP + random noise 

plot(x=x, y=y, main="", xlab="Time", ylab="GP", type="p", pch=16, col="steelblue")
lines(x=x, y=g, col="hotpink", lwd=2)
legend(x="topright", 
		 legend=c("TRUE GP", "Simulated data"), 
		 col=c("hotpink", "steelblue"), 
		 lwd=2, pch=16)

# Model specification
# g ~ MVN(0, \Sigma)
# \Sigma = \apha^2 exp(-(\rho d)^2)
