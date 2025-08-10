# X_t ~ GP(f(x), g(x))
# f(x) = x + e, where e ~ N(0, 4)
# g(x) = (sin(x))^2 + 1

rm(list = ls())
set.seed(seed = 2025)
x_seq <- seq(from = 1, to = 50, length.out = 11)
n <- 1000

f <- function(x){
	x + rnorm(n = 1, mean = 0, sd = 2)
}

g <- function(x){
	(sin(x))^2 + 1
}

#png(filename = "../images/gp.png", width = 8, height = 6, res = 600, units = "in")

plot(x = 0, 
	  y = 0, 
	  xlim = c(0, 55), 
	  ylim = c(0, 55), 
	  xlab = "t", 
	  ylab = expression(X[t]), 
	  main = "", 
	  type = "n")

for(x in x_seq){
	y_seq <- rnorm(n = n, mean = f(x), sd = g(x))
	y_dens <- density(x = y_seq)
	x_scaled <- x + y_dens$y * 10
	
	abline(v=x, col="lightgray")
	
	polygon(x = c(x, x_scaled, x),
			  y = c(min(y_dens$x), y_dens$x, max(y_dens$x)),
			  col = "lightblue", 
			  border = NA)
	
	lines(x = x_scaled, y = y_dens$x, col = "steelblue", lwd = 2)
}

#dev.off()
