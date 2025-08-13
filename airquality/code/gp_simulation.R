set.seed(seed=2025)

f <- function(x){
	x + rnorm(n=1, mean=0, sd=2)
}

g <- function(x){
	1 + (sin(x))^2
}

png(filename="../images/gp.png", width=8, height=6, res=600, units="in")
plot(x=0, y=0, xlim=c(0, 55), ylim=c(0, 55), main="", xlab="t", ylab=expression(X[t]), type="n")
x_seq <- seq(from=1, to=50, length.out=11)

for(x in x_seq){
	y_seq <- rnorm(n=1000, mean=f(x), sd=g(x))
	y_dens <- density(x=y_seq)
	x_scaled <- x + y_dens$y * 10
	abline(v=x, col="lightgray")
	polygon(x=c(x, x_scaled, x), y=c(min(y_dens$x), y_dens$x, max(y_dens$x)),col="lightblue", border=NA)
	lines(x=x_scaled, y=y_dens$x, col="steelblue", lwd=2)
}

dev.off()
