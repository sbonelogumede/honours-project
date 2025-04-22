packs <- c('colormap', 'extrafont', 'rstan')
for(p in packs){
	if(!requireNamespace(p, quietly=TRUE)){
		install.packages(p, dependencies=TRUE)
	}
	library(p, character.only=TRUE)
}

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy='sequential')

util <- new.env()

par(family='CMU Serif', las=1, bty='l', cex.axis=1, cex.lab=1, cex.main=1, xaxs='i', yaxs='i', mar=c(5, 5, 3, 5))

# Now we can define the covariate grid

N = 551
x <- 22 * (0:(N-1)) / (N-1) - 11

# the parameters that specify our particular Gaussian process

alpha_true <- 3
rho_true <- 5.5

# and then pack everything together,

simu_data <- list(alpha=alpha_true, rho=rho_true, N=N, x=x)

# In order to sample from the Gaussian Process projected onto our covariate grid, we just need to construct the Gram matrix and then sample from the corresponding multivariate normal random number generator

writeLines(readLines('stan_programs/simu.stan'))

# Here we use Stan's builtin cov_exp_quad function for evaluating the Gram matrix and then add a nugget or jitter of 10^{-10} to the diagonal elements in order to stabilize the subsequent numeral linear algebra calculations. Often the nugget is taking to be square root of the floating point precision, which would be 10^{-8} for double precision calculations, but for this particular example we can be a bit more aggressive.

# We can generate exact samples by running Fixed_param sampler in Stan, which evaluates the generated quantiles block that calls the multivariate normal random number generator

simu_fit <- stan(file='stan_programs/simu.stan', data = simu_data,
					  warmup=0, iter=4000, chains=1, seed=494838, algorithm='Fixed_param', refresh=4000)

# To visualize the sampled function we'll use some visualization code in the auxiliary gp_utility.R script.

source('gp_utility.R', local=util)

# For example we can overlay some of the functions in a spaghetti plot,
util$plot_gp_prior_realizations(simu_fit, x, 'Realisations')

# or plot marginal quantiles to summarize the aggregate behavior
util$plot_gp_prior_quantiles(simu_fit, x, 'Marginal Quantiles')


# f ~ GP(0, k)
# yn ~ N(f(xn), s)

writeLines(readLines('stan_programs/simu_normal.stan'))

sigma_true <- 2
simu_data$sigma <- sigma_true

set.seed(2595)
simu_fit <- stan(file='stan_programs/simu_normal.stan', data=simu_data, warmup=0, 
					  iter=4000, chains=1, seed=494838, algorithm='Fixed_param', refresh=4000)

util$plot_gp_prior_realizations(simu_fit, x, 'Prior Realizations')

util$plot_gp_prior_quantiles(simu_fit, x, 'Prior Marginal Quantiles')

util$plot_gp_prior_pred_quantiles(simu_fit, x, 'Prior Predictive Marginal Quantiles')

# Let's grab the first simulation for closer inspection

f <- extract(simu_fit)$f[1, ]
y <- extract(simu_fit)$y[1, ]

c_mid_teal='#487575'

plot(x, f, type='l', lwd=2, xlab='x', ylab='y', xlim=c(-11, 11),
	  ylim=c(-10, 10))
points(x, y, col='white', pch=16, cex=0.6)
points(x, y, col=c_mid_teal, pch=16, cex=0.4)

