util <- new.env()

par(family="CMU Serif", las=1, bty="l", cex.axis=1, cex.lab=1, cex.main=1,
	 xaxs="i", yaxs="i", mar = c(5, 5, 3, 5))

source('gp_utility.R', local=util)

util$plot_gp_post_realizations(pred_opt_fit, data, truth, "Posterior Realizations")
util$plot_gp_post_quantiles(pred_opt_fit, data, truth, "Posterior Marginal Quantiles")
