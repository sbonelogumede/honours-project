rm(list=ls())

requiredPackages <- c("dplyr", "forecast", "fpp3", "knitr", "ggthemes", "ggplot2")
for(package in requiredPackages){
	if(!requireNamespace(package=package, quietly=TRUE)){
		install.packages(pkgs=package, quiet=TRUE)
	}
	library(package=package, character.only=TRUE)
}

monthlyData <- read.csv(file="../data/KBP5356M.csv")
dataFrame <- monthlyData %>% mutate(Date=ym(Date)) %>% select(Date, Value)
standardDeviation <- dataFrame %>% pull(Value) %>% sd()
dataFrame <- dataFrame %>% mutate(
	Lower=Value-standardDeviation, 
	Upper=Value+standardDeviation,
	MA12=stats::filter(Value, rep(1/12, 12), sides=2))
dataFrame %>% head() %>% kable()

ggplot(data=dataFrame)+
	geom_histogram(binwidth=1000, fill="black", mapping=aes(x=Value, y=..density..))+
	labs(title="Histogram: Gold Price", x="Gold Price")+
	theme_minimal()
ggsave(filename="../images/histogram.png", dpi=600, width=10, height=6, units="in")

ggplot(data=dataFrame, mapping=aes(x=Date, y=Value))+
	geom_ribbon(aes(ymin=Lower, ymax=Upper), fill="pink", alpha=0.5)+
	geom_point(alpha=1, color="black", shape=18, size=1)+
	geom_line(mapping=aes(x=Date, y=MA12), color="red", linewidth=1)+
	labs(title="Scatterplot: Time vs Gold Price", x="Time", y="Gold Price")+
	theme_minimal()
ggsave(filename="../images/scatter-plot.png", dpi=600, width=10, height=6, units="in")
# Components of the time series:
# Increasing non-linear trend.
# Random variation.

