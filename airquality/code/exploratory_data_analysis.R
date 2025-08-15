require(package="corrplot")
require(package="dplyr")
require(package="ggplot2")
require(package="zoo")

load(file="../object/data_storage.RData")
train <- data_storage[[1]]

var_name <- c("DateTime", "NO2", "PM10", "SO2", "Speed")
x_name <- c("DateTime", expression(NO[2]), expression(PM[10]), expression(SO[2]), expression(Speed))
color_name <- c("lightskyblue", "lightseagreen", "palegreen", "plum3", "lightsalmon", "khaki2")

# Summary statistics.
summary_table <- rbind("Min."=sapply(X=train, FUN=min, na.rm=TRUE),
                       "1st Qu."=sapply(X=train, FUN=quantile, 0.25, na.rm=TRUE),
                       "Median"=sapply(X=train, FUN=median, na.rm=TRUE),
                       "Mean"=sapply(X=train, FUN=mean, na.rm=TRUE),
                       "Std."=sapply(X=train, FUN=sd, na.rm=TRUE),
                       "3rd Qu."=sapply(X=train, FUN=quantile, 0.75, na.rm=TRUE),
                       "Max."=sapply(X=train, FUN=max, na.rm=TRUE),
                       "NA's"=sapply(X=train, FUN=function(column) sum(is.na(column))))
summary_statistics <- round(x=t(x=summary_table[, 2:5]), digits=2)
summary_statistics

# Correlation plot.
png(filename="../img/corrplot_2019.png", width=8, height=6, res=600, units="in")
cor_matrix <- train %>% select(-DateTime) %>% cor(use="na.or.complete", method="pearson")
corrplot(corr=cor_matrix, method="number", type="lower")
dev.off()

for(i in 2:5){
   x <- train[[var_name[i]]]
   
   main_hist <- bquote(.("Histogram of") ~ .(x_name[[i]]))
   main_scatter <- bquote(.("Scatter plot of") ~ .(x_name[[i]]))
   
   filename1 <- paste0("../img/", tolower(x=var_name[i]), "_hist_2019.png")
   filename2 <- paste0("../img/", tolower(x=var_name[i]), "_scatter_2019.png")
   
   # Histogram plot.
   png(filename=filename1, width=8, height=6, res=600, units="in")
   hist(x=x, col=color_name[i-1], main=main_hist, xlab=x_name[i], breaks=30, freq=FALSE)
   abline(v=mean(x=x, na.rm=TRUE), col="red", lwd=3)
   abline(v=median(x=x, na.rm=TRUE), col="black", lwd=3)
   abline(v=mean(x=x, na.rm=TRUE) - sd(x=x, na.rm=T), col="hotpink", lwd=3)
   abline(v=mean(x=x, na.rm=TRUE) + sd(x=x, na.rm=T), col="hotpink", lwd=3)
   legend(x="topright", legend=c("Mean" , "Median", "Standard deviation"), 
          col=c("red", "black", "hotpink"), lwd=3)
   dev.off()
   
   # Scatter plot.
   png(filename=filename2, width=8, height=6, res=600, units="in")
   ma <- rollmean(x=x, k=718, na.rm=TRUE, fill=NA)
   plot(x=train$DateTime, y=x, main=main_scatter, xlab="Time", ylab=x_name[i], 
        col=color_name[i-1], type="p", cex=0.5, pch=18)
   lines(x=train$DateTime, y=ma, col="steelblue", lwd=3)
   legend(x="topright", legend=c("MA718"), col=c("steelblue"), lwd=3)
   dev.off()
}
