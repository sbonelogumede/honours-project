require(package="corrplot")
require(package="dplyr")
require(package="ggplot2")
require(package="zoo")

load(file="../object/extracted_data_storage.RData")
load(file="../object/transformed_data_storage.RData")

extracted_train_data <- rbind(extracted_data[[1]], extracted_data[[2]])
transformed_train_data <- rbind(transformed_data[[1]], transformed_data[[2]])

var_name <- c("DateTime", "NO2", "PM10", "SO2", "Speed")
x_name <- c("DateTime", 
            expression(NO[2] ~ "(" * mu * "g/m"^3 * ")"), 
            expression(PM[10] ~ "(" * mu * "g/m"^3 * ")"),
            expression(SO[2] ~ "(" * mu * "g/m"^3 * ")"), 
            expression("Wind Speed" ~ "(m/s)"))

# Summary statistics.
S_mat <- rbind("Min."=sapply(X=extracted_train_data, FUN=min, na.rm=TRUE),
               "1st Qu."=sapply(X=extracted_train_data, FUN=quantile, 0.25, na.rm=TRUE),
               "Median"=sapply(X=extracted_train_data, FUN=median, na.rm=TRUE),
               "Mean"=sapply(X=extracted_train_data, FUN=mean, na.rm=TRUE),
               "Std."=sapply(X=extracted_train_data, FUN=sd, na.rm=TRUE),
               "3rd Qu."=sapply(X=extracted_train_data, FUN=quantile, 0.75, na.rm=TRUE),
               "Max."=sapply(X=extracted_train_data, FUN=max, na.rm=TRUE),
               "NA's"=sapply(X=extracted_train_data, FUN=function(column) sum(is.na(column))))
summary_statistics <- round(x=t(x=S_mat[, 2:5]), digits=2)
print(x=summary_statistics)

# Scatter plots.
for(i in 1:2){
   for(j in 2:5){
      if(i == 1){
         x <- extracted_train_data[[var_name[j]]]
         type <- "extracted"
      } else {
         x <- transformed_train_data[[var_name[j]]]
         type <- "transformed"
      }
      
      filename <- paste0("../img/", type, "_", tolower(x=var_name[j]), ".png")
      png(filename=filename, width=6.5, height=4, res=300, units="in")
      plot(x=extracted_train_data$DateTime, y=x, main="", xlab="Time", ylab=x_name[j],
           type="l", col="steelblue", cex=0.5)
      dev.off()
   }
}

# Correlation plot.
png(filename="../img/corrplot.png", width=6.5, height=4, res=300, units="in")
cor_mat <- cor(transformed_train_data[, 2:5], use="na.or.complete", method="pearson")
corrplot(corr=cor_mat, method="number", type="lower")
dev.off()
