load(file = "../object/extracted_data_storage.RData")

transformed_data <- list()
n <- length(x = extracted_data)

for(i in 1:n){
	dataset <- extracted_data[[i]] # Data for year i, for i in {2018, 2019, 2020}.
	m <- ncol(x = dataset) # Number of variables.
	outer_temp <- dataset[1]
	
	for(j in 2:m){
		column <- dataset[[j]] # Get the column.
		
		column[column==0] <- rgamma(n=1, shape=2) # Add Gaussian noise.
		
		column_log <- sapply(X = column, FUN = function(entry){
			# Log transform the column.
			ifelse(test = is.na(entry), yes = NA, no = log(x = entry))
		})
		
		mean_log <- mean(x = column_log, na.rm = TRUE)
		sd_log <- sd(x = column_log, na.rm = TRUE)
		
		inner_temp <- sapply(X = column_log, FUN = function(entry_log){
			# Scale the log-transformed column.
			ifelse(test = is.na(entry_log), yes = NA, no = (entry_log - mean_log)/sd_log)
		})
		
		outer_temp <- cbind(outer_temp, inner_temp) # Store the processed column.
	}
	
	colnames(outer_temp) <- c("DateTime", "NO2", "PM10", "SO2", "Speed")
	transformed_data[[i]] <- outer_temp
	outer_temp <- NA
}

save(transformed_data, file="../object/transformed_data_storage.RData")
