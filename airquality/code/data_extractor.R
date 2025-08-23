require(package="dplyr")
require(package="readxl")

raw_data <- list(NA, NA, NA)
scaled_data <- list(NA, NA, NA)
year <- c(2018, 2019, 2020)

# Import the data.
for(i in 1:3){ 
	if(i==1){
		file_name <- rep("TableView2018.xls", 4)
		range <- "A5:A8764"
	} else {
		file_name <- c(
			paste(year[i], "NO2_CCT.xls", sep="_"),
			paste(year[i], "PM10_CCT.xls", sep="_"),
			paste(year[i], "SO2_CCT.xls", sep="_"),
			paste("Wind_direction_and_speed_", year[i], ".xlsx", sep=""))
		range <- "A6:A8742"
	}
	cell_range <- matrix(
		data=c(c("B5:B8764", "D5:D8764", "C5:C8764", "F5:F8764"),
				 c("E6:E8742", "E6:E8742", "H6:H8742", "O6:O8742"),
				 c("G6:G8742", "G6:G8742", "I6:I8742", "O6:O8742")), 
		nrow=3, ncol=4, byrow=T)
	
	outer_temp <- read_excel(path=paste("../data", file_name[1], sep="/"),
									 range=range,
									 col_names=F) %>%
		pull(1) %>%
		as.POSIXct(format="%d/%m/%Y %H:%M")
	
	raw_temp <- outer_temp
	scaled_temp <- outer_temp
	
	for(j in 1:4){
		# Read the column of interest from the data.
		inner_temp <- read_excel(path=paste("../data", file_name[j], sep="/"),
										 range=cell_range[i, j],
										 col_names=F,
										 col_types="text")
		
		# Handle the case where x=0 gracefully.
		inner_temp <- inner_temp %>%
			mutate(across(everything(), ~ case_when(
				.x %in% c("Down", "InVld", "NoData", "<Samp") ~ NA_real_,
				.x == "Zero" ~ 0,
				TRUE ~ as.numeric(.x)
			)))

		# Scale the column.
		extract_temp <- inner_temp[[1]]
		mean_ <- mean(x=extract_temp, na.rm=TRUE)
		sd_ <- sd(x=extract_temp, na.rm=TRUE)
		scale_temp <- sapply(X=extract_temp, FUN=function(entry){
			ifelse(test=is.na(entry), yes=NA, no=(entry - mean_)/sd_)
		})
		scale_temp <- tibble(scale_temp) # Convert to tibble object.
		raw_temp <- cbind(raw_temp, inner_temp)
		scaled_temp <- cbind(scaled_temp, scale_temp)
	}
	colnames(raw_temp) <- c("DateTime", "NO2", "PM10", "SO2", "Speed")
	colnames(scaled_temp) <- c("DateTime", "NO2", "PM10", "SO2", "Speed")
	
	raw_data[[i]] <- raw_temp
	scaled_data[[i]] <- scaled_temp
	
	raw_temp <- NA
	scaled_temp <- NA
}

save(raw_data, file="../object/raw_data.RData")
save(scaled_data, file="../object/scaled_data.RData")
