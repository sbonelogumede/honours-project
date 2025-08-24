require(package = "dplyr")
require(package = "readxl")

extracted_data <- list(NA, NA, NA)
year <- c(2018, 2019, 2020)

# Import the data.
for(i in 1:3){ 
	
	if(i==1){
		file_name <- rep(x = "TableView2018.xls", times = 4)
		range <- "A5:A8764"
	} 
	
	else {
		file_name <- c(
			paste(year[i], "NO2_CCT.xls", sep = "_"),
			paste(year[i], "PM10_CCT.xls", sep = "_"),
			paste(year[i], "SO2_CCT.xls", sep = "_"),
			paste("Wind_direction_and_speed_", year[i], ".xlsx", sep = ""))
		range <- "A6:A8742"
	}
	
	cell_range <- matrix(
		data = c(c("B5:B8764", "D5:D8764", "C5:C8764", "F5:F8764"),
				 c("E6:E8742", "E6:E8742", "H6:H8742", "O6:O8742"),
				 c("G6:G8742", "G6:G8742", "I6:I8742", "O6:O8742")), 
		nrow = 3, ncol = 4, byrow = TRUE)
	
	outer_temp <- read_excel(path = paste("../data", file_name[1], sep = "/"),
									 range = range,
									 col_names = FALSE) %>%
		pull(1) %>%
		as.POSIXct(format = "%d/%m/%Y %H:%M")
	
	for(j in 1:4){
		# Read the column of interest from the data.
		inner_temp <- read_excel(path = paste("../data", file_name[j], sep = "/"),
										 range = cell_range[i, j],
										 col_names = FALSE,
										 col_types = "text")
		
		# Handle the case where x=0 gracefully.
		inner_temp <- inner_temp %>%
			mutate(across(everything(), ~ case_when(
				.x %in% c("Down", "InVld", "NoData", "<Samp") ~ NA_real_,
				.x == "Zero" ~ 0,
				TRUE ~ as.numeric(.x)
			)))
	
		outer_temp <- cbind(outer_temp, inner_temp)
	}
	
	colnames(outer_temp) <- c("DateTime", "NO2", "PM10", "SO2", "Speed")
	extracted_data[[i]] <- outer_temp
	outer_temp <- NA
}

save(extracted_data, file = "../object/extracted_data_storage.RData")
