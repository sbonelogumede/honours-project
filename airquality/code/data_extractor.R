# Import the required packages.
require(package = "dplyr")
require(package = "readxl")

extracted_data <- list(NA, NA) # Create storage.
year <- c(2019, 2020) # Years of interest.

# Import the data.
for(i in 1:2){ 
	file_name <- c(
		paste(year[i], "NO2_CCT.xls", sep = "_"),
		paste(year[i], "PM10_CCT.xls", sep = "_"),
		paste(year[i], "SO2_CCT.xls", sep = "_"),
		paste("Wind_direction_and_speed_", year[i], ".xlsx", sep = ""))
	
	# Cells of interest within the dataset(s). 
	cell_range <- matrix(
		data = c(c("E6:E8742", "E6:E8742", "H6:H8742", "O6:O8742"),
				 c("G6:G8742", "G6:G8742", "I6:I8742", "O6:O8742")), 
		nrow = 2, ncol = 4, byrow = TRUE)
	
	# Initialize the new dataset with the date-time object.
	outer_temp <- read_excel(path = paste("../data", file_name[1], sep = "/"),
									 range = "A6:A8742",
									 col_names = FALSE,
									 col_types = "text")
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
	
	# Store the integrated dataset.
	colnames(outer_temp) <- c("DateTime", "NO2", "PM10", "SO2", "Speed")
	extracted_data[[i]] <- outer_temp
	outer_temp <- NA
}

save(extracted_data, file = "../objects/extracted_data_storage.RData")
