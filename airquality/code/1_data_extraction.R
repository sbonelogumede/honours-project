# Attach the required packages.
require(package = "dplyr")
require(package = "readxl")

# Import the data.
file_name <- c("2019_NO2_CCT.xls", "2019_PM10_CCT.xls", "2019_SO2_CCT.xls",
					"Wind_direction_and_speed_2019.xlsx")

# Cells of interest within the dataset(s). 
cell_range <- c("E6:E8742", "E6:E8742", "H6:H8742", "O6:O8742")

# Initialize the new dataset with the date-time object.
F_mat <- read_excel(path = paste("../data", file_name[1], sep = "/"),
						  range = "A6:A8742", col_names = FALSE) |>
	pull(var = 1) |> 
	as.POSIXct(format = "%d/%m/%Y %H:%M")

for(j in 1:4){
	# Read the column of interest from the data.
	tmp <- read_excel(path = paste("../data", file_name[j], sep = "/"),
							 range = cell_range[j],
							 col_names = FALSE,
							 col_types = "text")
	
	# Handle the case where x=0 gracefully.
	tmp <- tmp %>%
		mutate(across(everything(), ~ case_when(
			.x %in% c("Down", "InVld", "NoData", "<Samp") ~ NA_real_,
			.x == "Zero" ~ 0,
			TRUE ~ as.numeric(.x)
		)))
	
	# Add the current column to the new dataset.
	F_mat <- cbind(F_mat, tmp)
}

# Store the integrated dataset.
colnames(F_mat) <- c("DateTime", "NO2", "PM10", "SO2", "Speed")

# Save the data into an object.
save(F_mat, file = "../objects/extracted_data_storage.RData")
