# Import libraries
library(ggplot2)
library(readxl)

# Read data
dat <- read_xlsx(
	path = "../../data/Wind_direction_and_speed_2019.xlsx", 
	skip = 4
)

# Declare variables
dat <- head(dat, -8)
x_seq <- 1:nrow(dat)

# Convert non-numeric entries to NA.
for (i in 2:ncol(dat)) {
	dat[[i]] <- as.numeric(dat[[i]])
}

# Somerset wind direction is on column 12.
ggplot() +
	geom_histogram(
		mapping = aes(x = dat[[12]]), 
		color = "red",
		fill="lightcoral"
	) +
	labs(
		title = "Histogram of Someset wind direction", 
		x = "Degree", 
		y = "Frequency"
	) +
	theme_minimal()

# Somerset wind velocity is on column 13.
ggplot() +
	geom_histogram(
		mapping = aes(x = dat[[13]]), 
		color = "green",
		fill="lightseagreen"
	) +
	labs(
		title = "Histogram of Someset wind speed", 
		x = "Velocity", 
		y = "Frequency"
	) +
	theme_minimal()

# Plot time series
ggplot() +
	geom_line(
		mapping = aes(x = x_seq, y = (dat[[12]])/1), 
		color = "lightcoral"
	) +
	geom_line(
		mapping = aes(x = x_seq, y=(dat[[13]])/1), 
		color = "lightseagreen"
	) +
	labs(
		title = "Someset wind direction and speed plot", 
		x = "Time", 
		y = "f",
	) +
	theme_minimal()
