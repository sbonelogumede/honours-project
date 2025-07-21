# Clear the environment.
rm(list = ls())

# Attach the required packages.
required_packages <- c("corrplot", "dplyr", "readxl", "tidyverse")
for (package in required_packages) {
   if (!requireNamespace(package = package, quietly = TRUE)) {
      install.packages(pkgs = package, quiet = TRUE)
   }
   library(package = package, character.only = TRUE)
}

# Import the data.
file_names <- c(
   "2019_NO2_CCT.xls",
   "2019_PM10_CCT.xls",
   "2019_SO2_CCT.xls",
   "Wind_direction_and_speed_2019.xlsx",
   "Wind_direction_and_speed_2019.xlsx"
)
cell_ranges <- c("E6:E8742", "E6:E8742", "H6:H8742", "N6:N8742", "O6:O8742")
dat <- read_excel(
   path = paste("..", "data", file_names[1], sep = "/"),
   range = "A6:A8742",
   col_names = FALSE
) %>%
   pull(1) %>%
   as.POSIXct(format = "%d/%m/%Y %H:%M")

for (i in 1:5) {
   dat <- cbind(dat, read_excel(
      path = paste("..", "data", file_names[i], sep = "/"),
      range = cell_ranges[i],
      col_names = FALSE,
      col_types = "numeric",
      na = c("Down", "InVld", "NoData", "Zero", "<Samp")
   ))
}
colnames(dat) <- c("DateTime", "NO2", "PM10", "SO2", "Direction", "Speed")
head(dat)

cor_matrix <- dat %>%
   select(-DateTime) %>%
   cor(use = "na.or.complete", method = "pearson")
corrplot(corr = cor_matrix, method = "color", type = "lower")

plot(x = dat$NO2, main = "", xlab = "Date", ylab = "NO2", col = "steelblue", pch = 18)
hist(x = dat$NO2, main = "", xlab = "NO2", col = "steelblue")
