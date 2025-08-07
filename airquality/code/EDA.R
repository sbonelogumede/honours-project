# Clear the environment.
rm(list = ls())

# Declare important variables.
required_package <- c("bayesplot", "cmdstanr", "colormap", "corrplot", "dplyr", 
                      "ggplot2", "readxl", "rstan", "parallel", "zoo")
var_name <- c("DateTime", "NO2", "PM10", "SO2", "Direction", "Speed")
x_name <- c("DateTime", expression(NO[2]), expression(PM[10]), 
            expression(SO[2]), "Direction", "Speed")
color_name <- c("lightseagreen", "steelblue", "palegreen", "plum3",
                "lightsalmon", "khaki2")

# Attach the required packages.
for (package in required_package) {
   if (!requireNamespace(package = package, quietly = TRUE)) {
      install.packages(pkgs = package, quiet = TRUE)
   }
   require(package = package, character.only = TRUE)
}

data_storage <- list(NA, NA)
year <- c(2019, 2020)
seed_num <- 494838
core_num <- detectCores() - 2

# Import the data.
for(i in 1:2){ 
   file_name <- c(
      paste(year[i], "NO2_CCT.xls", sep="_"),
      paste(year[i], "PM10_CCT.xls", sep="_"),
      paste(year[i], "SO2_CCT.xls", sep="_"),
      paste("Wind_direction_and_speed_", year[i], ".xlsx", sep=""),
      paste("Wind_direction_and_speed_", year[i], ".xlsx", sep="")
   )
   
   cell_range <- matrix(
      data = c(c("E6:E8742", "E6:E8742", "H6:H8742", "N6:N8742", "O6:O8742"),
               c("G6:G8742", "G6:G8742", "I6:I8742", "N6:N8742", "O6:O8742")), 
      nrow = 2, ncol = 5, byrow = TRUE)
   
   outer_temp <- read_excel(path = paste("../data", file_name[1], sep = "/"),
                            range = "A6:A8742",
                            col_names = FALSE) %>%
      pull(1) %>%
      as.POSIXct(format = "%d/%m/%Y %H:%M")
   
   for (j in 1:5) {
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
   colnames(outer_temp) <- var_name
   data_storage[[i]] <- outer_temp
   outer_temp <- NA
}

# Create train and test splits.
data <- rbind(data_storage[[1]], data_storage[[2]])
train <- data_storage[[1]]
test <- data_storage[[2]]

# Generate histogram plots.
for(i in 2:6){
   x <- train[[var_name[i]]]
   filename <- paste0("../images/", tolower(x = var_name[i]), "_hist_2019.png")
   png(filename = filename, width = 8, height = 6, res = 600, units = "in")
   
   hist(x = x, col = color_name[i], main = "", xlab = x_name[i], freq = F)
   abline(v = mean(x = x, na.rm = TRUE), col = "red", lwd = 3)
   abline(v = median(x = x, na.rm = TRUE), col = "black", lwd = 3)
   legend(x = "topright", legend = c("mean" , "median"), col = c("red", "black"), lwd = 3)
   
   dev.off()
}

# Calculate the pairwise correlation between variables.
cor_matrix <- train %>% select(-DateTime) %>% cor(use = "na.or.complete", method = "pearson")

png(filename = "../images/corrplot_2019.png", width = 8, height = 6, res = 600, units = "in")
corrplot(corr = cor_matrix, method = "number", type = "lower")
dev.off()

no2_ma <- rollmean(x = train$NO2, k = 718, na.rm = TRUE, fill = NA)

png(filename = "../images/no2_scatter_2019.png", width = 8, height = 6, res = 600, units = "in")
plot(x = train$DateTime, 
     y = train$NO2, 
     main = "", 
     xlab = "DateTime", 
     ylab = x_name[2], 
     col = color_name[2], 
     pch = 18)
lines(x = train$DateTime, y = no2_ma, col = "hotpink", lwd = 3)
legend(x = "topright", legend = c("MA718"), col = c("hotpink"), lwd = 3)
dev.off()

rstan_options(auto_write = T)
options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

util <- new.env()

n <- nrow(x = train)
x <- 1 : n
alpha <- 3
rho <- 5.5

simu_data = list(alpha = alpha, rho = rho, n = n, x = x)

writeLines(readLines("simu.stan"))

simu_fit <- stan(file = "simu.stan", 
                 data = simu_data, 
                 warmup = 0,
                 seed = seed_num, 
                 algorithm = "Fixed_param",
                 cores = core_num,
                 verbose = TRUE,
                 refresh = 500)

source("gp_utility.R", local = util)

util$plot_gp_prior_realizations(simu_fit, x, "Realizations")

opt_fit <- optimizing(gp_opt, data = train, seed = seed_num, hessian = F)

alpha <- opt_fit$par[2]
rho <- opt_fit$par[1]
sigma <- opt_fit$par[3]
