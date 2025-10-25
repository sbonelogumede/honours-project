# ==============================================================================
# Plotting Functions
# Purpose: Reusable functions for creating visualizations
# ==============================================================================

library(ggplot2)
library(GGally)

# Variable names for labels
var_name <- c("DateTime", "NO2", "PM10", "SO2", "Speed")

x_name <- c(
  "DateTime",
  expression(NO[2] ~ "(" * mu * "g/m"^3 * ")"),
  expression(PM[10] ~ "(" * mu * "g/m"^3 * ")"),
  expression(SO[2] ~ "(" * mu * "g/m"^3 * ")"),
  expression("Wind Speed" ~ "(m/s)")
)

#' Create time series scatter plots for each variable
#'
#' @param X Data frame containing DateTime and measurement variables
#' @param name Prefix for output file names
#' @param output_dir Directory to save plots
#' @export
scatter_plot <- function(X, name, output_dir = "output/figures") {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  for (i in 2:5) {
    p <- ggplot(data = data.frame(
      X = X$DateTime,
      Y = X[[var_name[i]]]
    )) +
      geom_line(mapping = aes(x = X, y = Y, color = "observed")) +
      labs(
        title = NULL,
        x = "Time",
        y = x_name[i],
        color = "Series"
      ) +
      scale_color_manual(values = c("observed" = "lightskyblue")) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    filename <- file.path(
      output_dir,
      paste0(name, "_data_", tolower(var_name[i]), ".pdf")
    )
    
    ggsave(
      filename = filename,
      plot = p,
      dpi = 600,
      width = 10,
      height = 6,
      units = "in"
    )
  }
}

#' Create pairs plot for correlations between variables
#'
#' @param X Data frame containing measurement variables
#' @param name Prefix for output file name
#' @param output_dir Directory to save plot
#' @export
pairs_plot <- function(X, name, output_dir = "output/figures") {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  p <- ggpairs(
    data = X[, 2:5],
    lower = list(continuous = wrap(
      "points",
      color = "lightskyblue",
      alpha = 0.5
    )),
    diag = list(continuous = wrap(
      "densityDiag",
      color = "lightskyblue",
      fill = "lightskyblue",
      alpha = 0.5
    )),
    upper = list(continuous = wrap("cor", color = "black"))
  ) +
    theme_minimal()
  
  filename <- file.path(output_dir, paste0(name, "_data_pairsplot.pdf"))
  
  ggsave(
    filename = filename,
    plot = p,
    dpi = 600,
    width = 10,
    height = 6,
    units = "in"
  )
}
