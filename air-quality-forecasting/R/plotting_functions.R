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
      geom_line(mapping = aes(x = X, y = Y, color = "observed"), alpha = 0.3) +
      labs(
        title = NULL,
        x = "Time",
        y = x_name[i],
        color = "Series"
      ) +
      scale_color_manual(values = c("observed" = "navy")) +
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
      color = "navy",
      alpha = 0.3
    )),
    diag = list(continuous = wrap(
      "densityDiag",
      color = "navy",
      fill = "navy",
      alpha = 0.3
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

#' Create daily curve plots showing hourly patterns for each variable
#'
#' @param X Data frame with DateTime column and measurement variables
#' @param name Prefix for output file names
#' @param output_dir Directory to save plots
#' @export
daily_curves_plot <- function(X, name, output_dir = "output/figures") {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Load required packages
  require(dplyr)
  require(lubridate)
  
  # Extract hour and date from DateTime
  X$Hour <- lubridate::hour(X$DateTime)
  X$Date <- as.Date(X$DateTime)
  
  # Variables to plot (excluding DateTime, Hour, Date)
  variables <- setdiff(colnames(X), c("DateTime", "Hour", "Date"))
  
  # Create a plot for each variable
  for (var in variables) {
    # Prepare data for plotting
    plot_data <- X[, c("Date", "Hour", var)]
    colnames(plot_data) <- c("Date", "Hour", "Value")
    
    # Calculate mean curve for overlay
    mean_curve <- plot_data %>%
      group_by(Hour) %>%
      summarise(MeanValue = mean(Value, na.rm = TRUE), .groups = "drop")
    
    # Get variable label
    var_idx <- which(var_name == var)
    var_label <- if (length(var_idx) > 0) x_name[var_idx] else var
    
    # Create plot with individual daily curves
    p <- ggplot(plot_data, aes(x = Hour, y = Value, group = Date)) +
      geom_line(alpha = 0.3, color = "navy", linewidth = 0.5) +
      scale_x_continuous(breaks = seq(0, 23, by = 3)) +
      labs(
        title = NULL,
        x = "Time",
        y = var_label
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        panel.grid.minor = element_blank()
      )
    
    # Save plot
    filename <- file.path(
      output_dir,
      paste0(name, "_daily_curves_", tolower(var), ".pdf")
    )
    
    ggsave(
      filename = filename,
      plot = p,
      dpi = 600,
      width = 10,
      height = 6,
      units = "in"
    )
    
    message(sprintf("  Saved: %s", basename(filename)))
  }
  
  message(sprintf("Daily curve plots complete for %d variables", length(variables)))
}
