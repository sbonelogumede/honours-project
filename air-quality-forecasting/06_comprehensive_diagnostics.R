# ==============================================================================
# Comprehensive GP Diagnostics: Trace, Density, and Residuals
# Purpose: Generate diagnostic plots for GP models (SE, PER, SExPER)
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(posterior)
  library(dplyr)
  library(tidyr)
  library(qqconf)
})

cat("Starting comprehensive diagnostics...\n\n")

# Configuration ----------------------------------------------------------------
data_dir <- "data"
results_dir <- "output/results"
fig_dir <- "output/figures"

# Ensure output directories exist
if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

# -----------------------------------------------------------------------------
# Function to Create All Diagnostics for One Model
# -----------------------------------------------------------------------------

create_diagnostics <- function(model_file, model_name, fold_num = 1) {
  
  cat(rep("=", 70), "\n", sep = "")
  cat(sprintf("Processing %s Model\n", toupper(model_name)))
  cat(rep("=", 70), "\n\n", sep = "")
  
  # Load model from results directory
  model_path <- file.path(results_dir, model_file)
  env <- new.env()
  load(model_path, envir = env)
  
  # Extract appropriate draws based on model
  if (model_name == "SE") {
    draws_list <- env$SE_draws
    param_names <- c("alpha", "sigma", "lp__")
    all_vars <- variables(draws_list[[fold_num]])
    rho_vars <- all_vars[grepl("^rho\\[", all_vars)]
    beta_vars <- all_vars[grepl("^beta\\[", all_vars)]
    param_names <- c(param_names, rho_vars, beta_vars)
    
  } else if (model_name == "PER") {
    draws_list <- env$PER_draws
    param_names <- c("alpha", "rho", "sigma", "lp__")
    all_vars <- variables(draws_list[[fold_num]])
    beta_vars <- all_vars[grepl("^beta\\[", all_vars)]
    param_names <- c(param_names, beta_vars)
    
  } else if (model_name == "SExPER") {
    draws_list <- env$SExPER_draws
    param_names <- c("alpha_se", "alpha_per", "rho_per", "sigma", "lp__")
    all_vars <- variables(draws_list[[fold_num]])
    rho_se_vars <- all_vars[grepl("^rho_se\\[", all_vars)]
    beta_vars <- all_vars[grepl("^beta\\[", all_vars)]
    param_names <- c(param_names, rho_se_vars, beta_vars)
  }
  
  cat(sprintf("Parameters: %s\n\n", paste(param_names, collapse=", ")))
  
  # Extract draws
  draws_obj <- draws_list[[fold_num]]
  draws_subset <- subset_draws(draws_obj, variable = param_names)
  draws_arr <- as_draws_array(draws_subset)
  
  n_iter <- dim(draws_arr)[1]
  n_chains <- dim(draws_arr)[2]
  n_params <- length(param_names)
  
  cat(sprintf("Draws: %d iterations × %d chains × %d parameters\n\n", 
              n_iter, n_chains, n_params))
  
  # ---------------------------
  # 1. TRACE PLOTS
  # ---------------------------
  cat("Creating trace plots...\n")
  
  # Create long-format data for all parameters
  trace_data <- list()
  for (i in 1:n_params) {
    param <- param_names[i]
    trace_data[[i]] <- data.frame(
      iteration = rep(1:n_iter, n_chains),
      chain = factor(rep(1:n_chains, each = n_iter)),
      value = as.vector(draws_arr[,,i]),
      parameter = param,
      stringsAsFactors = FALSE
    )
  }
  trace_df <- bind_rows(trace_data)
  
  p_trace <- ggplot(trace_df, aes(x = iteration, y = value, color = chain)) +
    geom_line(linewidth = 0.3, alpha = 0.7) +
    facet_wrap(~ parameter, scales = "free_y", ncol = 3) +
    scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")) +
    labs(
      title = sprintf("%s Model - Trace Plots", toupper(model_name)),
      x = "Iteration",
      y = "Value",
      color = "Chain"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      strip.background = element_rect(fill = "gray90"),
      legend.position = "bottom"
    )
  
  ggsave(
    file.path(fig_dir, sprintf("trace_%s.pdf", model_name)), 
    plot = p_trace, 
    width = 14, 
    height = ceiling(n_params/3) * 2.5
  )
  cat(sprintf("Saved: %s\n", file.path(fig_dir, sprintf("trace_%s.pdf", model_name))))
  
  # ---------------------------
  # 2. DENSITY PLOTS
  # ---------------------------
  cat("Creating density plots...\n")
  
  p_density <- ggplot(trace_df, aes(x = value, fill = chain)) +
    geom_density(alpha = 0.4, linewidth = 0.5) +
    facet_wrap(~ parameter, scales = "free", ncol = 3) +
    scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")) +
    labs(
      title = sprintf("%s Model - Posterior Densities", toupper(model_name)),
      x = "Value",
      y = "Density",
      fill = "Chain"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      strip.background = element_rect(fill = "gray90"),
      legend.position = "bottom"
    )
  
  ggsave(
    file.path(fig_dir, sprintf("density_%s.pdf", model_name)), 
    plot = p_density, 
    width = 14, 
    height = ceiling(n_params/3) * 2.5
  )
  cat(sprintf("Saved: %s\n", file.path(fig_dir, sprintf("density_%s.pdf", model_name))))
  
  # ---------------------------
  # 3. PARAMETER SUMMARY
  # ---------------------------
  cat("Computing parameter summary...\n")
  
  summary_df <- summarise_draws(draws_subset)
  print(summary_df)
  
  # ---------------------------
  # 4. RESIDUAL DIAGNOSTICS
  # ---------------------------
  cat("Creating residual diagnostics...\n")
  
  # Load transformed data from data directory
  load(file.path(data_dir, "transformed_data.RData"))
  
  X2_fold <- window(
    x = X_mat, 
    start = start(X2) + c(fold_num-1, 0), 
    end = end(X2) + c(fold_num-1, 0)
  )
  Y2_fold <- window(
    x = Y_vec, 
    start = start(Y2) + c(fold_num-1, 0), 
    end = end(Y2) + c(fold_num-1, 0)
  )
  
  # Get y2 predictions
  y2_vars <- variables(draws_obj)[grepl("^y2\\[", variables(draws_obj))]
  y2_draws <- subset_draws(draws_obj, variable = y2_vars)
  y2_mean <- colMeans(as_draws_matrix(y2_draws))
  
  # Calculate residuals
  residuals <- as.numeric(Y2_fold) - y2_mean
  fitted <- y2_mean
  observations <- as.numeric(Y2_fold)
  time_idx <- time(Y2_fold)
  
  res_df <- data.frame(
    time = time_idx,
    residuals = residuals,
    fitted = fitted,
    observations = observations
  )
  
  # 4a. Residuals vs Fitted
  p1 <- ggplot(res_df, aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(se = FALSE, color = "blue", linewidth = 0.8) +
    labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") +
    theme_minimal(base_size = 9)
  
  # 4b. Residuals over time
  p2 <- ggplot(res_df, aes(x = time, y = residuals)) +
    geom_point(alpha = 0.5, size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(se = FALSE, color = "blue", linewidth = 0.8) +
    labs(title = "Residuals over Time", x = "Time", y = "Residuals") +
    theme_minimal(base_size = 9)
  
  # 4c. Q-Q plot with confidence bands using qqconf
  cat("Creating QQ plot with confidence bands...\n")
  
  # Note: qq_conf_plot will be created directly when saving (uses base R graphics)
  
  
  # 4d. Histogram
  p4 <- ggplot(res_df, aes(x = residuals)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, 
                   fill = "steelblue", alpha = 0.7) +
    geom_density(color = "red", linewidth = 1) +
    labs(title = "Distribution of Residuals", x = "Residuals", y = "Density") +
    theme_minimal(base_size = 9)
  
  # 4e. Observed vs Predicted
  p5 <- ggplot(res_df, aes(x = observations, y = fitted)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Observed vs Predicted", x = "Observed", y = "Predicted") +
    theme_minimal(base_size = 9)
  
  # 4f. ACF
  acf_vals <- acf(residuals, plot = FALSE, lag.max = 50)
  acf_df <- data.frame(lag = acf_vals$lag, acf = acf_vals$acf)
  
  p6 <- ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(
      yintercept = c(-1.96, 1.96)/sqrt(length(residuals)),
      linetype = "dashed", 
      color = "blue"
    ) +
    geom_segment(aes(xend = lag, yend = 0), linewidth = 1) +
    labs(title = "ACF of Residuals", x = "Lag", y = "ACF") +
    theme_minimal(base_size = 9)
  
  # Save each residual plot separately
  ggsave(file.path(fig_dir, sprintf("resid_%s_fitted.pdf", model_name)), 
         p1, width = 6, height = 4)
  ggsave(file.path(fig_dir, sprintf("resid_%s_time.pdf", model_name)), 
         p2, width = 6, height = 4)
  
  # QQ plot uses base R graphics, so save it separately
  pdf(file.path(fig_dir, sprintf("resid_%s_qq.pdf", model_name)), 
      width = 6, height = 4)
  qq_conf_plot(
    obs = residuals,
    distribution = qnorm,
    method = "ell",              # Equal local levels (recommended)
    alpha = 0.05,                # 95% confidence level
    points_params = list(
      col = "steelblue",
      pch = 19,                  # Filled circles
      cex = 0.8                  # Point size
    ),
    line_params = list(
      col = "red",
      lwd = 2                    # Line width
    ),
    polygon_params = list(
      border = NA,
      col = rgb(0.5, 0.5, 0.5, 0.3)  # Gray with transparency
    ),
    main = "Normal Q-Q Plot with 95% CI",
    xlab = "Theoretical Quantiles",
    ylab = "Sample Quantiles"
  )
  dev.off()
  
  ggsave(file.path(fig_dir, sprintf("resid_%s_hist.pdf", model_name)), 
         p4, width = 6, height = 4)
  ggsave(file.path(fig_dir, sprintf("resid_%s_obspred.pdf", model_name)), 
         p5, width = 6, height = 4)
  ggsave(file.path(fig_dir, sprintf("resid_%s_acf.pdf", model_name)), 
         p6, width = 6, height = 4)
  
  cat(sprintf("Saved: resid_%s_*.pdf (6 files) in %s\n", model_name, fig_dir))
  
  # Residual statistics
  res_stats <- data.frame(
    metric = c("Mean", "SD", "Min", "Max", "RMSE", "MAE"),
    value = c(
      mean(residuals), 
      sd(residuals), 
      min(residuals), 
      max(residuals), 
      sqrt(mean(residuals^2)), 
      mean(abs(residuals))
    )
  )
  
  cat("\nResidual Statistics:\n")
  print(res_stats)
  
  rm(draws_list, draws_obj, draws_subset, draws_arr, env, trace_df)
  gc()
  
  cat("\n")
  return(invisible(NULL))
}

# -----------------------------------------------------------------------------
# Run Diagnostics
# -----------------------------------------------------------------------------

cat(rep("=", 70), "\n", sep = "")
cat("COMPREHENSIVE DIAGNOSTICS\n")
cat(rep("=", 70), "\n\n", sep = "")

for (model in list(
  list(file = "GP_MLR_SE_IR.RData", name = "SE"),
  list(file = "GP_MLR_PER_IR.RData", name = "PER"),
  list(file = "GP_MLR_SExPER_IR.RData", name = "SExPER")
)) {
  tryCatch({
    create_diagnostics(model$file, model$name)
  }, error = function(e) {
    cat(sprintf("ERROR processing %s: %s\n\n", model$name, e$message))
  })
}

cat(rep("=", 70), "\n", sep = "")
cat("DIAGNOSTICS COMPLETE\n")
cat(rep("=", 70), "\n", sep = "")
cat(sprintf("\nAll diagnostic plots saved to: %s\n", fig_dir))
