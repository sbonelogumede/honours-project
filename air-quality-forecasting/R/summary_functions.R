# ==============================================================================
# Summary Statistics Functions
# Purpose: Reusable functions for computing descriptive statistics
# ==============================================================================

#' Calculate comprehensive summary statistics
#'
#' @param X A data frame or matrix with numeric columns
#' @return A matrix with summary statistics for each column
#' @export
summary_statistics <- function(X) {
  rbind(
    "Min." = sapply(X = X, FUN = min, na.rm = TRUE),
    "1st Qu." = sapply(X = X, FUN = quantile, 0.25, na.rm = TRUE),
    "Median" = sapply(X = X, FUN = median, na.rm = TRUE),
    "Mean" = sapply(X = X, FUN = mean, na.rm = TRUE),
    "SD" = sapply(X = X, FUN = sd, na.rm = TRUE),
    "3rd Qu." = sapply(X = X, FUN = quantile, 0.75, na.rm = TRUE),
    "Max." = sapply(X = X, FUN = max, na.rm = TRUE),
    "#Tot." = sapply(X = X, FUN = function(column) sum(!is.na(column))),
    "#NA." = sapply(X = X, FUN = function(column) sum(is.na(column)))
  )
}
