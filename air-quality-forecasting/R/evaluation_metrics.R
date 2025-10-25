# ==============================================================================
# Evaluation Metrics Functions
# Purpose: Reusable functions for model evaluation
# ==============================================================================

#' Calculate Root Mean Square Error
#'
#' @param yhat Predicted values
#' @param yobs Observed values
#' @return RMSE value
#' @export
rmse <- function(yhat, yobs) {
  sqrt(mean((yhat - yobs)^2))
}

#' Calculate Mean Absolute Error
#'
#' @param yhat Predicted values
#' @param yobs Observed values
#' @return MAE value
#' @export
mae <- function(yhat, yobs) {
  mean(abs(yhat - yobs))
}

#' Calculate Symmetric Mean Absolute Percentage Error
#'
#' @param yhat Predicted values
#' @param yobs Observed values
#' @return SMAPE value (as percentage)
#' @export
smape <- function(yhat, yobs) {
  mean(200 * abs(yobs - yhat) / (abs(yobs) + abs(yhat)))
}
