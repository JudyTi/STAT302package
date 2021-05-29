#' Random Forest Cross-Validation function
#'
#' This function predicts output \code{body_mass_g} using covariates
#'   \code{bill_length_mm}, \code{ill_depth_mm}, and \code{flipper_length_mm}.
#'
#' @param k Integer representing the number of folds.
#' @keywords prediction
#'
#' @return Numeric with the cross-validation error \code{cv_err}.
#'
#' @importFrom stats na.omit predict
#' @importFrom magrittr %>%
#' @importFrom dplyr select all_of
#' @importFrom randomForest randomForest
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  # Define a variable fold within the penguins data
  name <- c("body_mass_g", "bill_length_mm", "bill_depth_mm", "flipper_length_mm")
  data <- na.omit(STAT302package::my_penguins)
  data <- data %>% select(all_of(name))
  fold <- sample(rep(1:k, length = nrow(data)))
  # Empty vector to store MSE
  mse <- c(NA, k)
  # Iteration
  for (i in 1:k) {
    training_data <- data[fold != i, ]
    test_data <- data[fold == i, ]
    # Train models
    MODEL <- randomForest(body_mass_g ~ bill_length_mm +
                                          bill_depth_mm + flipper_length_mm,
                          data = training_data,
                          ntree = 100)
    # Record predictions
    PREDICTIONS <- predict(MODEL, test_data[, -1])
    # Compute MSE
    mse[i] <- mean((PREDICTIONS - test_data$body_mass_g)^2)
  }
  # Return average MSE to get CV error
  return(mean(mse))
}
