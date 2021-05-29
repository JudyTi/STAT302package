#' K-Nearest Neighbors Cross-Validation function
#'
#' This function predicts output class.
#'
#' @param train Input data frame.
#' @param cl True class value of your training data.
#' @param k_nn Integer representing the number of neighbors.
#' @param k_cv Integer representing the number of folds.
#' @keywords prediction
#'
#' @return List with objects: a vector of the predicted class Y for all
#'    observations \code{class} and a numeric with the cross-validation
#'    misclassification error \code{cv_err}.
#'
#' @examples
#' data <- na.omit(my_penguins)
#' train <- data[, 3:6]
#' my_knn_cv(train, data$species, 1, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  class <- vector()
  misclassification <- vector()
  # Iteration
  for (i in 1:k_cv) {
    x_train <- train[fold != i, ]
    x_test <- train[fold == i, ]
    y_train <- cl[fold != i]
    y_test <- cl[fold == i]
    # Record predictions
    prediction <- as.character(class::knn(x_train, x_test, y_train, k_nn))
    class <- c(class, prediction)
    # Compute misclassification rate
    misclassification[i] <- mean(prediction != y_test)
  }
  # Compute average misclassification rate to get CV error
  cv_err <- mean(misclassification)
  return(list("class" = class, "cv_err" = cv_err))
}
