#' Linear model function
#'
#' This function fits a linear model in R.
#'
#' @param formula \code{formula} class object, similar to \code{lm()}.
#' @param data Input data frame.
#' @keywords prediction
#'
#' @return Table with rows for each coefficient (including the \code{(Intercept)})
#'    and columns for the \code{Estimate}, \code{Std. Error}, \code{t value}, and
#'    \code{Pr(>|t|)}.
#'
#' @importFrom stats model.frame model.matrix model.response predict pt
#'
#' @examples
#' my_lm(mpg ~ hp + wt, mtcars)
#'
#' @export
my_lm <- function(formula, data) {
  x <- model.matrix(formula, data)
  y <- model.response(model.frame(formula, data))
  df <- nrow(x) - ncol(x)
  coeff <- solve(t(x) %*% x) %*% t(x) %*% y
  # Estimate
  sig2 <- sum((y - x %*% coeff)^2) / df
  # Std. Error
  standard_error <- diag(sqrt(sig2 * (solve(t(x) %*% x))))
  # t value
  test_stat <- coeff / standard_error
  # Pr(>|t|) -- two-sided t-test
  p_val <- 2 * pt(abs(test_stat), df, lower.tail = FALSE)
  # construct table
  tab <- cbind(coeff, standard_error, test_stat, p_val)
  colnames(tab) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(tab)
}
