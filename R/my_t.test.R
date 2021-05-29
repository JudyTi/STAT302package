#' T-test function
#'
#' This function performs a one sample t-test in R.
#'
#' @param x Numeric vector of data.
#' @param alternative Character string specifying the alternative hypothesis.
#'   This should only accept "two.sided", "less", or "greater".
#'   Otherwise, your function should throw an informative error.
#' @param mu Number indicating the null hypothesis value of the mean.
#' @keywords inference
#'
#' @return List with elements: the numeric test statistic \code{test_stat},
#'   the degrees of freedom \code{df},
#'   the value of the parameter \code{alternative},
#'   the numeric p-value \code{p_val}.
#'
#' @importFrom stats pt sd
#'
#' @examples
#' set.seed(302)
#' x <- rnorm(10, mean = 1, sd = 1)
#' # two-sided t-test
#' my_t.test(x, alternative = "two.sided", mu = 0.5)
#' # one-sided t-test -- upper tail
#' my_t.test(x, alternative = "greater", mu = 0.5)
#' # one-sided t-test -- lower tail
#' my_t.test(x, alternative = "less", mu = 0.5)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  mu_hat <- mean(x)
  test_stat <- (mu_hat - mu) / (sd(x) / sqrt(length(x)))
  df <- length(x) - 1
  # one-sided t-test -- upper tail
  if (alternative == "greater") {
    p_val <- pt(test_stat, df, lower.tail = FALSE)
    # two-sided t-test
  } else if (alternative == "two.sided") {
    p_val <- 2 * pt(abs(test_stat), df, lower.tail = FALSE)
    # one-sided t-test -- lower tail
  } else if (alternative == "less") {
    p_val <- 1 - pt(test_stat, df, lower.tail = FALSE)
    # throw an informative error
  } else {
    stop("Alternative must be greater, two.sided or less")
  }
  # return list
  return(list("test_stat" = test_stat,
              "df" = df,
              "alternative" = alternative,
              "p_val" = p_val))
}
