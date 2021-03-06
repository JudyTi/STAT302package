% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_lm.R
\name{my_lm}
\alias{my_lm}
\title{Linear model function}
\usage{
my_lm(formula, data)
}
\arguments{
\item{formula}{\code{formula} class object, similar to \code{lm()}.}

\item{data}{Input data frame.}
}
\value{
Table with rows for each coefficient (including the \code{(Intercept)})
and columns for the \code{Estimate}, \code{Std. Error}, \code{t value}, and
\code{Pr(>|t|)}.
}
\description{
This function fits a linear model in R.
}
\examples{
my_lm(mpg ~ hp + wt, mtcars)

}
\keyword{prediction}
