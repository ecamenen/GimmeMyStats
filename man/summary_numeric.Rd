% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_statistics.R
\name{summary_numeric}
\alias{summary_numeric}
\title{Summarizes descriptive statistics for numeric variables}
\usage{
summary_numeric(x, digits = 1)
}
\arguments{
\item{x}{Numeric vector, matrix, or data frame.}

\item{digits}{Integer specifying the number of decimal places for the test
statistic.}
}
\value{
Data frame with formatted descriptive statistics.
}
\description{
Formats the output of \code{print_numeric} into a concise summary.
}
\examples{
x <- data.frame(A = rnorm(100), B = rnorm(100))
summary_numeric(x)

}
