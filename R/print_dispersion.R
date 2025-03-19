#' Prints the dispersion of a numeric vector
#'
#' Calculates and prints the median and interquartile range (IQR) or the mean and standard deviation (SD).
#'
#' @inheritParams print_test
#' @inheritParams print_multinomial
#' @param x Numeric vector.
#' @param method Character specifying the method: `median` for median and IQR, or `mean` for mean and SD.
#'
#' @examples
#' print_dispersion(runif(10))
#' print_dispersion(runif(10), method = "mean", digits = 2)
#'
#' @export
print_dispersion <- function(x, digits = 2, method = "median", width = 10) {
  x <- unlist(x)
  method <- match.arg(method, c("median", "mean"))

  if (method == "mean") {
    center <- mean(x, na.rm = TRUE)
    dispersion <- sd(x, na.rm = TRUE)
  } else {
    center <- median(x, na.rm = TRUE)
    dispersion <- IQR(x, na.rm = TRUE)
  }

  result <- paste0(round(center, digits), "+/-", round(dispersion, digits))

  if (nchar(result) > width) {
    str_replace_all(result, "\\+\\/-", "\n+/-")
  } else {
    result
  }
}
