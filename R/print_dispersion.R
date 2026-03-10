#' Prints the dispersion of a numeric vector
#'
#' Calculates and prints the median and interquartile range (IQR) or the mean and standard deviation (SD).
#'
#' @inheritParams print_test
#' @inheritParams print_multinomial
#' @param x Vector containing numerical values.
#' @param method Character specifying the method: `median` for median and IQR, or `mean` for mean and SD.
#'
#' @return
#' A character string containing a measure of central tendency and
#' dispersion. Depending on \code{method}, this is either the median and
#' interquartile range or the mean and standard deviation.
#'
#' @examples
#' print_dispersion(runif(10))
#' print_dispersion(runif(10), method = "mean", digits = 2, width = 5)
#'
#' @export
print_dispersion <- function(x, digits = 1, width = 15, method = "median") {
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
