#' Identifies outliers in a numeric vector
#'
#' Detects outliers using methods like IQR, percentiles, Hampel, MAD, or SD.
#'
#' @inheritParams print_dispersion
#' @param probabilities Numeric vector specifying probabilities for percentiles.
#' @param method Character specifying the method: `iqr`, `percentiles`, `hampel`, `mad`, or `sd`.
#' @param weight Numeric value specifying the multiplier for the detection threshold.
#' @param replace Logical indicating whether to replace outliers with `NA`.
#'
#' @return Numeric vector with outliers replaced by `NA` or their indices.
#'
#' @examples
#' x <- rnorm(100)
#' identify_outliers(x, method = "iqr")
#'
#' @export
identify_outliers <- function(
    x,
    probabilities = c(0.25, 0.75),
    method = "iqr",
    weight = 1.5,
    replace = TRUE
) {
  stopifnot(method %in% c("iqr", "percentiles", "hampel", "mad", "sd"))
  med <- median(x, na.rm = TRUE)

  if (method %in% c("hampel", "mad", "sd")) {
    if (method %in% c("hampel", "mad")) {
      mad3 <- weight * mad(x, na.rm = TRUE, constant = 1)
    } else {
      mad3 <- weight * sd(x, na.rm = TRUE)
    }
    up <- med + mad3
    low <- med - mad3
  } else {
    quant <- quantile(x, probs = probabilities, na.rm = TRUE)
    if (method == "iqr") {
      iqr <- (quant[2] - quant[1]) * weight
      quant[2] <- med + iqr
      quant[1] <- med - iqr
    }
    up <- quant[2]
    low <- quant[1]
  }

  if (!replace) {
    i <- which(x < low | x > up)
    x <- x[i]
    names(x) <- i
  } else {
    x[which(x < low | x > up)] <- NA
  }
  return(x)
}
