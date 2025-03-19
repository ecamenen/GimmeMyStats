#' Multiple correlation test
#'
#' Calculates correlations between multiple variables.
#'
#' @param x Data frame of numerical variables.
#' @param y Data frame of numerical variables. If `NULL`, correlations are calculated within `x`.
#' @param estimate Logical indicating whether to return correlation coefficients.
#' @param p.value Logical indicating whether to return adjusted p-values.
#' @param method Character specifying the correlation method: `pearson`, `kendall`, or `spearman`.
#' @param method_adjust Character specifying the p-value adjustment method.
#'
#' @return List containing two data frames corresponding to correlation coefficients and p-values.
#'
#' @examples
#' library(magrittr)
#' x0 <- runif(20)
#' x <- lapply(
#'     c(1, -1),
#'     function(i) sapply(seq(10), function(j) x0 * i + runif(10, max = 1))
#' ) %>%
#'     Reduce(cbind, .) %>%
#'     set_colnames(paste("Variable", seq(20)))
#' y <- lapply(
#'     c(1, -1),
#'     function(i) sapply(seq(10), function(j) x0 * i + runif(10, max = 1))
#' ) %>%
#'     Reduce(cbind, .) %>%
#'     set_colnames(paste("Variable", seq(20))) %>%
#'     .[, seq(5)]
#' mcor_test(x)
#' mcor_test(
#'     x,
#'     y,
#'     p.value = TRUE,
#'     method = "pearson",
#'     method_adjust = "bonferroni"
#' )
#'
#' @export
mcor_test <- function(
    x,
    y = NULL,
    estimate = TRUE,
    p.value = FALSE,
    method = "spearman",
    method_adjust = "BH"
) {
  x <- as.data.frame(x)

  if (!is.null(y)) {
    y <- as.data.frame(y)
    if (nrow(x) != nrow(y)) {
      stop("The number of rows in x must match the number of rows in y.")
    }
  } else {
    y <- x
  }

  res <- lapply(
    seq(ncol(x)),
    function(i) {
      lapply(
        seq(ncol(y)),
        function(j) {
          if (is.numeric(x[, i]) & is.numeric(y[, j])) {
            tryCatch(
              {
                cor.test(
                  x[, i],
                  y[, j],
                  method = method,
                  na.rm = TRUE
                )
              },
              error = function(e) NA
            )
          } else {
            NA
          }
        }
      )
    }
  )

  if (estimate) {
    rho <- lapply(res, function(i) lapply(i, function(j) j$estimate)) %>%
      unlist() %>%
      matrix(nrow = NCOL(y), ncol = NCOL(x))
    colnames(rho) <- colnames(x)
    rownames(rho) <- colnames(y)
  }

  if (p.value) {
    p <- lapply(res, function(i) lapply(i, function(j) j$p.value)) %>%
      unlist() %>%
      matrix(nrow = NCOL(y), ncol = NCOL(x))
  }
  if (p.value && method_adjust != "none") {
    p <- as.vector(p) %>%
      p.adjust(method_adjust) %>%
      matrix(nrow = NCOL(y), ncol = NCOL(x))
  }
  if (p.value) {
    colnames(p) <- colnames(x)
    rownames(p) <- colnames(y)
  }

  if (estimate && p.value) {
    return(list(estimate = rho, p.value = p))
  } else if (estimate) {
    return(rho)
  } else {
    return(p)
  }
}
