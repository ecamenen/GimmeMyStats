
#' @description Redefine the default parameters of `rstatix::add_significance()`
#' by adding p-value significance symbols to a data frame.
#'
#' @inherit rstatix::add_significance
#'
#' @examples
#' library(magrittr)
#' library(rstatix)
#' data("ToothGrowth")
#' ToothGrowth %>%
#'   t_test(len ~ dose) %>%
#'   adjust_pvalue() %>%
#'   add_significance0("p.adj")
#'
#' @export
add_significance0 <- function(data, p.col = NULL, output.col = NULL) {
  add_significance(
    data,
    p.col = p.col,
    output.col = output.col,
    cutpoints = c(0, 1e-03, 1e-02, 5e-02, 1),
    symbols = c("***", "**", "*", "ns")
  )
}

#' Formats a hypothesis test
#'
#' Formats and prints the results of a hypothesis test (ANOVA, Kruskal-Wallis,
#' or Wilcoxon).
#'
#' @param x Test object from `rstatix` among `anova_test`, `kruskal_test`, or
#' `wilcox_test`.
#' @param digits Integer specifying the number of decimal places for the test
#' statistic.
#' @param digits_p Integer specifying the number of decimal places for the
#' p-value.
#'
#' @examples
#' library(rstatix)
#' data("ToothGrowth")
#' res <- anova_test(ToothGrowth, len ~ dose)
#' print_test(res)
#'
#' res <- kruskal_test(ToothGrowth, len ~ dose)
#' print_test(res)
#'
#' res <- wilcox_test(ToothGrowth, len ~ supp)
#' print_test(res)
#'
#' @export
print_test <- function(x, digits = 2, digits_p = 3) {
  if (!inherits(x, c("anova_test", "kruskal_test", "wilcox_test"))) {
    stop("x must be a test object from anova_test, kruskal_test, or wilcox_test.")
  }

  method <- sub("_test", "", class(x)[2])
  method <- ifelse(method == "data.frame", "anova", method)

  if (is.null(x$p.signif)) {
    x <- add_significance0(x)
  }

  x[x == "ns"] <- ""
  x$p <- paste0("= ", round(x$p, digits_p)) %>%
    str_replace_all("^= 0$", "< 0.001")

  if (method == "anova") {
    par <- paste0("(", x$DFn, ", ", x$DFd, ")")
    statistic <- round(x$F, digits)
    index <- "Anova, F"
  } else if (method %in% c("kruskal", "t")) {
    par <- paste0("(", round(x$df, 1), ")")
    statistic <- round(x$statistic, digits)
    index <- ifelse(method == "t", "T-test, F", "Kruskal-Wallis, K")
  } else if (method == "wilcox") {
    par <- ""
    statistic <- round(x$statistic, digits)
    index <- "W"
  }

  paste0(index, par, " = ", statistic, ",", " p ", x$p, x$p.signif)
}

#' Prints the results of a Chi2 or Fisher's exact test
#'
#' Formats and prints the results of a chi-squared or Fisher's exact test.
#'
#' @inheritParams print_test
#' @param x Test object from `rstatix` among `chisq_test` or `fisher_test`.
#'
#' @examples
#' x <- c(A = 100, B = 78, C = 25)
#' library(rstatix)
#' print_chi2_test(chisq_test(x))
#'
#' xtab <- as.table(rbind(c(490, 10), c(400, 100)))
#' dimnames(xtab) <- list(
#'     group = c("grp1", "grp2"),
#'     smoker = c("yes", "no")
#' )
#' print_chi2_test(fisher_test(xtab))
#'
#' @export
print_chi2_test <- function(x, digits = 3) {
  if ("chisq_test" %in% class(x)) {
    x$statistic <- paste0("X2(", x$df, ") = ", round(x$statistic, 1), ", ")
    x$method <- paste0(x$method, ", ")
  } else {
    x$method <- "Fisher's Exact test"
    x$statistic <- ""
  }

  if (x$p.signif == "ns") {
    x$p.signif <- ""
  }

  if (x$p < 0.001) {
    x$p <- "< 0.001"
  } else {
    x$p <- paste("=", round(x$p, digits))
  }

  x$p.signif[x$p.signif == "****"] <- "***"
  paste0(x$statistic, "P ", x$p, x$p.signif, ", N = ", x$n)
}

#' Performs post hoc analysis for chi-squared or Fisher's exact test
#'
#' Identifies pairwise differences between categories following a chi-squared
#' or Fisher's exact test.
#'
#' @inheritParams print_test
#' @inheritParams mcor_test
#' @param x Data frame, vector, or table. If numeric, treated as a contingency
#' table and the names are considered as categories; otherwise, the levels of
#' the factor or the characters are used.
#' @param method Character specifying the type of test: `chisq` for chi-squared
#' or `fisher` for Fisher's exact test.
#' @param count Logical indicating if `x` is a contingency table.
#' @param ... Additional arguments passed to `chisq.test` or `fisher.test`.
#' @details If x is numeric, it is treated as a contingency table and the names
#' are considered as categories; otherwise, the levels of the factor or the
#' characters are used.
#' @return Data frame with pairwise test results.
#'
#' @examples
#' x <- c(rep("A", 100), rep("B", 78), rep("C", 25))
#' post_hoc_chi2(x)
#'
#' x <- c(A = 100, B = 78, C = 25)
#' post_hoc_chi2(x, count = TRUE, method = "chisq")
#'
#' data("housetasks")
#' housetasks[, c("Wife", "Husband")] %>%
#'     t() %>%
#'     post_hoc_chi2(count = TRUE, workspace = 1e6)
#'
#' x <- cbind(
#'     mapply(function(x, y) rep(x, y), letters[seq(3)], c(7, 5, 8)) %>% unlist(),
#'     mapply(function(x, y) rep(x, y), LETTERS[seq(3)], c(6, 6, 8)) %>% unlist()
#' )
#' post_hoc_chi2(x)
#'
#' @export
post_hoc_chi2 <- function(
    x,
    method = "fisher",
    method_adjust = "BH",
    digits = 3,
    count = FALSE,
    ...
) {
  df0 <- as.data.frame(x)

  if (ncol(df0) > 1) {
    if (count) {
      x <- colnames(df0)
    } else {
      x <- pull(df0, 2)
    }
  }

  comb <- combn(unique(x) %>% length() %>% seq(), 2)

  res <- lapply(
    seq(ncol(comb)),
    function(i) {
      if (ncol(df0) > 1) {
        if (!count) {
          x0 <- table(df0)
        } else {
          x0 <- df0
        }
        df <- x0[, comb[, i]]
        dimn <- colnames(df)
      } else {
        method <- "chisq"
        warning(
          "With a single categorical data, Fisher's test cannot be performed. Using chi-squared test instead."
        )
        if (!count) {
          x0 <-  as.character(x) %>% table()
        } else {
          x0 <- x
        }
        df <- x0[comb[, i]]
        dimn <- names(df)
      }
      get(paste0(method, "_test"))(df, ...) %>%
        mutate(group1 = dimn[1], group2 = dimn[2])
    }
  ) %>%
    Reduce(rbind, .) %>%
    mutate(FDR = round(p.adjust(p, method_adjust), digits)) %>%
    add_significance(p.col = "FDR", output.col = "fdr.signif") %>%
  mutate(
    p = ifelse(p < 0.001, "< 0.001", round(p, digits)),
    FDR = ifelse(FDR < 0.001, "< 0.001", FDR)
  ) %>%
  select(-matches("method"))

res[res == "****"] <- "***"

if (method == "chisq") {
  relocate(res, df, .before = p)
} else {
  res
}
}
