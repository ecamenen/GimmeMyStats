#' @description Redefine the default parameters of `rstatix::add_significance()`
#' by adding p-value significance symbols to a data frame.
#'
#' @inherit rstatix::add_significance
#'
#' @examples
#' library(magrittr)
#' library(rstatix, warn.conflicts = FALSE)
#' data("ToothGrowth")
#' ToothGrowth %>%
#'     t_test(len ~ dose) %>%
#'     adjust_pvalue() %>%
#'     add_significance0("p.adj")
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

pval_stars <- function(p) {
    if (p < 0.001) {
        return("***")
    }
    if (p < 0.01) {
        return("**")
    }
    if (p < 0.05) {
        return("*")
    }
    # if (p < 0.1) return(".")
    return("ns")
}

#' Prints a hypothesis test
#'
#' Formats the results of a hypothesis test (ANOVA, Kruskal-Wallis, or Wilcoxon).
#'
#' @param x Test object from `rstatix` among `anova_test`, `kruskal_test`, or
#' `wilcox_test`.
#' @param digits Integer specifying the number of decimal places for the test
#' statistic.
#' @param digits_p Integer specifying the number of decimal places for the
#' p-value.
#'
#' #' @return A character string containing the formatted test results with:
#' \describe{
#'   \item{Test name}{Name of the statistical test (ANOVA, Kruskal-Wallis, Wilcoxon,
#'   t-test, Friedman, or mixed-effects model).}
#'   \item{Test statistic}{Test statistic (F, K, W, T, or \eqn{\chi^2})
#'   with degrees of freedom when applicable.}
#'   \item{P-value}{P-value with significance stars.}
#' }
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
#' library(lmerTest)
#' data("sleepstudy", package = "lme4")
#' res <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' print_test(res)
#'
#' @export
print_test <- function(x, digits = 0, digits_p = 2) {
    if (!inherits(x, c("anova_test", "kruskal_test", "wilcox_test", "lmerModLmerTest", "htest"))) {
        stop("x must be a test object from anova_test, kruskal_test, wilcox_test, friedman.test or lmerTest::lmer.")
    }

    tmp <- class(x)
    if (length(tmp) == 1) {
        method <- tmp
    } else {
        method <- sub("_test", "", tmp[2])
        method <- ifelse(method == "data.frame", "anova", method)
    }

    if (method == "anova") {
        par <- paste0("(", x$DFn, ", ", x$DFd, ")")
        statistic <- round(x$F, digits)
        index <- "Anova, F"
    } else if (method %in% c("kruskal", "t")) {
        par <- paste0("(", x$df, ")")
        statistic <- round(x$statistic, digits)
        index <- switch(method,
            "t" = "T-test, F",
            "kruskal" = "Kruskal-Wallis, K",
            "htest" = paste0("Friedman, ", "\u03C7\u00B2")
        )
    } else if (method == "wilcox") {
        par <- ""
        statistic <- round(x$statistic, digits)
        index <- "Wilcoxon, W"
    } else if (method == "lmerModLmerTest") {
        x <- anova(x)
        par <- paste0("(", x$NumDF, ", ", round(x$DenDF), ")")
        statistic <- round(x$F, digits)
        index <- "Lmer, T"
        x$p <- x[, "Pr(>F)"]
    }

    if (!"p.signif" %in% colnames(x)) {
        x <- add_significance0(x)
    }

    x[x == "ns"] <- ""
    x$p <- paste0("= ", round(x$p, digits_p)) %>%
        str_replace_all("^= 0$", "< 0.001")

    paste0(index, par, " = ", statistic, ",", " p ", x$p, x$p.signif)
}

#' Prints the results of a Chi2
#'
#' Formats the results of a Chi-squared or Fisher's exact test.
#'
#' @inheritParams print_test
#' @param x Test object from `rstatix` among `chisq_test` or `fisher_test`.
#'
#' @return A character string containing the formatted test results with:
#' \describe{
#'   \item{Test statistic}{For Chi-squared test.}
#'   \item{P-value}{Formatted p-value with significance stars.}
#'   \item{Sample size}{Total count for sample size.}
#' }
#' For Fisher's exact test, only the P-value and sample size are included.
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
#' @param method Character specifying the statistical test: `chisq` for chi-squared
#' or `fisher` for Fisher's exact test.
#' @param count Logical specifying if `x` is a contingency table.
#' @param ... Additional arguments passed to `chisq.test` or `fisher.test`.
#' @details If x is numeric, it is treated as a contingency table and the names
#' are considered as categories; otherwise, the levels of the factor or the
#' characters are used.
#' @return A tibble with pairwise test results containing the following columns:
#' \describe{
#'   \item{group1, group2}{Character vectors specifying the pair of groups being compared.}
#'   \item{n}{Numeric vector specifying the total count or sample size for the comparison.}
#'   \item{statistic}{Numeric vector specifying the test statistic (for chi-squared tests only).}
#'   \item{df}{Numeric vector specifying the degrees of freedom (for chi-squared tests only).}
#'   \item{p}{Raw p-value for the pairwise comparison, formatted as numeric or character
#'            ("< 0.001" for very small p-values).}
#'   \item{p.signif}{Character vectors specifying the significance codes for raw p-values: 'ns' (not significant).}
#'   \item{FDR}{False Discovery Rate adjusted p-value using the specified method,
#'              formatted as numeric or character ("< 0.001" for very small values).}
#'   \item{fdr.signif}{Character vectors specifying the significance codes for FDR-adjusted p-values: 'ns' (not significant),
#'                     '*' (p < 0.05), '**' (p < 0.01), '***' (p < 0.001).}
#' }
#' For Fisher's exact tests, the `statistic` and `df` columns are not included..
#'
#' @examples
#' x <- c(rep("A", 100), rep("B", 78), rep("C", 25))
#' post_hoc_chi2(x)
#'
#' x <- data.frame(G1 = c(Yes = 100, No = 78), G2 = c(Yes = 75, No = 23))
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
    ...) {
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
                if (method == "fisher") {
                    method <- "chisq"
                    warning(
                        "With a single categorical data, Fisher's test cannot be performed. Using chi-squared test instead."
                    )
                }
                if (!count) {
                    x0 <- as.character(x) %>% table()
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
        mutate(FDR = p.adjust(p, method_adjust)) %>%
        add_significance(p.col = "FDR", output.col = "fdr.signif") %>%
        mutate(
            p = ifelse(p < 0.001, "< 0.001", round(p, digits)),
            FDR = ifelse(FDR < 0.001, "< 0.001", round(FDR, digits))
        ) %>%
        select(-matches("method")) %>%
        relocate(group1, group2)

    res[res == "****"] <- "***"

    if (method == "chisq") {
        relocate(res, df, .before = p)
    } else {
        res
    }
}
