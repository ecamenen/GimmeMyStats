#' Prints descriptive statistics for numeric variables
#'
#' Prints summary statistics (mean, median, quartiles, range, etc.) for numeric variables.
#'
#' @inheritParams print_test
#' @param x Numeric vector, matrix, or data frame.
#'
#' @return Data frame with descriptive statistics for each variable.
#'
#' @examples
#' x <- data.frame(A = rnorm(100), B = rnorm(100))
#' print_numeric(x)
#'
#' @export
print_numeric <- function(x, digits = 1) {
  as.data.frame(x) %>%
    pivot_longer(everything()) %>%
    set_colnames(c("Variables", "value")) %>%
    group_by(Variables) %>%
    summarise(
      "Mean+/-SD" = print_dispersion(value, digits, "mean"),
      "Median+/-IQR" = print_dispersion(value, digits, "median"),
      "Q1-Q3" = paste(
        quantile(value, .25, na.rm = TRUE) %>% round(digits),
        quantile(value, .75, na.rm = TRUE) %>% round(digits),
        sep = "-"
      ),
      Range = paste(
        min(value, na.rm = TRUE) %>% round(digits),
        max(value, na.rm = TRUE) %>% round(digits),
        sep = "-"
      ),
      Kurtosis = kurtosis(value, na.rm = TRUE) %>% round(digits),
      Skewness = skewness(value, na.rm = TRUE) %>% round(digits),
      Normality = {
        if (unique(value) %>% na.omit() %>% length() < 3) {
          NA
        } else {
          ifelse(length(value) > 5000, 5000, length(value)) %>%
            sample(value, .) %>%
            shapiro_test() %>%
            add_significance0() %>%
            pull(p.value.signif)
        }
      },
      Zeros = length(which(value == 0)),
      NAs = length(which(is.na(value)))
    )
}

#' Summarizes descriptive statistics for numeric variables
#'
#' Formats the output of `print_numeric` into a concise summary.
#'
#' @inheritParams print_numeric
#'
#' @return Data frame with formatted descriptive statistics.
#'
#' @examples
#' x <- data.frame(A = rnorm(100), B = rnorm(100))
#' summary_numeric(x)
#'
#' @export
summary_numeric <- function(x, digits = 1) {
  print_numeric(x, digits) %>%
    select(Variables, `Median+/-IQR`)
}

#' Frequency of categorical variables
#'
#' Formats a data frame or vector containing categorical variables and calculates the frequency of each category.
#'
#' @inheritParams print_multinomial
#' @param x Vector or data frame of categorical variables.
#' @param collapse Logical indicating whether to merge categories with identical proportions.
#' @param sort Logical or character vector. If `TRUE`, orders categories by frequency. If `FALSE`, orders by names. If a character vector, renames and orders categories accordingly.
#' @param format Logical indicating whether to format category names if the input is a vector.
#'
#' @return Data frame with two columns: `f` (category names) and `n` (frequency counts).
#'
#' @examples
#' # Vector of categorical variable
#' k <- 10
#' n <- runif(k, 1, 10) %>% round()
#' x <- paste("Level", seq(k)) %>%
#'     mapply(function(x, y) rep(x, y), ., n) %>%
#'     unlist()
#' count_cat(x)
#'
#' # Data frame of categorical variable
#' df <- sapply(seq(10), function(x) runif(10) %>% round()) %>% as.data.frame()
#' colnames(df) <- paste("Level", seq(10))
#' count_cat(df)
#'
#' @export
count_cat <- function(
    x,
    width = 20,
    collapse = FALSE,
    sort = TRUE,
    format = TRUE
) {
  x <- as.data.frame(x)
  col_name <- colnames(x)

  if (ncol(x) > 1) {
    x <- sapply(
      colnames(x),
      function(i) rep(i, pull(x, i) %>% unlist() %>% sum(na.rm = TRUE))
    )
  }

  x0 <- unlist(x) %>%
    stri_trans_general("latin-ascii") %>%
    str_replace_all("\n", " ") %>%
    to_title() %>%
    str_wrap(width) %>%
    factor()

  if (isTRUE(sort)) {
    x0 <- fct_infreq(x0) %>%
      fct_rev()
  } else if (!isFALSE(sort)) {
    x0 <- ordered(x0, levels = str_wrap(sort, width))
  }

  df <- fct_relabel(x0, ~ str_remove_all(.x, "\\s*\\([^\\)]+\\)")) %>%
    fct_relabel(~ str_remove_all(.x, "\\$\\$[^\\)]+"))

  if (format) {
    df <- df %>%
      fct_relabel(~ str_replace_all(.x, "^0$", "No")) %>%
      fct_relabel(
        ~ str_replace_all(
          .x,
          "^1$",
          ifelse(col_name[1] == "x", "Yes", col_name[1])
        )
      )
  }

  df <- fct_count(df)

  if (collapse) {
    df <- group_by(df, n) %>%
      summarise(
        f = paste(f, collapse = ", ") %>%
          str_wrap(width)
      ) %>%
      mutate(f = factor(f))
    df$f <- reorder(df$f, df$n)
  }

  return(df)
}

#' Prints descriptive statistics for binomial variables
#'
#' Calculates and prints frequency counts and percentages for binomial (two-level) categorical variables.
#'
#' @inheritParams print_test
#' @param x Data frame, matrix, or vector containing binomial variables.
#'
#' @return Data frame with frequency counts and percentages for each category.
#'
#' @examples
#' x <- data.frame(A = sample(c("X", "Y"), 100, replace = TRUE))
#' print_binomial(x)
#'
#' @export
print_binomial <- function(x, digits = 1) {
  pivot_longer(x, everything()) %>%
    set_colnames(c("Variables", "value")) %>%
    group_by(Variables) %>%
    summarise(
      fct_count(value) %>%
        set_colnames(c("Levels", "N")) %>%
        mutate(
          `%` = (N / length(value) * 100) %>% round(digits),
          stat = paste0(N, " (", `%`, "%)")
        )
    ) %>%
    ungroup() %>%
    select(Variables, Levels, stat)
}

#' Prints descriptive statistics for multinomial variables
#'
#' Calculates and prints frequency counts and percentages for multinomial (multi-level) categorical variables.
#'
#' @inheritParams print_test
#' @param x Data frame, matrix, or vector containing multinomial variables.
#' @param var Character vector specifying the names of the categorical variables.
#' @param parse Logical indicating whether to parse variable names.
#' @param width Integer specifying the maximum width for wrapping text.
#' @param collapse Logical indicating whether to collapse categories into a single string.
#' @param label Character vector specifying labels for variables.
#' @param n Integer specifying the total number of observations.
#'
#' @return Data frame with frequency counts and percentages for each category.
#'
#' @examples
#' x <- data.frame(A = sample(c("X", "Y", "Z"), 100, replace = TRUE))
#' print_multinomial(x, var = "A")
#'
#' @export
print_multinomial <- function(x, var, digits = 1, parse = FALSE, width = 20, collapse = FALSE, label = NULL, n = nrow(x)) {
  count_cat(x, width = width) %>%
    set_colnames(c("Levels", "N")) %>%
    mutate(
      `%` = round((N / n) * 100, digits),
      Variables = var,
      stat = paste0(N, " (", `%`, "%)")
    ) %>%
    select(Variables, Levels, stat)
}

#' Summarizes descriptive statistics for binomial variables
#'
#' @inheritParams print_binomial
#'
#' @return Data frame with formatted descriptive statistics.
#'
#' @examples
#' x <- data.frame(A = sample(c("X", "Y"), 100, replace = TRUE))
#' summary_binomial(x)
#'
#' @export
summary_binomial <- function(x, digits = 1) {
  print_binomial(x, digits) %>%
    summarise(Levels = paste(stat, collapse = "; "))
}
