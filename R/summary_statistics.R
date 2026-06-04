#' Prints descriptive statistics for numeric variables
#'
#' Prints summary statistics (mean, median, quartiles, range, etc.) for numeric variables.
#'
#' @inheritParams print_test
#' @inheritParams count_category
#' @param x Data frame, matrix, or vector containing numerical variables.
#'
#' @return
#' A tibble with one row per numeric variable and the following columns:
#' \describe{
#'   \item{Variables}{Character specifying the variable name.}
#'   \item{Mean+/-SD}{Character specifying the mean and standard deviation.}
#'   \item{Median+/-IQR}{Character specifying the median and interquartile range.}
#'   \item{Q1-Q3}{Character specifying the first and third quartiles.}
#'   \item{Range}{Character specifying the minimum and maximum values.}
#'   \item{Kurtosis}{Numeric specifying the kurtosis coefficient.}
#'   \item{Skewness}{Numeric specifying the skewness coefficient.}
#'   \item{Normality}{Character specifying the Shapiro-Wilk normality test significance code.}
#'   \item{Zeros}{Integer specifying the number of zero values.}
#'   \item{NAs}{Integer specifying the number of missing values.}
#' }
#'
#' @examples
#' x <- data.frame(A = rnorm(100), B = rnorm(100))
#' print_numeric(x)
#' print_numeric(x, digits = 2, width = 5)
#'
#' @export
print_numeric <- function(x, digits = 1, width = 15) {
    as.data.frame(x) %>%
        pivot_longer(everything()) %>%
        set_colnames(c("Variables", "value")) %>%
        group_by(Variables) %>%
        summarise(
            "Mean+/-SD" = print_dispersion(value, digits, width, "mean"),
            "Median+/-IQR" = print_dispersion(value, digits, width, "median"),
            "Q1-Q3" = paste(
                quantile(value, .25, na.rm = TRUE) %>% round(digits),
                quantile(value, .75, na.rm = TRUE) %>% round(digits),
                sep = ";"
            ),
            Range = paste(
                min(value, na.rm = TRUE) %>% round(digits),
                max(value, na.rm = TRUE) %>% round(digits),
                sep = ";"
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
        ) %>%
        mutate(Variables = str_wrap(Variables, width))
}

#' Summarizes descriptive statistics for numeric variables
#'
#' Formats the output of `print_numeric` into a concise summary.
#'
#' @inheritParams print_numeric
#' @param ... Additional arguments passed to `print_numeric`.
#'
#' @return
#' A tibble with one row per numeric variable and the following columns:
#' \describe{
#'   \item{Variables}{Character specifying the variable name.}
#'   \item{Median+/-IQR}{Character specifying the median and interquartile range.}
#' }
#'
#' @examples
#' x <- data.frame(A = rnorm(100), B = rnorm(100))
#' summary_numeric(x)
#' summary_numeric(x, digits = 2, width = 5)
#'
#' @export
summary_numeric <- function(x, ...) {
    print_numeric(x, ...) %>%
        select(Variables, `Median+/-IQR`)
}

#' Frequency of categorical variables
#'
#' Formats a data frame or vector containing a multinomial (multi-level) variable and calculates the frequency of each levels.
#' @param x Either a character or factor vector, or a data frame of numerical values. For the latter, each column represents the absence (0) or presence (1) for each level of a categorical variable.
#' @param width Integer specifying the maximum width for wrapping text.
#' @param collapse Logical specifying whether to merge levels with identical proportions.
#' @param sort Logical or character vector. If `TRUE`, orders levels by frequency. If `FALSE`, orders by names. If a vector, renames and orders levels accordingly.
#' @param format Logical specifying whether to format level names if the input is a vector.
#'
#' @return
#' A tibble with one row per level and the following columns:
#' \describe{
#'   \item{f}{Factor specifying the level labels, possibly wrapped to the specified width. When
#'   \code{collapse = TRUE}, multiple levels with identical frequencies are
#'   merged into a single label separated by commas.}
#'   \item{n}{Integer specifying the frequency count for each level.}
#' }
#'
#' @examples
#' # Vector of categorical variable
#' k <- 5
#' n <- runif(k, 1, 10) %>% round()
#' x <- paste("Level", seq(k)) %>%
#'     mapply(function(x, y) rep(x, y), ., n) %>%
#'     unlist()
#' count_category(x)
#'
#' # Data frame of categorical variable
#' df <- table(seq_along(x), factor(x, levels = paste("Level", seq(k)))) %>%
#' as.data.frame.matrix()
#' count_category(df)
#' count_category(x, sort = FALSE, width = 5)
#' count_category(x, sort = seq(k), format = FALSE)
#' x2 <- c(x, rep("Level 6", n[1]))
#' count_category(x2, collapse = TRUE)
#' @export
count_category <- function(
    x,
    width = 15,
    collapse = FALSE,
    sort = TRUE,
    format = TRUE) {
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
        x0 <- factor(x0, labels = str_wrap(sort, width))
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
            mutate(f = factor(f)) %>%
            relocate(f)
    }

    return(df)
}

#' Prints descriptive statistics for binomial variables
#'
#' Calculates and prints frequency counts and percentages for binomial (two-level) categorical variables.
#'
#' @inheritParams print_test
#' @inheritParams print_multinomial
#' @inherit print_multinomial return
#' @param x Data frame, matrix, or vector containing binomial variables.
#'
#' @examples
#' x <- data.frame(A = sample(c("X", "Y"), 100, replace = TRUE))
#' print_binomial(x)
#' print_binomial(x, digits = 2, width = 5)
#'
#' @export
print_binomial <- function(x, digits = 1, width = 15) {
    as.data.frame(x) %>%
        pivot_longer(everything(), names_to = "Variables", values_to = "value") %>%
        group_by(Variables, value) %>%
        summarise(N = n(), .groups = "drop") %>%
        group_by(Variables) %>%
        mutate(
            `%` = (N / sum(N) * 100) %>% round(digits),
            Statistics = paste0(N, " (", `%`, "%)"),
            Levels = as.character(value)
        ) %>%
        ungroup() %>%
        mutate(across(c(Variables, Levels, Statistics), ~ str_wrap(.x, width = width))) %>%
        select(Variables, Levels, Statistics)
}

#' Summarizes descriptive statistics for binomial variables
#'
#' @inheritParams print_binomial
#' @param ref Character specifying the name of the reference level.
#' @param ... Additional arguments passed to `print_binomial`.
#'
#' @return A tibble with descriptive statistics containing the following columns:
#' \describe{
#'   \item{Variables}{Character vector specifying the name of each variable.}
#'   \item{Statistics}{Character vector combining the reference level of a variable with its frequency count and its percentage.}
#' }
#'
#' @examples
#' x <- data.frame(A = sample(c("X", "Y"), 100, replace = TRUE))
#' summary_binomial(x)
#' summary_binomial(x, digits = 2, width = 5)
#'
#' @export
summary_binomial <- function(x, ref = NULL, ...) {
    res <- print_binomial(x, ...) %>%
        group_by(Variables)
    if (!is.null(ref)) {
        res <- filter(res, Levels == !!ref)
    } else {
        res <- slice(res, 1)
    }

    summarise(res, Statistics = paste(Levels, ":", Statistics))
}

#' Prints descriptive statistics for multinomial variables
#'
#' Calculates and prints frequency counts and percentages for multinomial (multi-level) categorical variables.
#'
#' @inheritParams print_test
#' @inheritParams count_category
#' @param x Data frame, matrix, or vector containing multinomial variables.
#' @param label Character vector specifying the names of the categorical variables.
#' @param n Integer specifying the total number of observations.
#' @param ... Additional arguments passed to `count_category`.
#'
#'
#' @return A tibble with one row per level for each categorical level containing the following columns:
#' \describe{
#'   \item{Variables}{Character vector specifying the name of each variable.}
#'   \item{Levels}{Character vector specifying the category level for each variable.}
#'   \item{Statistics}{Character vector combining the frequency count and the percentage for each level.}
#' }
#'
#' @examples
#' x <- data.frame(A = sample(c("X", "Y", "Z"), 100, replace = TRUE))
#' print_multinomial(x, label = "A")
#' x2 <- rbind(x, data.frame(A = rep("Level A", length(x[x == "Level X", ]))))
#' print_multinomial(
#'     x,
#'     label = "Variable A",
#'     sort = FALSE,
#'     n = 90,
#'     digits = 2,
#'     width = 5
#' )
#'
#' @export
print_multinomial <- function(
        x,
        label = NULL,
        digits = 1,
        width = 15,
        n = nrow(x),
        format = FALSE,
        ...
        ) {
    if (is.null(label)) {
        label <- ifelse(!is.null(colnames(x)), colnames(x), "Variable")
    } else {
        label <- str_wrap(label, width)
    }
    count_category(x, width = width, format = format, ...) %>%
        set_colnames(c("Levels", "N")) %>%
        mutate(
            `%` = round((N / n) * 100, digits),
            Variables = label,
            Statistics = paste0(N, " (", `%`, "%)") %>% str_wrap(width)
        ) %>%
        select(Variables, Levels, Statistics)
}
