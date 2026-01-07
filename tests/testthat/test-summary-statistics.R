set.seed(123)
x <- data.frame(A = rnorm(100), B = rnorm(100))
colnames(x) <- paste("Variable", colnames(x))

test_print_numeric <- function(x) {
    expect_equal(
        colnames(x),
        c(
            "Variables", "Mean+/-SD", "Median+/-IQR", "Q1-Q3",
            "Range", "Kurtosis", "Skewness", "Normality",
            "Zeros", "NAs"
        )
    )
    expect_equal(nrow(x), 2)
}

test_that("print numeric works", {
    res <- print_numeric(x)
    test_print_numeric(res)
    res <- print_numeric(x, digits = 2, width = 5)
    test_print_numeric(res)
    expect_equal(res[[1, 1]], "Variable\nA")
    expect_equal(res[[1, 6]], -0.22)
})

test_summary_numeric <- function(x) {
    expect_equal(colnames(x), c("Variables", "Median+/-IQR"))
    expect_equal(nrow(x), 2)
}

test_that("summary numeric works", {
    res <- summary_numeric(x)
    test_summary_numeric(res)
    res <- summary_numeric(x, digits = 2, width = 5)
    test_summary_numeric(res)
    expect_equal(unname(unlist(res[1, ])), c("Variable\nA", "0.06\n+/-1.19"))
})

test_print_binomial <- function(x) {
    expect_equal(colnames(x), c("Variables", "Levels", "Statistics"))
    expect_equal(nrow(x), 4)
}

x <- lapply(
    seq(2),
    function(i) sample(paste0("Level ", c("X", "Y"), i), 100, replace = TRUE)
)
x <- data.frame(x)
colnames(x) <- paste("Variable", c("A", "B"))

test_that("print binomial works", {
    res <- print_binomial(x)
    test_print_binomial(res)
    res <- print_binomial(x, digits = 2, width = 5)
    test_print_binomial(res)
    expect_equal(unname(unlist(res[1, ])), c("Variable\nA", "Level\nX1", "47\n(47%)"))
})

test_summary_binomial <- function(x) {
    expect_equal(colnames(x), c("Variables", "Statistics"))
    expect_equal(nrow(x), 2)
}

test_that("summary binomial works", {
    res <- summary_binomial(x)
    test_summary_binomial(res)
    res <- summary_binomial(x, digits = 2, width = 5)
    test_summary_binomial(res)
    expect_equal(unname(unlist(res[1, ])), c("Variable\nA", "Level\nX1 : 47\n(47%)"))
})

test_count_cat <- function(x) {
    expect_equal(colnames(x), c("f", "n"))
    expect_lte(nrow(x), 5)
}

test_that("count cat works", {
    k <- 5
    n <- runif(k, 1, 10) %>% round()
    x <- paste("Level", seq(k)) %>%
        mapply(function(x, y) rep(x, y), ., n) %>%
        unlist()
    res <- count_cat(x)
    test_count_cat(res)
    expect_equal(as.character(res[[1, 1]]), "Level 3")
    res <- count_cat(x, sort = FALSE, width = 5)
    test_count_cat(res)
    expect_equal(as.character(res[[1, 1]]), "Level\n1")
    res <- count_cat(x, sort = seq(k))
    test_count_cat(res)
    expect_equal(as.character(res[[1, 1]]), "Yes")
    res <- count_cat(x, sort = seq(k), format = FALSE)
    test_count_cat(res)
    expect_equal(as.character(res[[1, 1]]), "1")
    x2 <- c(x, rep("Level 6", n[1]))
    res <- count_cat(x2, collapse = TRUE)
    test_count_cat(res)

    # Data frame of categorical variable
    df <- sapply(seq(k), function(x) runif(10) %>% round()) %>%
        as.data.frame()
    colnames(df) <- paste("Level", seq(k))

    res <- count_cat(df, width = 5)
    test_count_cat(res)
    expect_equal(as.character(res[[1, 1]]), "Level\n4")
    res <- count_cat(df, sort = FALSE)
    test_count_cat(res)
    expect_equal(as.character(res[[1, 1]]), "Level 1")
    res <- count_cat(df, sort = seq(k), format = FALSE)
    test_count_cat(res)
})

test_print_multinomial <- function(x) {
    expect_equal(colnames(x), c("Variables", "Levels", "Statistics"))
    expect_lte(nrow(x), 3)
}

test_that("print multinomial works", {
    x <- data.frame(A = sample(paste("Level", c("X", "Y", "Z")), 100, replace = TRUE))
    x2 <- rbind(x, data.frame(A = rep("Level A", length(x[x == "Level X", ]))))
    res <- print_multinomial(x)
    test_print_multinomial(res)
    res <- print_multinomial(
        x,
        label = "Variable A",
        sort = FALSE,
        n = 90,
        digits = 2,
        width = 5
    )
    test_print_multinomial(res)
    expect_equal(res[[1, 1]], "Variable\nA")
    expect_equal(as.character(res[[1, 2]]), "Level\nX")
    expect_equal(res[[1, 3]], "42\n(46.67%)")
    res <- print_multinomial(x2, collapse = TRUE)
    test_print_multinomial(res)
})
