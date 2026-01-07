test_identify_outliers <- function(x) {
    expect_type(x, "double")
    expect_vector(names(x))
}

test_that("identify outliers works", {
    x <- rnorm(100)
    res <- identify_outliers(x, method = "iqr")
    test_identify_outliers(res)

    res <- identify_outliers(x, method = "percentiles", probabilities = c(0.1, 0.9))
    test_identify_outliers(res)

    res <- identify_outliers(x, method = "hampel")
    test_identify_outliers(res)

    res <- identify_outliers(x, method = "mad")
    tmp <- names(res)
    test_identify_outliers(res)
    res <- identify_outliers(x, method = "mad", replace = TRUE)
    expect_equal(unique(res[tmp]), NA_real_)

    res <- identify_outliers(x, method = "sd", weight = 3)
    test_identify_outliers(res)
})
