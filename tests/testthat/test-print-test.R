data("ToothGrowth")
df <- ToothGrowth

test_that("mean_test test works", {
    res <- anova_test(df, len ~ dose) %>% print_test()
    expect_type(res, "character")
    expect_equal(res, "Anova, F(1, 58) = 105, p < 0.001***")
    res <- kruskal_test(df, len ~ dose) %>% print_test()
    expect_type(res, "character")
    expect_equal(res, "Kruskal-Wallis, K(2) = 41, p < 0.001***")
    res <- wilcox_test(df, len ~ supp) %>% print_test()
    expect_type(res, "character")
    expect_equal(res, "Wilcoxon, W = 576, p = 0.06")

    data("sleepstudy", package = "lme4")
    res <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy) %>% print_test()
    expect_type(res, "character")
    expect_equal(res, "Lmer, T(1, 17) = 46, p < 0.001***")
})

test_that("median test works", {
    res <- print_dispersion(c(0, 0, 0, 1, 1, 1))
    expect_type(res, "character")
    expect_equal(res, "0.5+/-1")
    res <- print_dispersion(c(0, 0, 0, 1, 1, 1), method = "mean", digits = 2, width = 5)
    expect_type(res, "character")
    expect_equal(res, "0.5\n+/-0.55")
})

test_that("add_significance0 test works", {
    tmp <- t_test(df, len ~ dose) %>% adjust_pvalue()
    res <- add_significance0(tmp, "p.adj")
    res0 <- add_significance(tmp, "p.adj")
    expect_identical(res0[, -10], res[, -10])
    expect_setequal(pull(res, "p.adj.signif"), "***")
})

test_that("chi test works", {
    x <- c(A = 100, B = 78, C = 25)
    res <- print_chi2_test(chisq_test(x))

    expect_type(res, "character")
    expect_equal(res, "X2(2) = 43.9, P < 0.001***, N = 3")

    xtab <- as.table(rbind(c(490, 10), c(400, 100)))
    dimnames(xtab) <- list(
        group = c("grp1", "grp2"),
        smoker = c("yes", "no")
    )
    res <- print_chi2_test(fisher_test(xtab))
    expect_equal(res, "P < 0.001***, N = 1000")
})

test_post_chi2 <- function(x, n) {
    expect_s3_class(x, "tbl_df")
    expect_contains(colnames(x), c("n", "p", "p.signif", "group1", "group2", "FDR", "fdr.signif"))
}

test_that("chi test works", {
    x <- c(rep("A", 100), rep("B", 78), rep("C", 25))
    expect_warning(post_hoc_chi2(x))

    res <- post_hoc_chi2(x, method = "chisq")
    test_post_chi2(res)
    expect_equal(dim(res), c(3, 9))
    expect_contains(colnames(res), c("statistic", "df"))


    x <- data.frame(G1 = c(Yes = 100, No = 78), G2 = c(Yes = 75, No = 23))
    res <- post_hoc_chi2(x, count = TRUE, method = "chisq")
    test_post_chi2(res)
    expect_equal(dim(res), c(1, 9))
    expect_contains(colnames(res), c("statistic", "df"))


    data("housetasks")
    res <- housetasks[, c("Wife", "Husband")] %>%
        t() %>%
        post_hoc_chi2(count = TRUE, workspace = 1e6)
    test_post_chi2(res)
    expect_equal(dim(res), c(78, 7))

    x <- cbind(
        mapply(function(x, y) rep(x, y), letters[seq(3)], c(7, 5, 8)) %>% unlist(),
        mapply(function(x, y) rep(x, y), LETTERS[seq(3)], c(6, 6, 8)) %>% unlist()
    )
    res <- post_hoc_chi2(x)
    test_post_chi2(res)
    expect_equal(dim(res), c(3, 7))
})
