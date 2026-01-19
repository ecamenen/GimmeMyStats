# Prints the results of a Chi2

Formats the results of a Chi-squared or Fisher's exact test.

## Usage

``` r
print_chi2_test(x, digits = 3)
```

## Arguments

- x:

  Test object from `rstatix` among `chisq_test` or `fisher_test`.

- digits:

  Integer specifying the number of decimal places for the test
  statistic.

## Value

A character string containing the formatted test results with:

- Test statistic:

  For Chi-squared test.

- P-value:

  Formatted p-value with significance stars.

- Sample size:

  Total count for sample size.

For Fisher's exact test, only the P-value and sample size are included.

## Examples

``` r
x <- c(A = 100, B = 78, C = 25)
library(rstatix)
print_chi2_test(chisq_test(x))
#> [1] "X2(2) = 43.9, P < 0.001***, N = 3"

xtab <- as.table(rbind(c(490, 10), c(400, 100)))
dimnames(xtab) <- list(
    group = c("grp1", "grp2"),
    smoker = c("yes", "no")
)
print_chi2_test(fisher_test(xtab))
#> [1] "P < 0.001***, N = 1000"
```
