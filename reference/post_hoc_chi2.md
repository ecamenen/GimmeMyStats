# Performs post hoc analysis for chi-squared or Fisher's exact test

Identifies pairwise differences between categories following a
chi-squared or Fisher's exact test.

## Usage

``` r
post_hoc_chi2(
  x,
  method = "fisher",
  method_adjust = "BH",
  digits = 3,
  count = FALSE,
  ...
)
```

## Arguments

- x:

  Data frame, vector, or table. If numeric, treated as a contingency
  table and the names are considered as categories; otherwise, the
  levels of the factor or the characters are used.

- method:

  Character specifying the statistical test: `chisq` for chi-squared or
  `fisher` for Fisher's exact test.

- method_adjust:

  Character specifying the p-value adjustment method.

- digits:

  Integer specifying the number of decimal places for the test
  statistic.

- count:

  Logical specifying if `x` is a contingency table.

- ...:

  Additional arguments passed to `chisq.test` or `fisher.test`.

## Value

A tibble with pairwise test results containing the following columns:

- group1, group2:

  Character vectors specifying the pair of groups being compared.

- n:

  Numeric vector specifying the total count or sample size for the
  comparison.

- statistic:

  Numeric vector specifying the test statistic (for chi-squared tests
  only).

- df:

  Numeric vector specifying the degrees of freedom (for chi-squared
  tests only).

- p:

  Raw p-value for the pairwise comparison, formatted as numeric or
  character ("\< 0.001" for very small p-values).

- p.signif:

  Character vectors specifying the significance codes for raw p-values:
  'ns' (not significant).

- FDR:

  False Discovery Rate adjusted p-value using the specified method,
  formatted as numeric or character ("\< 0.001" for very small values).

- fdr.signif:

  Character vectors specifying the significance codes for FDR-adjusted
  p-values: 'ns' (not significant), '*' (p \< 0.05), '**' (p \< 0.01),
  '***' (p \< 0.001).

For Fisher's exact tests, the `statistic` and `df` columns are not
included..

## Details

If x is numeric, it is treated as a contingency table and the names are
considered as categories; otherwise, the levels of the factor or the
characters are used.

## Examples

``` r
x <- c(rep("A", 100), rep("B", 78), rep("C", 25))
post_hoc_chi2(x)
#> Warning: With a single categorical data, Fisher's test cannot be performed. Using chi-squared test instead.
#> Warning: With a single categorical data, Fisher's test cannot be performed. Using chi-squared test instead.
#> Warning: With a single categorical data, Fisher's test cannot be performed. Using chi-squared test instead.
#> # A tibble: 3 × 9
#>   group1 group2     n statistic p          df p.signif FDR     fdr.signif
#>   <chr>  <chr>  <int>     <dbl> <chr>   <dbl> <chr>    <chr>   <chr>     
#> 1 A      B        178      2.72 0.099       1 ns       0.099   ns        
#> 2 A      C        125     45    < 0.001     1 ***      < 0.001 ***       
#> 3 B      C        103     27.3  < 0.001     1 ***      < 0.001 ***       

x <- data.frame(G1 = c(Yes = 100, No = 78), G2 = c(Yes = 75, No = 23))
post_hoc_chi2(x, count = TRUE, method = "chisq")
#> # A tibble: 1 × 9
#>   group1 group2     n statistic    df     p p.signif   FDR fdr.signif
#>   <chr>  <chr>  <dbl>     <dbl> <int> <dbl> <chr>    <dbl> <chr>     
#> 1 G1     G2       276      10.4     1 0.001 **       0.001 **        

data("housetasks")
housetasks[, c("Wife", "Husband")] %>%
    t() %>%
    post_hoc_chi2(count = TRUE, workspace = 1e6)
#> # A tibble: 78 × 7
#>    group1  group2         n p       p.signif FDR     fdr.signif
#>    <chr>   <chr>      <int> <chr>   <chr>    <chr>   <chr>     
#>  1 Laundry Main_meal    287 0.249   ns       0.29    ns        
#>  2 Laundry Dinner       242 0.009   **       0.013   *         
#>  3 Laundry Breakfeast   255 < 0.001 ***      < 0.001 ***       
#>  4 Laundry Tidying      212 1       ns       1       ns        
#>  5 Laundry Dishes       194 0.012   *        0.016   *         
#>  6 Laundry Shopping     200 < 0.001 ***      < 0.001 ***       
#>  7 Laundry Official     193 < 0.001 ***      < 0.001 ***       
#>  8 Laundry Driving      243 < 0.001 ***      < 0.001 ***       
#>  9 Laundry Finances     192 < 0.001 ***      < 0.001 ***       
#> 10 Laundry Insurance    219 < 0.001 ***      < 0.001 ***       
#> # ℹ 68 more rows

x <- cbind(
    mapply(function(x, y) rep(x, y), letters[seq(3)], c(7, 5, 8)) %>% unlist(),
    mapply(function(x, y) rep(x, y), LETTERS[seq(3)], c(6, 6, 8)) %>% unlist()
)
post_hoc_chi2(x)
#> # A tibble: 3 × 7
#>   group1 group2     n p       p.signif FDR     fdr.signif
#>   <chr>  <chr>  <int> <chr>   <chr>    <chr>   <chr>     
#> 1 A      B         12 0.015   *        0.015   *         
#> 2 A      C         14 < 0.001 ***      < 0.001 ***       
#> 3 B      C         14 < 0.001 ***      < 0.001 ***       
```
