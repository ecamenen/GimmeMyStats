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

  Character specifying the type of test: `chisq` for chi-squared or
  `fisher` for Fisher's exact test.

- method_adjust:

  Character specifying the p-value adjustment method.

- digits:

  Integer specifying the number of decimal places for the test
  statistic.

- count:

  Logical indicating if `x` is a contingency table.

- ...:

  Additional arguments passed to `chisq.test` or `fisher.test`.

## Value

Data frame with pairwise test results.

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
#>       n statistic p          df p.signif group1 group2 FDR     fdr.signif
#>   <int>     <dbl> <chr>   <dbl> <chr>    <chr>  <chr>  <chr>   <chr>     
#> 1   178      2.72 0.099       1 ns       A      B      0.099   ns        
#> 2   125     45    < 0.001     1 ***      A      C      < 0.001 ***       
#> 3   103     27.3  < 0.001     1 ***      B      C      < 0.001 ***       

x <- data.frame(G1 = c(Yes = 100, No = 78), G2 =  c(Yes = 75, No = 23))
post_hoc_chi2(x, count = TRUE, method = "chisq")
#> # A tibble: 1 × 9
#>       n statistic    df     p p.signif group1 group2   FDR fdr.signif
#>   <dbl>     <dbl> <int> <dbl> <chr>    <chr>  <chr>  <dbl> <chr>     
#> 1   276      10.4     1 0.001 **       G1     G2     0.001 **        

data("housetasks")
housetasks[, c("Wife", "Husband")] %>%
    t() %>%
    post_hoc_chi2(count = TRUE, workspace = 1e6)
#> # A tibble: 78 × 7
#>        n p       p.signif group1  group2     FDR     fdr.signif
#>    <int> <chr>   <chr>    <chr>   <chr>      <chr>   <chr>     
#>  1   287 0.249   ns       Laundry Main_meal  0.29    ns        
#>  2   242 0.009   **       Laundry Dinner     0.013   *         
#>  3   255 < 0.001 ***      Laundry Breakfeast < 0.001 ***       
#>  4   212 1       ns       Laundry Tidying    1       ns        
#>  5   194 0.012   *        Laundry Dishes     0.016   *         
#>  6   200 < 0.001 ***      Laundry Shopping   < 0.001 ***       
#>  7   193 < 0.001 ***      Laundry Official   < 0.001 ***       
#>  8   243 < 0.001 ***      Laundry Driving    < 0.001 ***       
#>  9   192 < 0.001 ***      Laundry Finances   < 0.001 ***       
#> 10   219 < 0.001 ***      Laundry Insurance  < 0.001 ***       
#> # ℹ 68 more rows

x <- cbind(
    mapply(function(x, y) rep(x, y), letters[seq(3)], c(7, 5, 8)) %>% unlist(),
    mapply(function(x, y) rep(x, y), LETTERS[seq(3)], c(6, 6, 8)) %>% unlist()
)
post_hoc_chi2(x)
#> # A tibble: 3 × 7
#>       n p       p.signif group1 group2 FDR     fdr.signif
#>   <int> <chr>   <chr>    <chr>  <chr>  <chr>   <chr>     
#> 1    12 0.015   *        A      B      0.015   *         
#> 2    14 < 0.001 ***      A      C      < 0.001 ***       
#> 3    14 < 0.001 ***      B      C      < 0.001 ***       
```
