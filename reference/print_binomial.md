# Prints descriptive statistics for binomial variables

Calculates and prints frequency counts and percentages for binomial
(two-level) categorical variables.

## Usage

``` r
print_binomial(x, digits = 1)
```

## Arguments

- x:

  Data frame, matrix, or vector containing binomial variables.

- digits:

  Integer specifying the number of decimal places for the test
  statistic.

## Value

Data frame with frequency counts and percentages for each category.

## Examples

``` r
x <- data.frame(A = sample(c("X", "Y"), 100, replace = TRUE))
print_binomial(x)
#> Warning: Returning more (or less) than 1 row per `summarise()` group was deprecated in
#> dplyr 1.1.0.
#> ℹ Please use `reframe()` instead.
#> ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()`
#>   always returns an ungrouped data frame and adjust accordingly.
#> ℹ The deprecated feature was likely used in the GimmeMyStats package.
#>   Please report the issue at <https://github.com/ecamenen/GimmeMyStats/issues>.
#> # A tibble: 2 × 3
#>   Variables Levels stat    
#>   <chr>     <fct>  <chr>   
#> 1 A         X      50 (50%)
#> 2 A         Y      50 (50%)
```
