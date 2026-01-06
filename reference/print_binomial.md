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
#> # A tibble: 2 × 3
#>   Variables Levels stat    
#>   <chr>     <fct>  <chr>   
#> 1 A         X      50 (50%)
#> 2 A         Y      50 (50%)
```
