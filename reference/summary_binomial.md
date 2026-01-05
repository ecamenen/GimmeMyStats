# Summarizes descriptive statistics for binomial variables

Summarizes descriptive statistics for binomial variables

## Usage

``` r
summary_binomial(x, digits = 1)
```

## Arguments

- x:

  Data frame, matrix, or vector containing binomial variables.

- digits:

  Integer specifying the number of decimal places for the test
  statistic.

## Value

Data frame with formatted descriptive statistics.

## Examples

``` r
x <- data.frame(A = sample(c("X", "Y"), 100, replace = TRUE))
summary_binomial(x)
#> # A tibble: 1 × 2
#>   Variables Statistics  
#>   <chr>     <chr>       
#> 1 A         X : 44 (44%)
```
