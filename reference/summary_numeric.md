# Summarizes descriptive statistics for numeric variables

Formats the output of `print_numeric` into a concise summary.

## Usage

``` r
summary_numeric(x, digits = 1)
```

## Arguments

- x:

  Numeric vector, matrix, or data frame.

- digits:

  Integer specifying the number of decimal places for the test
  statistic.

## Value

Data frame with formatted descriptive statistics.

## Examples

``` r
x <- data.frame(A = rnorm(100), B = rnorm(100))
summary_numeric(x)
#> # A tibble: 2 × 2
#>   Variables `Median+/-IQR`
#>   <chr>     <chr>         
#> 1 A         0.1+/-1.3     
#> 2 B         -0.1+/-1.3    
```
