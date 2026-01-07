# Summarizes descriptive statistics for numeric variables

Formats the output of `print_numeric` into a concise summary.

## Usage

``` r
summary_numeric(x, ...)
```

## Arguments

- x:

  Data frame, matrix, or vector containing numerical variables.

- ...:

  Additional arguments passed to `print_numeric`.

## Value

Data frame with formatted descriptive statistics.

## Examples

``` r
x <- data.frame(A = rnorm(100), B = rnorm(100))
summary_numeric(x)
#> # A tibble: 2 × 2
#>   Variables `Median+/-IQR`
#>   <chr>     <chr>         
#> 1 A         -0.2+/-1.2    
#> 2 B         -0.2+/-1.4    
summary_numeric(x, digits = 2, width = 5)
#> # A tibble: 2 × 2
#>   Variables `Median+/-IQR`  
#>   <chr>     <chr>           
#> 1 A         "-0.18\n+/-1.16"
#> 2 B         "-0.22\n+/-1.44"
```
