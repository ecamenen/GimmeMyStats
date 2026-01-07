# Summarizes descriptive statistics for binomial variables

Summarizes descriptive statistics for binomial variables

## Usage

``` r
summary_binomial(x, ...)
```

## Arguments

- x:

  Data frame, matrix, or vector containing binomial variables.

- ...:

  Additional arguments passed to `print_binomial`.

## Value

Data frame with formatted descriptive statistics.

## Examples

``` r
x <- data.frame(A = sample(c("X", "Y"), 100, replace = TRUE))
summary_binomial(x)
#> # A tibble: 1 × 2
#>   Variables Statistics  
#>   <chr>     <chr>       
#> 1 A         X : 53 (53%)
summary_binomial(x, digits = 2, width = 5)
#> # A tibble: 1 × 2
#>   Variables Statistics     
#>   <chr>     <chr>          
#> 1 A         "X : 53\n(53%)"
```
