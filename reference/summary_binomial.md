# Summarizes descriptive statistics for binomial variables

Summarizes descriptive statistics for binomial variables

## Usage

``` r
summary_binomial(x, ref = NULL, ...)
```

## Arguments

- x:

  Data frame, matrix, or vector containing binomial variables.

- ref:

  Character specifying the name of the reference level.

- ...:

  Additional arguments passed to `print_binomial`.

## Value

A tibble with descriptive statistics containing the following columns:

- Variables:

  Character vector specifying the name of each variable.

- Statistics:

  Character vector combining the reference level of a variable with its
  frequency count and its percentage.

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
