# Prints descriptive statistics for binomial variables

Calculates and prints frequency counts and percentages for binomial
(two-level) categorical variables.

## Usage

``` r
print_binomial(x, digits = 1, width = 15)
```

## Arguments

- x:

  Data frame, matrix, or vector containing binomial variables.

- digits:

  Integer specifying the number of decimal places for the test
  statistic.

- width:

  Integer specifying the maximum width for wrapping text.

## Value

Data frame with frequency counts and percentages for each category.

## Examples

``` r
x <- data.frame(A = sample(c("X", "Y"), 100, replace = TRUE))
print_binomial(x)
#> # A tibble: 2 × 3
#>   Variables Levels Statistics
#>   <chr>     <chr>  <chr>     
#> 1 A         X      59 (59%)  
#> 2 A         Y      41 (41%)  
print_binomial(x, digits = 2, width = 5)
#> # A tibble: 2 × 3
#>   Variables Levels Statistics 
#>   <chr>     <chr>  <chr>      
#> 1 A         X      "59\n(59%)"
#> 2 A         Y      "41\n(41%)"
```
