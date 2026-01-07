# Prints descriptive statistics for multinomial variables

Calculates and prints frequency counts and percentages for multinomial
(multi-level) categorical variables.

## Usage

``` r
print_multinomial(x, label = NULL, digits = 1, width = 15, n = nrow(x), ...)
```

## Arguments

- x:

  Data frame, matrix, or vector containing multinomial variables.

- label:

  Character vector specifying the names of the categorical variables.

- digits:

  Integer specifying the number of decimal places for the test
  statistic.

- width:

  Integer specifying the maximum width for wrapping text.

- n:

  Integer specifying the total number of observations.

- ...:

  Additional arguments passed to `count_category`.

## Value

Data frame with frequency counts and percentages for each category.

## Examples

``` r
x <- data.frame(A = sample(c("X", "Y", "Z"), 100, replace = TRUE))
print_multinomial(x, label = "A")
#> # A tibble: 3 × 3
#>   Variables Levels Statistics
#>   <chr>     <fct>  <chr>     
#> 1 A         X      24 (24%)  
#> 2 A         Z      34 (34%)  
#> 3 A         Y      42 (42%)  
x2 <- rbind(x, data.frame(A = rep("Level A", length(x[x == "Level X", ]))))
print_multinomial(
    x,
    label = "Variable A",
    sort = FALSE,
    n = 90,
    digits = 2,
    width = 5
)
#> # A tibble: 3 × 3
#>   Variables     Levels Statistics    
#>   <chr>         <fct>  <chr>         
#> 1 "Variable\nA" X      "24\n(26.67%)"
#> 2 "Variable\nA" Y      "42\n(46.67%)"
#> 3 "Variable\nA" Z      "34\n(37.78%)"
```
