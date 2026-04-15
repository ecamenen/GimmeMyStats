# Prints descriptive statistics for multinomial variables

Calculates and prints frequency counts and percentages for multinomial
(multi-level) categorical variables.

## Usage

``` r
print_multinomial(
  x,
  label = NULL,
  digits = 1,
  width = 15,
  n = nrow(x),
  format = FALSE,
  ...
)
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

- format:

  Logical specifying whether to format level names if the input is a
  vector.

- ...:

  Additional arguments passed to `count_category`.

## Value

A tibble with one row per level for each categorical level containing
the following columns:

- Variables:

  Character vector specifying the name of each variable.

- Levels:

  Character vector specifying the category level for each variable.

- Statistics:

  Character vector combining the frequency count and the percentage for
  each level.

## Examples

``` r
x <- data.frame(A = sample(c("X", "Y", "Z"), 100, replace = TRUE))
print_multinomial(x, label = "A")
#> # A tibble: 3 × 3
#>   Variables Levels Statistics
#>   <chr>     <fct>  <chr>     
#> 1 A         X      28 (28%)  
#> 2 A         Y      35 (35%)  
#> 3 A         Z      37 (37%)  
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
#> 1 "Variable\nA" X      "28\n(31.11%)"
#> 2 "Variable\nA" Y      "35\n(38.89%)"
#> 3 "Variable\nA" Z      "37\n(41.11%)"
```
