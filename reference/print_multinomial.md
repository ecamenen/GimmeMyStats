# Prints descriptive statistics for multinomial variables

Calculates and prints frequency counts and percentages for multinomial
(multi-level) categorical variables.

## Usage

``` r
print_multinomial(
  x,
  var = NULL,
  digits = 1,
  parse = FALSE,
  width = 20,
  collapse = FALSE,
  label = NULL,
  n = nrow(x)
)
```

## Arguments

- x:

  Data frame, matrix, or vector containing multinomial variables.

- var:

  Character vector specifying the names of the categorical variables.

- digits:

  Integer specifying the number of decimal places for the test
  statistic.

- parse:

  Logical indicating whether to parse variable names.

- width:

  Integer specifying the maximum width for wrapping text.

- collapse:

  Logical indicating whether to collapse categories into a single
  string.

- label:

  Character vector specifying labels for variables.

- n:

  Integer specifying the total number of observations.

## Value

Data frame with frequency counts and percentages for each category.

## Examples

``` r
x <- data.frame(A = sample(c("X", "Y", "Z"), 100, replace = TRUE))
print_multinomial(x, var = "A")
#> # A tibble: 3 × 3
#>   Variables Levels Statistics
#>   <chr>     <fct>  <chr>     
#> 1 Variable  Z      29 (29%)  
#> 2 Variable  X      33 (33%)  
#> 3 Variable  Y      38 (38%)  
```
