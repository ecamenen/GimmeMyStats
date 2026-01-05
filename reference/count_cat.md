# Frequency of categorical variables

Formats a data frame or vector containing categorical variables and
calculates the frequency of each category.

## Usage

``` r
count_cat(x, width = 20, collapse = FALSE, sort = TRUE, format = TRUE)
```

## Arguments

- x:

  Vector or data frame of categorical variables.

- width:

  Integer specifying the maximum width for wrapping text.

- collapse:

  Logical indicating whether to merge categories with identical
  proportions.

- sort:

  Logical or character vector. If `TRUE`, orders categories by
  frequency. If `FALSE`, orders by names. If a character vector, renames
  and orders categories accordingly.

- format:

  Logical indicating whether to format category names if the input is a
  vector.

## Value

Data frame with two columns: `f` (category names) and `n` (frequency
counts).

## Examples

``` r
# Vector of categorical variable
k <- 10
n <- runif(k, 1, 10) %>% round()
x <- paste("Level", seq(k)) %>%
    mapply(function(x, y) rep(x, y), ., n) %>%
    unlist()
count_cat(x)
#> # A tibble: 10 × 2
#>    f            n
#>    <fct>    <int>
#>  1 Level 9      1
#>  2 Level 1      1
#>  3 Level 8      3
#>  4 Level 4      4
#>  5 Level 10     4
#>  6 Level 3      5
#>  7 Level 2      5
#>  8 Level 6      8
#>  9 Level 5      8
#> 10 Level 7      9

# Data frame of categorical variable
df <- sapply(seq(10), function(x) runif(10) %>% round()) %>% as.data.frame()
colnames(df) <- paste("Level", seq(10))
count_cat(df)
#> # A tibble: 10 × 2
#>    f            n
#>    <fct>    <int>
#>  1 Level 1      3
#>  2 Level 7      4
#>  3 Level 6      4
#>  4 Level 5      4
#>  5 Level 9      5
#>  6 Level 8      5
#>  7 Level 4      5
#>  8 Level 3      5
#>  9 Level 2      6
#> 10 Level 10     8
```
