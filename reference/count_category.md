# Frequency of categorical variables

Formats a data frame or vector containing categorical variables and
calculates the frequency of each category.

## Usage

``` r
count_category(x, width = 15, collapse = FALSE, sort = TRUE, format = TRUE)
```

## Arguments

- x:

  Data frame or vector containing categorical variables.

- width:

  Integer specifying the maximum width for wrapping text.

- collapse:

  Logical specifying whether to merge categories with identical
  proportions.

- sort:

  Logical or character vector. If `TRUE`, orders categories by
  frequency. If `FALSE`, orders by names. If a character vector, renames
  and orders categories accordingly.

- format:

  Logical specifying whether to format category names if the input is a
  vector.

## Value

A tibble with one row per category and the following columns:

- f:

  Factor specifying the category labels, possibly wrapped to the
  specified width. When `collapse = TRUE`, multiple categories with
  identical frequencies are merged into a single label separated by
  commas.

- n:

  Integer specifying the frequency count for each category.

## Examples

``` r
# Vector of categorical variable
k <- 5
n <- runif(k, 1, 10) %>% round()
x <- paste("Level", seq(k)) %>%
    mapply(function(x, y) rep(x, y), ., n) %>%
    unlist()
count_category(x)
#> # A tibble: 5 × 2
#>   f           n
#>   <fct>   <int>
#> 1 Level 5     1
#> 2 Level 4     2
#> 3 Level 1     2
#> 4 Level 3     6
#> 5 Level 2     9

# Data frame of categorical variable
df <- sapply(seq(k), function(x) runif(10) %>% round()) %>% as.data.frame()
colnames(df) <- paste("Level", seq(k))
count_category(df)
#> # A tibble: 5 × 2
#>   f           n
#>   <fct>   <int>
#> 1 Level 1     3
#> 2 Level 2     4
#> 3 Level 5     5
#> 4 Level 4     5
#> 5 Level 3     5
count_category(x, sort = FALSE, width = 5)
#> # A tibble: 5 × 2
#>   f              n
#>   <fct>      <int>
#> 1 "Level\n1"     2
#> 2 "Level\n2"     9
#> 3 "Level\n3"     6
#> 4 "Level\n4"     2
#> 5 "Level\n5"     1
count_category(x, sort = seq(k), format = FALSE)
#> # A tibble: 5 × 2
#>   f         n
#>   <fct> <int>
#> 1 1         2
#> 2 2         9
#> 3 3         6
#> 4 4         2
#> 5 5         1
x2 <- c(x, rep("Level 6", n[1]))
count_category(x2, collapse = TRUE)
#> # A tibble: 4 × 2
#>   f                                n
#>   <fct>                        <int>
#> 1 "Level 5"                        1
#> 2 "Level 6, Level\n4, Level 1"     2
#> 3 "Level 3"                        6
#> 4 "Level 2"                        9
```
