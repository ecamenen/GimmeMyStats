# Frequency of categorical variables

Formats a data frame or vector containing a multinomial (multi-level)
variable and calculates the frequency of each levels.

## Usage

``` r
count_category(x, width = 15, collapse = FALSE, sort = TRUE, format = TRUE)
```

## Arguments

- x:

  Either a character or factor vector, or a data frame of numerical
  values. For the latter, each column represents the absence (0) or
  presence (1) for each level of a categorical variable.

- width:

  Integer specifying the maximum width for wrapping text.

- collapse:

  Logical specifying whether to merge levels with identical proportions.

- sort:

  Logical or character vector. If `TRUE`, orders levels by frequency. If
  `FALSE`, orders by names. If a vector, renames and orders levels
  accordingly.

- format:

  Logical specifying whether to format level names if the input is a
  vector.

## Value

A tibble with one row per level and the following columns:

- f:

  Factor specifying the level labels, possibly wrapped to the specified
  width. When `collapse = TRUE`, multiple levels with identical
  frequencies are merged into a single label separated by commas.

- n:

  Integer specifying the frequency count for each level.

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
df <- table(seq_along(x), factor(x, levels = paste("Level", seq(k)))) %>%
as.data.frame.matrix()
count_category(df)
#> # A tibble: 5 × 2
#>   f           n
#>   <fct>   <int>
#> 1 Level 5     1
#> 2 Level 4     2
#> 3 Level 1     2
#> 4 Level 3     6
#> 5 Level 2     9
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
