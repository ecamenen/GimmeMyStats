# Identifies outliers in a numeric vector

Detects outliers using methods like IQR, percentiles, Hampel, MAD, or
SD.

## Usage

``` r
identify_outliers(
  x,
  probabilities = c(0.25, 0.75),
  method = "iqr",
  weight = 1.5,
  replace = FALSE
)
```

## Arguments

- x:

  Numeric vector.

- probabilities:

  Numeric vector specifying probabilities for percentiles.

- method:

  Character specifying the method: `iqr`, `percentiles`, `hampel`,
  `mad`, or `sd`.

- weight:

  Numeric value specifying the multiplier for the detection threshold.

- replace:

  Logical indicating whether to replace outliers with `NA`.

## Value

Numeric vector with outliers replaced by `NA` or their indices.

## Examples

``` r
x <- rnorm(100)
identify_outliers(x, method = "iqr")
#> Error in identify_outliers(x, method = "iqr"): data should be a data frame
```
