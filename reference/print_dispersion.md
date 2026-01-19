# Prints the dispersion of a numeric vector

Calculates and prints the median and interquartile range (IQR) or the
mean and standard deviation (SD).

## Usage

``` r
print_dispersion(x, digits = 1, width = 15, method = "median")
```

## Arguments

- x:

  Vector containing numerical values.

- digits:

  Integer specifying the number of decimal places for the test
  statistic.

- width:

  Integer specifying the maximum width for wrapping text.

- method:

  Character specifying the method: `median` for median and IQR, or
  `mean` for mean and SD.

## Value

A character string containing a measure of central tendency and
dispersion. Depending on `method`, this is either the median and
interquartile range or the mean and standard deviation.

## Examples

``` r
print_dispersion(runif(10))
#> [1] "0.5+/-0.5"
print_dispersion(runif(10), method = "mean", digits = 2, width = 5)
#> [1] "0.33\n+/-0.32"
```
