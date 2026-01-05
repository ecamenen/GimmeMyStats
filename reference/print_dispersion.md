# Prints the dispersion of a numeric vector

Calculates and prints the median and interquartile range (IQR) or the
mean and standard deviation (SD).

## Usage

``` r
print_dispersion(x, digits = 1, method = "median", width = 10)
```

## Arguments

- x:

  Numeric vector.

- digits:

  Integer specifying the number of decimal places for the test
  statistic.

- method:

  Character specifying the method: `median` for median and IQR, or
  `mean` for mean and SD.

- width:

  Integer specifying the maximum width for wrapping text.

## Examples

``` r
print_dispersion(runif(10))
#> [1] "0.4+/-0.4"
print_dispersion(runif(10), method = "mean", digits = 2)
#> [1] "0.56\n+/-0.29"
```
