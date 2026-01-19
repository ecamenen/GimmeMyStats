# Prints descriptive statistics for numeric variables

Prints summary statistics (mean, median, quartiles, range, etc.) for
numeric variables.

## Usage

``` r
print_numeric(x, digits = 1, width = 15)
```

## Arguments

- x:

  Data frame, matrix, or vector containing numerical variables.

- digits:

  Integer specifying the number of decimal places for the test
  statistic.

- width:

  Integer specifying the maximum width for wrapping text.

## Value

A tibble with one row per numeric variable and the following columns:

- Variables:

  Character specifying the variable name.

- Mean+/-SD:

  Character specifying the mean and standard deviation.

- Median+/-IQR:

  Character specifying the median and interquartile range.

- Q1-Q3:

  Character specifying the first and third quartiles.

- Range:

  Character specifying the minimum and maximum values.

- Kurtosis:

  Numeric specifying the kurtosis coefficient.

- Skewness:

  Numeric specifying the skewness coefficient.

- Normality:

  Character specifying the Shapiro-Wilk normality test significance
  code.

- Zeros:

  Integer specifying the number of zero values.

- NAs:

  Integer specifying the number of missing values.

## Examples

``` r
x <- data.frame(A = rnorm(100), B = rnorm(100))
print_numeric(x)
#> # A tibble: 2 × 10
#>   Variables `Mean+/-SD` `Median+/-IQR` `Q1-Q3` Range Kurtosis Skewness Normality
#>   <chr>     <chr>       <chr>          <chr>   <chr>    <dbl>    <dbl> <chr>    
#> 1 A         0+/-1       -0.2+/-1.3     -0.7;0… -2.8…      0.3        0 ns       
#> 2 B         -0.1+/-0.9  -0.1+/-1.3     -0.8;0… -2.5…      0          0 ns       
#> # ℹ 2 more variables: Zeros <int>, NAs <int>
print_numeric(x, digits = 2, width = 5)
#> # A tibble: 2 × 10
#>   Variables `Mean+/-SD` `Median+/-IQR` `Q1-Q3` Range Kurtosis Skewness Normality
#>   <chr>     <chr>       <chr>          <chr>   <chr>    <dbl>    <dbl> <chr>    
#> 1 A         "-0.05\n+/… "-0.16\n+/-1.… -0.69;… -2.8…     0.33    -0.02 ns       
#> 2 B         "-0.12\n+/… "-0.12\n+/-1.… -0.78;… -2.5…    -0.02    -0.01 ns       
#> # ℹ 2 more variables: Zeros <int>, NAs <int>
```
