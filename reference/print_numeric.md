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

Data frame with descriptive statistics for each variable.

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
