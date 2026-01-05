# Add P-value Significance Symbols

Redefine the default parameters of
[`rstatix::add_significance()`](https://rpkgs.datanovia.com/rstatix/reference/add_significance.html)
by adding p-value significance symbols to a data frame.

## Usage

``` r
add_significance0(data, p.col = NULL, output.col = NULL)
```

## Arguments

- data:

  a data frame containing a p-value column.

- p.col:

  column name containing p-values.

- output.col:

  the output column name to hold the adjusted p-values.

## Value

a data frame

## Examples

``` r
library(magrittr)
library(rstatix)
#> 
#> Attaching package: ‘rstatix’
#> The following object is masked from ‘package:GimmeMyStats’:
#> 
#>     identify_outliers
#> The following object is masked from ‘package:stats’:
#> 
#>     filter
data("ToothGrowth")
ToothGrowth %>%
  t_test(len ~ dose) %>%
  adjust_pvalue() %>%
  add_significance0("p.adj")
#> # A tibble: 3 × 10
#>   .y.   group1 group2    n1    n2 statistic    df        p    p.adj p.adj.signif
#>   <chr> <chr>  <chr>  <int> <int>     <dbl> <dbl>    <dbl>    <dbl> <chr>       
#> 1 len   0.5    1         20    20     -6.48  38.0 1.27e- 7 2.54e- 7 ***         
#> 2 len   0.5    2         20    20    -11.8   36.9 4.40e-14 1.32e-13 ***         
#> 3 len   1      2         20    20     -4.90  37.1 1.91e- 5 1.91e- 5 ***         
```
