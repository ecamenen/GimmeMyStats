# Prints a hypothesis test

Formats the results of a hypothesis test (ANOVA, Kruskal-Wallis, or
Wilcoxon).

## Usage

``` r
print_test(x, digits = 0, digits_p = 2)
```

## Arguments

- x:

  Test object from `rstatix` among `anova_test`, `kruskal_test`, or
  `wilcox_test`.

- digits:

  Integer specifying the number of decimal places for the test
  statistic.

- digits_p:

  Integer specifying the number of decimal places for the p-value.

## Examples

``` r
library(rstatix)
data("ToothGrowth")
res <- anova_test(ToothGrowth, len ~ dose)
print_test(res)
#> [1] "Anova, F(1, 58) = 105, p < 0.001***"

res <- kruskal_test(ToothGrowth, len ~ dose)
print_test(res)
#> [1] "Kruskal-Wallis, K(2) = 41, p < 0.001***"

res <- wilcox_test(ToothGrowth, len ~ supp)
print_test(res)
#> [1] "Wilcoxon, W = 576, p = 0.06"

library(lmerTest)
#> Loading required package: lme4
#> Loading required package: Matrix
#> 
#> Attaching package: ‘Matrix’
#> The following objects are masked from ‘package:tidyr’:
#> 
#>     expand, pack, unpack
#> 
#> Attaching package: ‘lmerTest’
#> The following object is masked from ‘package:lme4’:
#> 
#>     lmer
#> The following object is masked from ‘package:stats’:
#> 
#>     step
data("sleepstudy", package = "lme4")
res <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
print_test(res)
#> [1] "Lmer, T(1, 17) = 46, p < 0.001***"
```
