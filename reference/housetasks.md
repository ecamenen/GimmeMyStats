# Household tasks distribution by gender and arrangement

A dataset containing the distribution of household tasks among different
arrangements: Wife, Alternating, Husband, and Jointly. The data
represents the frequency of each task performed by each arrangement.

## Usage

``` r
data(housetasks)
```

## Format

A `data.frame` with 13 rows (tasks) and 4 columns (arrangements):

- Wife:

  Numeric, the frequency of the task performed primarily by the wife.

- Alternating:

  Numeric, the frequency of the task performed in an alternating manner.

- Husband:

  Numeric, the frequency of the task performed primarily by the husband.

- Jointly:

  Numeric, the frequency of the task performed jointly by both partners.

## Source

The dataset was downloaded from the `ggpubr` GitHub repository:
<https://raw.githubusercontent.com/kassambara/ggpubr/refs/heads/master/inst/demo-data/housetasks.txt>

## Examples

``` r
data(housetasks)
head(housetasks)
#>            Wife Alternating Husband Jointly
#> Laundry     156          14       2       4
#> Main_meal   124          20       5       4
#> Dinner       77          11       7      13
#> Breakfeast   82          36      15       7
#> Tidying      53          11       1      57
#> Dishes       32          24       4      53
```
