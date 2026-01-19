# Convert Strings to Title Case

Converts the first character of each string to uppercase and the rest to
lowercase.

## Usage

``` r
to_title(x)
```

## Arguments

- x:

  A character vector or a list containing strings to convert to title
  case.

## Value

A character vector with the same length as `x`, where each element has
its first character converted to uppercase and remaining characters are
preserved as-is.

## Examples

``` r
to_title(c("hELLO", "WoRLD", "R"))
#> [1] "HELLO" "WoRLD" "R"    
# Returns: "Hello" "World" "R"
```
