# Compute total fertility rate from age-specific fertility rates

Total fertility rate is the expected number of babies per woman in a
life-time given the fertility rate at each age of a woman's life.

## Usage

``` r
total_fertility_rate(.data, fertility)
```

## Arguments

- .data:

  A vital object including an age variable and a variable containing
  fertility rates.

- fertility:

  Variable in `.data` containing fertility rates. If omitted, the
  variable with name `fx`, `Fertility` or `Rate` will be used (not case
  sensitive).

## Value

A vital object with total fertility in column `tfr`.

## Author

Rob J Hyndman

## Examples

``` r
# Compute Norwegian total fertility rates over time
norway_fertility |>
  total_fertility_rate()
#> # A vital: 56 x 2 [1Y]
#>     Year   tfr
#>    <int> <dbl>
#>  1  1967  2.78
#>  2  1968  2.75
#>  3  1969  2.70
#>  4  1970  2.51
#>  5  1971  2.49
#>  6  1972  2.38
#>  7  1973  2.23
#>  8  1974  2.13
#>  9  1975  1.98
#> 10  1976  1.86
#> # ℹ 46 more rows
```
