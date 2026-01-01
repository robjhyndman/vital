# Compute life expectancy from age-specific mortality rates

Returns remaining life expectancy at a given age (0 by default).

## Usage

``` r
life_expectancy(.data, from_age = 0, mortality)
```

## Arguments

- .data:

  A vital object including an age variable and a variable containing
  mortality rates.

- from_age:

  Age at which life expectancy to be calculated. Either a scalar or a
  vector of ages.

- mortality:

  Variable in `.data` containing Mortality rates (mx). If omitted, the
  variable with name `mx`, `Mortality` or `Rate` will be used (not case
  sensitive).

## Value

A `vital` object with life expectancy in column `ex`.

## References

Chiang CL. (1984) *The life table and its applications*. Robert E
Krieger Publishing Company: Malabar.

Keyfitz, N, and Caswell, H. (2005) *Applied Mathematical Demography*,
Springer-Verlag: New York.

Preston, S.H., Heuveline, P., and Guillot, M. (2001) *Demography:
measuring and modeling population processes*. Blackwell

## See also

[`life_table()`](https://pkg.robjhyndman.com/vital/reference/life_table.md)

## Author

Rob J Hyndman

## Examples

``` r
# Compute Norwegian life expectancy for females over time
norway_mortality |>
  dplyr::filter(Sex == "Female") |>
  life_expectancy()
#> # A vital: 124 x 7 [1Y]
#> # Key:     Age x Sex [1 x 1]
#>     Year   Age Sex       ex    rx    nx    ax
#>    <int> <int> <chr>  <dbl> <dbl> <dbl> <dbl>
#>  1  1900     0 Female  55.2 0.946     1 0.262
#>  2  1901     0 Female  56.4 0.946     1 0.264
#>  3  1902     0 Female  58.0 0.953     1 0.224
#>  4  1903     0 Female  56.5 0.951     1 0.233
#>  5  1904     0 Female  57.3 0.954     1 0.218
#>  6  1905     0 Female  56.2 0.949     1 0.243
#>  7  1906     0 Female  58.1 0.955     1 0.211
#>  8  1907     0 Female  57.9 0.958     1 0.200
#>  9  1908     0 Female  57.6 0.952     1 0.227
#> 10  1909     0 Female  58.9 0.954     1 0.220
#> # ℹ 114 more rows
```
