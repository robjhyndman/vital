# Compute period life tables from age-specific mortality rates

All available years and ages are included in the tables. \$qx = mx/(1 +
((1-ax) \* mx))\$ as per Chiang (1984). Warning: the code has only been
tested for data based on single-year age groups.

## Usage

``` r
life_table(.data, mortality)
```

## Arguments

- .data:

  A `vital` including an age variable and a variable containing
  mortality rates.

- mortality:

  Variable in `.data` containing Mortality rates (mx). If omitted, the
  variable with name `mx`, `Mortality` or `Rate` will be used (not case
  sensitive).

## Value

A vital object containing the index, keys, and the new life table
variables `mx`, `qx`, `lx`, `dx`, `Lx`, `Tx` and `ex`.

## References

Chiang CL. (1984) *The life table and its applications*. Robert E
Krieger Publishing Company: Malabar.

Keyfitz, N, and Caswell, H. (2005) *Applied mathematical demography*,
Springer-Verlag: New York.

Preston, S.H., Heuveline, P., and Guillot, M. (2001) *Demography:
measuring and modeling population processes*. Blackwell

## Author

Rob J Hyndman

## Examples

``` r
# Compute Norwegian life table for females in 2003
norway_mortality |>
  dplyr::filter(Sex == "Female", Year == 2003) |>
  life_table()
#> # A vital: 111 x 13 [?]
#> # Key:     Age x Sex [111 x 1]
#>     Year   Age Sex          mx        qx    lx        dx    Lx    Tx    ex    rx
#>    <int> <int> <chr>     <dbl>     <dbl> <dbl>     <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  2003     0 Female 0.00309  0.00308   1     0.00308   0.997  81.9  81.9 0.997
#>  2  2003     1 Female 0.000434 0.000434  0.997 0.000433  0.997  80.9  81.2 1.000
#>  3  2003     2 Female 0.000278 0.000278  0.996 0.000277  0.996  79.9  80.2 1.000
#>  4  2003     3 Female 0.000136 0.000136  0.996 0.000135  0.996  78.9  79.2 1.000
#>  5  2003     4 Female 0.00024  0.000240  0.996 0.000239  0.996  77.9  78.2 1.000
#>  6  2003     5 Female 0.000034 0.0000340 0.996 0.0000339 0.996  76.9  77.3 1.000
#>  7  2003     6 Female 0.000099 0.0000990 0.996 0.0000986 0.996  76.0  76.3 1.000
#>  8  2003     7 Female 0.0001   0.0001000 0.996 0.0000996 0.996  75.0  75.3 1.000
#>  9  2003     8 Female 0.000166 0.000166  0.996 0.000165  0.996  74.0  74.3 1.000
#> 10  2003     9 Female 0.000033 0.0000330 0.995 0.0000328 0.995  73.0  73.3 1.000
#> # ℹ 101 more rows
#> # ℹ 2 more variables: nx <dbl>, ax <dbl>
```
