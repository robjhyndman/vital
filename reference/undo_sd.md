# Undo a mean/difference transformation

Make a new vital from means and differences of a measured variable by a
key variable. The most common use case of this function is for computing
migration numbers by sex, from the sex differences and mean of the
numbers.

## Usage

``` r
undo_sd(.data, .var, key = Sex, times = 2000)
```

## Arguments

- .data:

  A vital object

- .var:

  A bare variable name of the measured variable to use.

- key:

  A bare variable name specifying the key variable to use. This key
  variable must include the value `geometric_mean`.

- times:

  When the variable is a distribution, the product must be computed by
  simulation. This argument specifies the number of simulations to use.

## Value

A vital object

## References

Hyndman, R.J., Booth, H., & Yasmeen, F. (2013). Coherent mortality
forecasting: the product-ratio method with functional time series
models. *Demography*, 50(1), 261-283.

## Examples

``` r
# Make sums and differences
mig <- net_migration(norway_mortality, norway_births) |>
  dplyr::filter(Sex != "Total")
sd <- mig |>
  make_sd(NetMigration)
# Undo products and ratios
sd |> undo_sd(NetMigration)
#> # A vital: 27,306 x 6 [1Y]
#> # Key:     Age x Sex [111 x 2]
#>     Year   Age Sex    Population Deaths NetMigration
#>    <dbl> <dbl> <chr>       <dbl>  <dbl>        <dbl>
#>  1  1900    -1 Female      32150 1745.        248.  
#>  2  1900     0 Female      30070 1035.        -86.2 
#>  3  1900     1 Female      28960  594.        222.  
#>  4  1900     2 Female      28043  281.         57.3 
#>  5  1900     3 Female      27019  190.         26.8 
#>  6  1900     4 Female      26854  155.          3.50
#>  7  1900     5 Female      25569  122.          5.37
#>  8  1900     6 Female      25534  102.          4.64
#>  9  1900     7 Female      24314   91.7        -5.27
#> 10  1900     8 Female      24979   92.9       -11.1 
#> # ℹ 27,296 more rows
```
