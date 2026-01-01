# Generate responses from a mable

Use a fitted model to simulate future data with similar behaviour to the
response.

## Usage

``` r
# S3 method for class 'mdl_vtl_df'
generate(x, new_data = NULL, h = NULL, bootstrap = FALSE, times = 1, ...)
```

## Arguments

- x:

  A mable.

- new_data:

  Future data needed for generation (should include the time index and
  exogenous regressors)

- h:

  The simulation horizon (can be used instead of `new_data` for regular
  time series with no exogenous regressors).

- bootstrap:

  If `TRUE`, then forecast distributions are computed using simulation
  with resampled errors.

- times:

  The number of replications.

- ...:

  Additional arguments

## Value

A vital object with simulated values.

## Details

Innovations are sampled by the model's assumed error distribution. If
`bootstrap` is `TRUE`, innovations will be sampled from the model's
residuals.

## Author

Rob J Hyndman and Mitchell O'Hara-Wild

## Examples

``` r
norway_mortality |>
  model(lc = LC(Mortality)) |>
  generate(times = 3, bootstrap = TRUE)
#> Warning: 3 errors (1 unique) encountered for lc
#> [3] Lee-Carter models require a log transformation of the response variable.
#> # A vital: 1,998 x 6 [1Y]
#> # Key:     Age x (Sex, .model, .rep) [111 x 9]
#>     Year   Age Sex    .model .rep   .sim
#>    <dbl> <int> <chr>  <chr>  <chr> <dbl>
#>  1  2024     0 Female lc     1        NA
#>  2  2024     1 Female lc     1        NA
#>  3  2024     2 Female lc     1        NA
#>  4  2024     3 Female lc     1        NA
#>  5  2024     4 Female lc     1        NA
#>  6  2024     5 Female lc     1        NA
#>  7  2024     6 Female lc     1        NA
#>  8  2024     7 Female lc     1        NA
#>  9  2024     8 Female lc     1        NA
#> 10  2024     9 Female lc     1        NA
#> # ℹ 1,988 more rows
```
