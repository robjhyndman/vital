# Extract time components from a model

For a mable with a single model column, return the model components that
are indexed by time.

## Usage

``` r
time_components(object, ...)
```

## Arguments

- object:

  A vital mable object with a single model column.

- ...:

  Not currently used.

## Value

tsibble object containing the time components from the model.

## Examples

``` r
norway_mortality |>
  dplyr::filter(Sex == "Female") |>
  model(lee_carter = LC(log(Mortality))) |>
  time_components()
#> # A tsibble: 124 x 3 [1Y]
#> # Key:       Sex [1]
#>    Sex     Year    kt
#>    <chr>  <int> <dbl>
#>  1 Female  1900  117.
#>  2 Female  1901  111.
#>  3 Female  1902  105.
#>  4 Female  1903  110.
#>  5 Female  1904  108.
#>  6 Female  1905  112.
#>  7 Female  1906  103.
#>  8 Female  1907  108.
#>  9 Female  1908  107.
#> 10 Female  1909  101.
#> # ℹ 114 more rows
```
