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
#>  1 Female  1900  121.
#>  2 Female  1901  114.
#>  3 Female  1902  108.
#>  4 Female  1903  114.
#>  5 Female  1904  111.
#>  6 Female  1905  115.
#>  7 Female  1906  106.
#>  8 Female  1907  111.
#>  9 Female  1908  110.
#> 10 Female  1909  104.
#> # ℹ 114 more rows
```
