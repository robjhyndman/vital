# Extract age components from a model

For a mable with a single model column, return the model components that
are indexed by age.

## Usage

``` r
age_components(object, ...)
```

## Arguments

- object:

  A vital mable object with a single model column.

- ...:

  Not currently used.

## Value

vital object containing the age components from the model.

## Examples

``` r
norway_mortality |>
  dplyr::filter(Sex == "Female") |>
  model(lee_carter = LC(log(Mortality))) |>
  age_components()
#> # A tibble: 111 × 4
#>    Sex      Age    ax     bx
#>    <chr>  <int> <dbl>  <dbl>
#>  1 Female     0 -4.33 0.0152
#>  2 Female     1 -6.16 0.0219
#>  3 Female     2 -6.77 0.0189
#>  4 Female     3 -7.14 0.0184
#>  5 Female     4 -7.18 0.0162
#>  6 Female     5 -7.41 0.0171
#>  7 Female     6 -7.45 0.0162
#>  8 Female     7 -7.48 0.0152
#>  9 Female     8 -7.37 0.0122
#> 10 Female     9 -7.39 0.0122
#> # ℹ 101 more rows
```
