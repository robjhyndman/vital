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
#>  1 Female     0 -4.33 0.0148
#>  2 Female     1 -6.16 0.0213
#>  3 Female     2 -6.88 0.0193
#>  4 Female     3 -7.20 0.0186
#>  5 Female     4 -7.35 0.0173
#>  6 Female     5 -7.53 0.0175
#>  7 Female     6 -7.63 0.0173
#>  8 Female     7 -7.73 0.0169
#>  9 Female     8 -7.75 0.0160
#> 10 Female     9 -7.83 0.0164
#> # ℹ 101 more rows
```
