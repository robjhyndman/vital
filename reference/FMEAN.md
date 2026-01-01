# Functional mean model

`FMEAN()` returns an iid functional model applied to the formula's
response variable as a function of age.

## Usage

``` r
FMEAN(formula, ...)
```

## Arguments

- formula:

  Model specification.

- ...:

  Not used.

## Value

A model specification.

## Author

Rob J Hyndman

## Examples

``` r
fmean <- norway_mortality |>
  dplyr::filter(Sex == "Female") |>
  model(mean = FMEAN(Mortality))
report(fmean)
#> Series: Mortality 
#> Model: FMEAN 
#> 
#> # A tibble: 111 × 3
#>      Age     mean   sigma
#>    <int>    <dbl>   <dbl>
#>  1     0 0.0231   0.0212 
#>  2     1 0.00616  0.00763
#>  3     2 0.00262  0.00324
#>  4     3 0.00179  0.00215
#>  5     4 0.00144  0.00173
#>  6     5 0.00124  0.00152
#>  7     6 0.00109  0.00134
#>  8     7 0.000972 0.00118
#>  9     8 0.000907 0.00111
#> 10     9 0.000879 0.00113
#> # ℹ 101 more rows
autoplot(fmean) + ggplot2::scale_y_log10()
```
