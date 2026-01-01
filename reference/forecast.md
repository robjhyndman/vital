# Produce forecasts from a vital model

The forecast function allows you to produce future predictions of a
vital model, where the response is a function of age. The forecasts
returned contain both point forecasts and their distribution.

## Usage

``` r
# S3 method for class 'FDM'
forecast(
  object,
  new_data = NULL,
  h = NULL,
  point_forecast = list(.mean = mean),
  simulate = FALSE,
  bootstrap = FALSE,
  times = 5000,
  ...
)

# S3 method for class 'LC'
forecast(
  object,
  new_data = NULL,
  h = NULL,
  point_forecast = list(.mean = mean),
  simulate = FALSE,
  bootstrap = FALSE,
  times = 5000,
  ...
)

# S3 method for class 'GAPC'
forecast(
  object,
  new_data = NULL,
  h = NULL,
  point_forecast = list(.mean = mean),
  simulate = FALSE,
  bootstrap = FALSE,
  times = 5000,
  ...
)

# S3 method for class 'FMEAN'
forecast(
  object,
  new_data = NULL,
  h = NULL,
  point_forecast = list(.mean = mean),
  simulate = FALSE,
  bootstrap = FALSE,
  times = 5000,
  ...
)

# S3 method for class 'FNAIVE'
forecast(
  object,
  new_data = NULL,
  h = NULL,
  point_forecast = list(.mean = mean),
  simulate = FALSE,
  bootstrap = FALSE,
  times = 5000,
  ...
)

# S3 method for class 'mdl_vtl_df'
forecast(
  object,
  new_data = NULL,
  h = NULL,
  point_forecast = list(.mean = mean),
  simulate = FALSE,
  bootstrap = FALSE,
  times = 5000,
  ...
)
```

## Arguments

- object:

  A mable containing one or more models.

- new_data:

  A `tsibble` containing future information used to forecast.

- h:

  Number of time steps ahead to forecast. This can be used instead of
  `new_data` when there are no covariates in the model. It is ignored if
  `new_data` is provided.

- point_forecast:

  A list of functions used to compute point forecasts from the forecast
  distribution.

- simulate:

  If `TRUE`, then forecast distributions are computed using simulation
  from a parametric model.

- bootstrap:

  If `TRUE`, then forecast distributions are computed using simulation
  with resampling.

- times:

  The number of sample paths to use in estimating the forecast
  distribution when `bootstrap = TRUE`.

- ...:

  Additional arguments passed to the specific model method.

## Value

A fable containing the following columns:

- `.model`: The name of the model used to obtain the forecast. Taken
  from the column names of models in the provided mable.

- The forecast distribution. The name of this column will be the same as
  the dependent variable in the model(s). If multiple dependent
  variables exist, it will be named `.distribution`.

- Point forecasts computed from the distribution using the functions in
  the `point_forecast` argument.

- All columns in `new_data`, excluding those whose names conflict with
  the above.

## Author

Rob J Hyndman and Mitchell O'Hara-Wild

## Examples

``` r
norway_mortality |>
  dplyr::filter(Sex == "Female") |>
  model(naive = FNAIVE(Mortality)) |>
  forecast(h = 10)
#> # A vital fable: 1,110 x 6 [1Y]
#> # Key:           Age x (Sex, .model) [111 x 1]
#>    Sex    .model  Year   Age
#>    <chr>  <chr>  <dbl> <int>
#>  1 Female naive   2024     0
#>  2 Female naive   2025     0
#>  3 Female naive   2026     0
#>  4 Female naive   2027     0
#>  5 Female naive   2028     0
#>  6 Female naive   2029     0
#>  7 Female naive   2030     0
#>  8 Female naive   2031     0
#>  9 Female naive   2032     0
#> 10 Female naive   2033     0
#> # ℹ 1,100 more rows
#> # ℹ 2 more variables: Mortality <dist>, .mean <dbl>
```
