# Plot forecasts from a vital model

Produces a plot showing forecasts obtained from a model applied to a
vital object.

## Usage

``` r
# S3 method for class 'fbl_vtl_ts'
autoplot(object, ...)
```

## Arguments

- object:

  A fable object obtained from a vital model.

- ...:

  Further arguments ignored.

## Value

A ggplot2 object.

## Author

Rob J Hyndman

## Examples

``` r
library(ggplot2)
norway_mortality |>
  model(ave = FMEAN(Mortality)) |>
  forecast(h = 10) |>
  autoplot() + scale_y_log10()

```
