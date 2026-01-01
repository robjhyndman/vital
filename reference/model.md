# Estimate models for vital data

Trains specified model definition(s) on a dataset. This function will
estimate the a set of model definitions (passed via `...`) to each
series within `.data` (as identified by the key structure). The result
will be a mable (a model table), which neatly stores the estimated
models in a tabular structure. Rows of the data identify different
series within the data, and each model column contains all models from
that model definition. Each cell in the mable identifies a single model.

## Usage

``` r
# S3 method for class 'vital'
model(.data, ..., .safely = TRUE)
```

## Arguments

- .data:

  A vital object including an age variable.

- ...:

  Definitions for the models to be used. All models must share the same
  response variable.

- .safely:

  If a model encounters an error, rather than aborting the process a
  [NULL
  model](https://fabletools.tidyverts.org/reference/null_model.html)
  will be returned instead. This allows for an error to occur when
  computing many models, without losing the results of the successful
  models.

## Value

A mable containing the fitted models.

## Parallel

It is possible to estimate models in parallel using the
[future](https://cran.r-project.org/package=future) package. By
specifying a
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
before estimating the models, they will be computed according to that
plan.

## Progress

Progress on model estimation can be obtained by wrapping the code with
[`progressr::with_progress()`](https://progressr.futureverse.org/reference/with_progress.html).
Further customisation on how progress is reported can be controlled
using the `progressr` package.

## Author

Rob J Hyndman and Mitchell O'Hara-Wild

## Examples

``` r
norway_mortality |>
  dplyr::filter(Sex == "Female") |>
  model(
    naive = FNAIVE(Mortality),
    mean = FMEAN(Mortality)
  )
#> # A mable: 1 x 3
#> # Key:     Sex [1]
#>   Sex       naive    mean
#>   <chr>   <model> <model>
#> 1 Female <FNAIVE> <FMEAN>
```
