# Interpolate missing values using a vital model

Uses a fitted vital model to interpolate missing values from a dataset.

## Usage

``` r
# S3 method for class 'mdl_vtl_df'
interpolate(object, new_data, ...)
```

## Arguments

- object:

  A mable containing a single model column.

- new_data:

  A dataset with the same structure as the data used to fit the model.

- ...:

  Other arguments passed to interpolate methods.

## Value

A vital object with missing values interpolated.

## Author

Rob J Hyndman

## Examples

``` r
nor_female <- norway_mortality |>
  dplyr::filter(Sex == "Female")
nor_female |>
  model(mean = FMEAN(Mortality)) |>
  interpolate(nor_female)
#> # A vital: 13,764 x 7 [1Y]
#> # Key:     Age x Sex [111 x 1]
#>     Year   Age OpenInterval Sex    Population Deaths Mortality
#>    <int> <int> <lgl>        <chr>       <dbl>  <dbl>     <dbl>
#>  1  1900     0 FALSE        Female      30070 2376.    0.0778 
#>  2  1900     1 FALSE        Female      28960  842     0.0290 
#>  3  1900     2 FALSE        Female      28043  348     0.0123 
#>  4  1900     3 FALSE        Female      27019  216.    0.00786
#>  5  1900     4 FALSE        Female      26854  168.    0.00624
#>  6  1900     5 FALSE        Female      25569  140.    0.00538
#>  7  1900     6 FALSE        Female      25534  108.    0.00422
#>  8  1900     7 FALSE        Female      24314   93.5   0.00376
#>  9  1900     8 FALSE        Female      24979   93.5   0.00380
#> 10  1900     9 FALSE        Female      24428   90     0.00365
#> # ℹ 13,754 more rows
```
