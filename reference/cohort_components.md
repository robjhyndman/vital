# Extract cohort components from a model

For a mable with a single model column, return the model components that
are indexed by birth year of the cohort.

## Usage

``` r
cohort_components(object, ...)
```

## Arguments

- object:

  A vital mable object with a single model column.

- ...:

  Not currently used.

## Value

tsibble object containing the cohort components from the model.

## Examples

``` r
norway_mortality |>
  dplyr::filter(Sex == "Male", Age > 50, Year > 1960) |>
  model(apc = APC(Mortality)) |>
  cohort_components()
#> Warning: StMoMo: 22 data points have 
#>                   non-positive exposures and have been zero weighted
#> # A tsibble: 122 x 3 [1Y]
#> # Key:       Sex [1]
#>    Sex   Birth_Year     gc
#>    <chr>      <int>  <dbl>
#>  1 Male        1851 NA    
#>  2 Male        1852 NA    
#>  3 Male        1853 NA    
#>  4 Male        1854 NA    
#>  5 Male        1855 -0.585
#>  6 Male        1856 NA    
#>  7 Male        1857 -7.97 
#>  8 Male        1858 NA    
#>  9 Male        1859 -0.543
#> 10 Male        1860  0.646
#> # ℹ 112 more rows
```
