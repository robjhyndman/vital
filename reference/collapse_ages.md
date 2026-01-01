# Collapse upper ages into a single age group. Counts are summed while rates are recomputed where possible.

Collapse upper ages into a single age group. Counts are summed while
rates are recomputed where possible.

## Usage

``` r
collapse_ages(.data, max_age = 100)
```

## Arguments

- .data:

  A vital object including an age variable

- max_age:

  Maximum age to include in the collapsed age group.

## Value

A vital object with the same variables as `.data`, but with the upper
ages collapsed into a single age group.

## Details

If the object includes deaths, population and mortality rates, then
deaths and population are summed and mortality rates are recomputed as
deaths/population. But if the object contains mortality rates but not
deaths and population, then the last rate remains unchanged (and a
warning is generated).

## Author

Rob J Hyndman

## Examples

``` r
norway_mortality |>
  dplyr::filter(Sex == "Female") |>
  collapse_ages(max_age = 85)
#> # A vital: 10,664 x 7 [1Y]
#> # Key:     Age x Sex [86 x 1]
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
#> # ℹ 10,654 more rows
```
