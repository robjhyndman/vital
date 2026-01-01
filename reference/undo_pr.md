# Undo a product/ratio transformation

Make a new vital from products and ratios of a measured variable by a
key variable. The most common use case of this function is for computing
mortality rates by sex, from the sex ratios and geometric mean of the
rates.

## Usage

``` r
undo_pr(.data, .var, key = Sex, times = 2000)
```

## Arguments

- .data:

  A vital object

- .var:

  A bare variable name of the measured variable to use.

- key:

  A bare variable name specifying the key variable to use. This key
  variable must include the value `geometric_mean`.

- times:

  When the variable is a distribution, the product must be computed by
  simulation. This argument specifies the number of simulations to use.

## Value

A vital object

## Details

Note that when a measured variable takes value 0, the geometric mean is
set to 10^-6 to avoid infinite values in the ratio. Therefore, when the
transformation is undone, the results will not be identical to the
original in the case that the original data was 0.

## References

Hyndman, R.J., Booth, H., & Yasmeen, F. (2013). Coherent mortality
forecasting: the product-ratio method with functional time series
models. *Demography*, 50(1), 261-283.

## Examples

``` r
# Make products and ratios
orig_data <- norway_mortality |>
  dplyr::filter(Year > 2015, Sex != "Total")
pr <- orig_data |>
  make_pr(Mortality)
# Compare original data with product/ratio version
orig_data
#> # A vital: 1,776 x 7 [1Y]
#> # Key:     Age x Sex [111 x 2]
#>     Year   Age OpenInterval Sex    Population Deaths Mortality
#>    <int> <int> <lgl>        <chr>       <dbl>  <dbl>     <dbl>
#>  1  2016     0 FALSE        Female      28851     57  0.00197 
#>  2  2016     1 FALSE        Female      29195      7  0.00024 
#>  3  2016     2 FALSE        Female      29631      6  0.000203
#>  4  2016     3 FALSE        Female      30416      2  0.000066
#>  5  2016     4 FALSE        Female      30582      0  0       
#>  6  2016     5 FALSE        Female      31567      1  0.000032
#>  7  2016     6 FALSE        Female      31963      2  0.000063
#>  8  2016     7 FALSE        Female      31318      2  0.000063
#>  9  2016     8 FALSE        Female      30710      0  0       
#> 10  2016     9 FALSE        Female      30882      1  0.000032
#> # ℹ 1,766 more rows
pr
#> # A vital: 2,664 x 7 [1Y]
#> # Key:     Age x Sex [111 x 3]
#>     Year   Age OpenInterval Sex    Population Deaths Mortality
#>    <int> <int> <lgl>        <chr>       <dbl>  <dbl>     <dbl>
#>  1  2016     0 FALSE        Female      28851     57     0.924
#>  2  2016     1 FALSE        Female      29195      7     1.36 
#>  3  2016     2 FALSE        Female      29631      6     1.78 
#>  4  2016     3 FALSE        Female      30416      2     1.44 
#>  5  2016     4 FALSE        Female      30582      0     0    
#>  6  2016     5 FALSE        Female      31567      1     0.724
#>  7  2016     6 FALSE        Female      31963      2     1.45 
#>  8  2016     7 FALSE        Female      31318      2     0.841
#>  9  2016     8 FALSE        Female      30710      0     0    
#> 10  2016     9 FALSE        Female      30882      1     1.79 
#> # ℹ 2,654 more rows
# Undo products and ratios
pr |> undo_pr(Mortality)
#> # A vital: 1,776 x 7 [1Y]
#> # Key:     Age x Sex [111 x 2]
#>     Year   Age OpenInterval Sex    Population Deaths Mortality
#>    <int> <int> <lgl>        <chr>       <dbl>  <dbl>     <dbl>
#>  1  2016     0 FALSE        Female      28851     57  0.00197 
#>  2  2016     1 FALSE        Female      29195      7  0.00024 
#>  3  2016     2 FALSE        Female      29631      6  0.000203
#>  4  2016     3 FALSE        Female      30416      2  0.000066
#>  5  2016     4 FALSE        Female      30582      0  0       
#>  6  2016     5 FALSE        Female      31567      1  0.000032
#>  7  2016     6 FALSE        Female      31963      2  0.000063
#>  8  2016     7 FALSE        Female      31318      2  0.000063
#>  9  2016     8 FALSE        Female      30710      0  0       
#> 10  2016     9 FALSE        Female      30882      1  0.000032
#> # ℹ 1,766 more rows
```
