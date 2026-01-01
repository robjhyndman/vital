# Functions to smooth demographic data

These smoothing functions allow smoothing of a variable in a vital
object. The vital object is returned along with some additional columns
containing information about the smoothed variable: usually `.smooth`
containing the smoothed values, and `.smooth_se` containing the
corresponding standard errors.

## Usage

``` r
smooth_spline(.data, .var, age_spacing = 1, k = -1)

smooth_mortality(.data, .var, age_spacing = 1, b = 65, power = 0.4, k = 30)

smooth_fertility(.data, .var, age_spacing = 1, lambda = 1e-10)

smooth_loess(.data, .var, age_spacing = 1, span = 0.2)
```

## Arguments

- .data:

  A vital object

- .var:

  name of variable to smooth

- age_spacing:

  Spacing between ages for smoothed vital. Default is 1.

- k:

  Number of knots to use for penalized regression spline estimate.

- b:

  Lower age for monotonicity. Above this, the smooth curve is assumed to
  be monotonically increasing.

- power:

  Power transformation for age variable before smoothing. Default is 0.4
  (for mortality data).

- lambda:

  Penalty for constrained regression spline.

- span:

  Span for loess smooth.

## Value

vital with added columns containing smoothed values and their standard
errors

## Details

`smooth_mortality()` use penalized regression splines applied to log
mortality with a monotonicity constraint above age `b`. The methodology
is based on Wood (1994). `smooth_fertility()` uses weighted regression
B-splines with a concavity constraint, based on He and Ng (1999). The
function `smooth_loess()` uses locally quadratic regression, while
`smooth_spline()` uses penalized regression splines.

## References

Hyndman, R.J., and Ullah, S. (2007) Robust forecasting of mortality and
fertility rates: a functional data approach. *Computational Statistics &
Data Analysis*, 51, 4942-4956.
<https://robjhyndman.com/publications/funcfor/>

## Author

Rob J Hyndman

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
norway_mortality |>
  filter(Sex == "Female", Year > 2000) |>
  smooth_mortality(Mortality)
#> # A vital: 2,553 x 9 [1Y]
#> # Key:     Age x Sex [111 x 1]
#>     Year   Age OpenInterval Sex   Population Deaths Mortality .smooth .smooth_se
#>    <int> <dbl> <lgl>        <chr>      <dbl>  <dbl>     <dbl> <dbl[1>  <dbl[1d]>
#>  1  2001     0 FALSE        Fema…      28805     97  0.00343  3.42e-3  0.000311 
#>  2  2001     1 FALSE        Fema…      29090      8  0.000274 2.71e-4  0.0000717
#>  3  2001     2 FALSE        Fema…      28891      2  0.000069 1.64e-4  0.0000393
#>  4  2001     3 FALSE        Fema…      29589      5  0.000172 1.32e-4  0.0000296
#>  5  2001     4 FALSE        Fema…      30155      3  0.0001   1.16e-4  0.0000250
#>  6  2001     5 FALSE        Fema…      29874      3  0.0001   1.07e-4  0.0000224
#>  7  2001     6 FALSE        Fema…      29751      5  0.000167 1.01e-4  0.0000210
#>  8  2001     7 FALSE        Fema…      29812      1  0.000034 9.64e-5  0.0000202
#>  9  2001     8 FALSE        Fema…      29832      4  0.000134 9.45e-5  0.0000200
#> 10  2001     9 FALSE        Fema…      30410      1  0.000033 9.58e-5  0.0000203
#> # ℹ 2,543 more rows
norway_fertility |>
  filter(Year > 2000) |>
  smooth_fertility(Fertility)
#> # A vital: 968 x 6 [1Y]
#> # Key:     Age [44 x 1]
#>     Year   Age Fertility OpenInterval    .smooth .smooth_se
#>    <int> <dbl>     <dbl> <lgl>             <dbl>      <dbl>
#>  1  2001    12   0       TRUE         0.00000601  0.0000299
#>  2  2001    13   0.00004 FALSE        0.0000262   0.000119 
#>  3  2001    14   0.00019 FALSE        0.000114    0.000469 
#>  4  2001    15   0.00046 FALSE        0.000498    0.00184  
#>  5  2001    16   0.00217 FALSE        0.00217     0.00717  
#>  6  2001    17   0.00752 FALSE        0.00751     0.0221   
#>  7  2001    18   0.0158  FALSE        0.0164      0.0426   
#>  8  2001    19   0.0291  FALSE        0.0269      0.0613   
#>  9  2001    20   0.0404  FALSE        0.0391      0.0776   
#> 10  2001    21   0.0502  FALSE        0.0502      0.0864   
#> # ℹ 958 more rows
```
