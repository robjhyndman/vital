# Function to smooth mortality rates using MortalityLaw package

This smoothing function allows smoothing of a variable in a vital object
using the MortalityLaw package. The vital object is returned along with
some additional columns containing information about the smoothed
variable: `.smooth` containing the smoothed values, and `.smooth_se`
containing the corresponding standard errors. If `.data` has deaths and
population variables (see
[`vital_vars()`](https://pkg.robjhyndman.com/vital/reference/vital_vars.md)),
the law is fitted to these; otherwise it is fitted to `.var`. The
standard errors are approximate: they are the smoothed values multiplied
by the residual standard deviation of the log rates and by
\\\sqrt{p/n}\\, where \\p\\ is the number of parameters of the law and
\\n\\ is the number of ages with positive rates.

## Usage

``` r
smooth_mortality_law(.data, .var, law = "gompertz", ...)
```

## Arguments

- .data:

  A vital object

- .var:

  name of variable to smooth. This should contain mortality rates.

- law:

  name of mortality law. See
  [`availableLaws`](https://rdrr.io/pkg/MortalityLaws/man/availableLaws.html)
  for the available options. Argument ignored if a custom law supplied.

- ...:

  Additional arguments are passed to
  [`MortalityLaw`](https://rdrr.io/pkg/MortalityLaws/man/MortalityLaw.html).

## Value

vital with added columns containing smoothed values and their standard
errors

## Author

Sixian Tang and Rob J Hyndman

## Examples

``` r
norway_mortality |> smooth_mortality_law(Mortality)
#> # A vital: 41,292 x 9 [1Y]
#> # Key:     Age x Sex [111 x 3]
#>     Year   Age OpenInterval Sex   Population Deaths Mortality .smooth .smooth_se
#>    <int> <int> <lgl>        <chr>      <dbl>  <dbl>     <dbl>   <dbl>      <dbl>
#>  1  1900     0 FALSE        Fema…      30070 2376.    0.0778  0.00221   0.000252
#>  2  1900     1 FALSE        Fema…      28960  842     0.0290  0.00232   0.000264
#>  3  1900     2 FALSE        Fema…      28043  348     0.0123  0.00243   0.000277
#>  4  1900     3 FALSE        Fema…      27019  216.    0.00786 0.00255   0.000290
#>  5  1900     4 FALSE        Fema…      26854  168.    0.00624 0.00267   0.000304
#>  6  1900     5 FALSE        Fema…      25569  140.    0.00538 0.00280   0.000319
#>  7  1900     6 FALSE        Fema…      25534  108.    0.00422 0.00293   0.000334
#>  8  1900     7 FALSE        Fema…      24314   93.5   0.00376 0.00307   0.000351
#>  9  1900     8 FALSE        Fema…      24979   93.5   0.00380 0.00322   0.000367
#> 10  1900     9 FALSE        Fema…      24428   90     0.00365 0.00338   0.000385
#> # ℹ 41,282 more rows
```
