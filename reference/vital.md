# Create a vital object

A vital object is a type of tsibble that contains vital statistics such
as births, deaths, and population counts, and mortality and fertility
rates. It is a tsibble with a special class that allows for special
methods to be used. The object has an attribute that stores variables
names needed for some functions, including age, sex, births, deaths and
population.

## Usage

``` r
vital(
  ...,
  key = NULL,
  index,
  .age = NULL,
  .sex = NULL,
  .deaths = NULL,
  .births = NULL,
  .population = NULL,
  regular = TRUE,
  .drop = TRUE
)
```

## Arguments

- ...:

  A set of name-value pairs

- key:

  Variable(s) that uniquely determine time indices. NULL for empty key,
  and [`c()`](https://rdrr.io/r/base/c.html) for multiple variables. It
  works with tidy selector (e.g.
  [`tidyselect::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html))

- index:

  A variable to specify the time index variable.

- .age:

  Character string with name of age variable

- .sex:

  Character string with name of sex variable

- .deaths:

  Character string with name of deaths variable

- .births:

  Character string with name of births variable

- .population:

  Character string with name of population variable

- regular:

  Regular time interval (`TRUE`) or irregular (`FALSE`). The interval is
  determined by the greatest common divisor of index column, if `TRUE`.

- .drop:

  If `TRUE`, empty key groups are dropped.

## Value

A tsibble with class `vital`.

## See also

[`tsibble::tsibble()`](https://tsibble.tidyverts.org/reference/tsibble.html)

## Author

Rob J Hyndman

## Examples

``` r
# create a vital with only age as a key
vital(
  year = rep(2010:2015, 100),
  age = rep(0:99, each = 6),
  mx = runif(600, 0, 1),
  index = year,
  key = age,
  .age = "age"
)
#> # A vital: 600 x 3 [1Y]
#> # Key:     age [100 x 1]
#>     year   age    mx
#>    <int> <int> <dbl>
#>  1  2010     0 0.680
#>  2  2011     0 0.143
#>  3  2012     0 0.765
#>  4  2013     0 0.627
#>  5  2014     0 0.730
#>  6  2015     0 0.424
#>  7  2010     1 0.268
#>  8  2011     1 0.769
#>  9  2012     1 0.587
#> 10  2013     1 0.899
#> # ℹ 590 more rows
```
