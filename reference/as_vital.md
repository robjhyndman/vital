# Coerce to a vital object

A vital object is a type of tsibble that contains vital statistics such
as births, deaths, and population counts, and mortality and fertility
rates. It is a tsibble with a special class that allows for special
methods to be used. The object has an attribute that stores variables
names needed for some functions, including age, sex, births, deaths and
population.

## Usage

``` r
as_vital(x, ...)

# S3 method for class 'demogdata'
as_vital(x, sex_groups = TRUE, ...)

# S3 method for class 'tbl_ts'
as_vital(
  x,
  .age = NULL,
  .sex = NULL,
  .deaths = NULL,
  .births = NULL,
  .population = NULL,
  reorder = FALSE,
  ...
)

# S3 method for class 'data.frame'
as_vital(
  x,
  key = NULL,
  index,
  .age = NULL,
  .sex = NULL,
  .deaths = NULL,
  .births = NULL,
  .population = NULL,
  reorder = TRUE,
  ...
)
```

## Arguments

- x:

  Object to be coerced to a vital format.

- ...:

  Other arguments passed to
  [`tsibble::as_tsibble()`](https://tsibble.tidyverts.org/reference/as-tsibble.html)

- sex_groups:

  Logical variable indicating if the groups denote sexes

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

- reorder:

  Logical indicating if the variables should be reordered.

- key:

  Variable(s) that uniquely determine time indices. NULL for empty key,
  and [`c()`](https://rdrr.io/r/base/c.html) for multiple variables. It
  works with tidy selector (e.g.
  [`tidyselect::starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html)).

- index:

  A variable to specify the time index variable.

## Value

A tsibble with class `vital`.

## See also

[`tsibble::tsibble()`](https://tsibble.tidyverts.org/reference/tsibble.html)

## Author

Rob J Hyndman

## Examples

``` r
# coerce demogdata object to vital
as_vital(demography::fr.mort)
#> # A vital: 63,603 x 7 [1Y]
#> # Key:     Age x (AgeGroup, Sex) [111 x 3]
#>     Year AgeGroup   Age Sex    Mortality Exposure Deaths
#>    <int> <chr>    <int> <chr>      <dbl>    <dbl>  <dbl>
#>  1  1816 0            0 female   0.187    408224. 76332.
#>  2  1816 1            1 female   0.0467   382452. 17861.
#>  3  1816 2            2 female   0.0339   351454. 11924.
#>  4  1816 3            3 female   0.0229   337733.  7738.
#>  5  1816 4            4 female   0.0160   331576.  5304.
#>  6  1816 5            5 female   0.0138   313554.  4338.
#>  7  1816 6            6 female   0.0121   306853.  3714.
#>  8  1816 7            7 female   0.0104   301860.  3148.
#>  9  1816 8            8 female   0.00891  296639.  2642.
#> 10  1816 9            9 female   0.00760  289057.  2195.
#> # ℹ 63,593 more rows
# create a vital with only age as a key
data.frame(
  year = rep(2010:2015, 100),
  age = rep(0:99, each = 6),
  mx = runif(600, 0, 1)
) |>
  as_vital(
    index = year,
    key = age,
    .age = "age"
  )
#> # A vital: 600 x 3 [1Y]
#> # Key:     age [100 x 1]
#>     year   age     mx
#>    <int> <int>  <dbl>
#>  1  2010     0 0.402 
#>  2  2010     1 0.290 
#>  3  2010     2 0.0514
#>  4  2010     3 0.301 
#>  5  2010     4 0.180 
#>  6  2010     5 0.0960
#>  7  2010     6 0.461 
#>  8  2010     7 0.204 
#>  9  2010     8 0.570 
#> 10  2010     9 0.545 
#> # ℹ 590 more rows
```
