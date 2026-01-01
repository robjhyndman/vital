# Rainbow plot of demographic data against age

Produce rainbow plot (coloured by time index) of demographic variable
against against age.

## Usage

``` r
# S3 method for class 'vital'
autoplot(object, .vars = NULL, age = age_var(object), ...)
```

## Arguments

- object:

  A vital including an age variable and the variable you wish to plot.

- .vars:

  The name of the variable you wish to plot.

- age:

  The name of the age variable. If not supplied, the function will
  attempt to find it.

- ...:

  Further arguments not used.

## Value

A ggplot2 object.

## References

Hyndman, Rob J & Shang, Han Lin (2010) Rainbow plots, bagplots, and
boxplots for functional data. *Journal of Computational and Graphical
Statistics*, 19(1), 29-45.
<https://robjhyndman.com/publications/rainbow-fda/>

## Author

Rob J Hyndman

## Examples

``` r
autoplot(norway_fertility, Fertility)
```
