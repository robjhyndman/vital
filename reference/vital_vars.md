# Return vital variables

A vital object is a special case of a tsibble object with additional
attributes identifying the age, sex, deaths, births and population
variables. `vital_vars()` returns a character vector the names of the
vital variables.

## Usage

``` r
vital_vars(x)
```

## Arguments

- x:

  A tsibble object.

## Value

A character vector of the names of the vital variables.

## Examples

``` r
vital_vars(norway_mortality)
#>          age          sex       deaths   population 
#>        "Age"        "Sex"     "Deaths" "Population" 
```
