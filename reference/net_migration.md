# Calculate net migration from a vital object

Calculate net migration from a vital object

## Usage

``` r
net_migration(deaths, births)
```

## Arguments

- deaths:

  A vital object containing at least a time index, age, population at 1
  January, and death rates.

- births:

  A vital object containing at least a time index and number of births
  per time period. It is assumed that the population variable is the
  same as in the deaths object, and that the same keys other than age
  are present in both objects.

## Value

A vital object containing population, estimated deaths (not actual
deaths) and net migration, using the formula Net Migration =
Population - lag(Population cohort) - Deaths + Births. Births are
returned as Population at Age -1, and deaths are estimated from the life
table

## References

Hyndman and Booth (2008) Stochastic population forecasts using
functional data models for mortality, fertility and migration.
*International Journal of Forecasting*, 24(3), 323-342.

## Examples

``` r
net_migration(norway_mortality, norway_births)
#> # A vital: 40,959 x 6 [1Y]
#> # Key:     Age x Sex [111 x 3]
#>     Year   Age Sex    Population Deaths NetMigration
#>    <dbl> <dbl> <chr>       <dbl>  <dbl>        <dbl>
#>  1  1900    -1 Female      32150 1745.        248.  
#>  2  1900     0 Female      30070 1035.        -86.2 
#>  3  1900     1 Female      28960  594.        222.  
#>  4  1900     2 Female      28043  281.         57.3 
#>  5  1900     3 Female      27019  190.         26.8 
#>  6  1900     4 Female      26854  155.          3.50
#>  7  1900     5 Female      25569  122.          5.37
#>  8  1900     6 Female      25534  102.          4.64
#>  9  1900     7 Female      24314   91.7        -5.27
#> 10  1900     8 Female      24979   92.9       -11.1 
#> # ℹ 40,949 more rows
if (FALSE) { # \dontrun{
# Files downloaded from the [Human Mortality Database](https://mortality.org)
deaths <- read_hmd_files(c("Population.txt", "Mx_1x1.txt"))
births <- read_hmd_file("Births.txt")
mig <- net_migration(deaths, births)
} # }
```
