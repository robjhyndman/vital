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
deaths) and net migration. Net migration at age x in year t is for the
cohort aged x at the end of year t (on 1 January of year t+1), so it
equals the population aged x on 1 January of year t+1, minus the
cohort's population on 1 January of year t (births during year t for age
0, and the two oldest ages combined for the open age group), plus the
cohort's deaths during year t. Deaths are estimated from the
survivorship ratios of the life table, as in
[`demography::netmigration()`](https://pkg.robjhyndman.com/demography/reference/migration.html).

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
#>    <int> <int> <chr>       <dbl>  <dbl>        <dbl>
#>  1  1900     0 Female      30070 1726.        229.  
#>  2  1900     1 Female      28960 1054.        -66.6 
#>  3  1900     2 Female      28043  594.        222.  
#>  4  1900     3 Female      27019  281.         57.3 
#>  5  1900     4 Female      26854  190.         26.8 
#>  6  1900     5 Female      25569  155.          3.50
#>  7  1900     6 Female      25534  122.          5.37
#>  8  1900     7 Female      24314  102.          4.64
#>  9  1900     8 Female      24979   91.7        -5.27
#> 10  1900     9 Female      24428   92.9       -11.1 
#> # ℹ 40,949 more rows
if (FALSE) { # \dontrun{
# Files downloaded from the [Human Mortality Database](https://mortality.org)
deaths <- read_hmd_files(c("Population.txt", "Mx_1x1.txt"))
births <- read_hmd_files("Births.txt")
mig <- net_migration(deaths, births)
} # }
```
