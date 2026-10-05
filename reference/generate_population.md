# Future population simulation

Simulate future age-specific population given a starting population and
models for fertility, mortality, and migration. If any model is NULL, it
is assumed there are no future births, deaths or net migrants,
respectively. This is an experimental function and has not been
thoroughly tested. The simulation follows
[`demography::pop.sim()`](https://pkg.robjhyndman.com/demography/reference/pop.sim.html),
as described in Hyndman and Booth (2008).

## Usage

``` r
generate_population(
  starting_population,
  mortality_model = NULL,
  fertility_model = NULL,
  migration_model = NULL,
  h = 10,
  n_reps = 1000,
  female = NULL
)
```

## Arguments

- starting_population:

  A `vital` object with the age-sex-specific starting population.

- mortality_model:

  A `mable` object containing an age-sex-specific model for mortality
  rates, trained on data up to the year of the starting population. If
  NULL, there are zero future deaths.

- fertility_model:

  A `mable` object containing an age-specific model for fertility rates,
  trained on data up to the year of the starting population. If NULL,
  there are zero future births.

- migration_model:

  A `mable` object containing an age-sex-specific model for net
  migration numbers, trained on data up to the year of the starting
  population. Net migrants are indexed by age at the end of the year, as
  returned by
  [`net_migration()`](https://pkg.robjhyndman.com/vital/reference/net_migration.md).
  If NULL, there are zero future net migrants.

- h:

  The forecast horizon equal to the number of years to simulate into the
  future.

- n_reps:

  The number of replicates to simulate.

- female:

  A character string giving the name used for females in the sex
  variable of the `starting_population`. This is needed when computing
  births from the fertility rates. If missing, the function will try to
  identify the most likely value automatically.

## Value

A `vital` object containing the simulated future population.

## References

Hyndman and Booth (2008) Stochastic population forecasts using
functional data models for mortality, fertility and migration.
*International Journal of Forecasting*, 24(3), 323-342.

## Examples

``` r
# Norwegian data, with ages above 100 combined
nor <- norway_mortality |>
  dplyr::filter(Sex != "Total") |>
  collapse_ages(max_age = 100)
# Models trained on data up to the year of the starting population
mortality <- nor |>
  dplyr::filter(Year > 2010) |>
  model(fmean = FMEAN(log(Mortality)))
fertility <- norway_fertility |>
  dplyr::filter(Year > 2010) |>
  model(fmean = FMEAN(Fertility))
migration <- net_migration(nor, norway_births) |>
  dplyr::filter(Year > 2010) |>
  model(fmean = FMEAN(NetMigration))
# Simulate 5 future populations for 3 years, starting from the final year of data
generate_population(
  starting_population = nor |> dplyr::filter(Year == max(Year)),
  mortality_model = mortality,
  fertility_model = fertility,
  migration_model = migration,
  h = 3,
  n_reps = 5
)
#> # A vital: 3,030 x 5 [1Y]
#> # Key:     Age x (Sex, .rep) [101 x 10]
#>     Year   Age Sex    .rep  Population
#>    <int> <int> <chr>  <chr>      <dbl>
#>  1  2024     0 Female 1          28791
#>  2  2024     1 Female 1          25636
#>  3  2024     2 Female 1          28528
#>  4  2024     3 Female 1          26908
#>  5  2024     4 Female 1          27810
#>  6  2024     5 Female 1          27863
#>  7  2024     6 Female 1          28828
#>  8  2024     7 Female 1          29870
#>  9  2024     8 Female 1          30189
#> 10  2024     9 Female 1          30424
#> # ℹ 3,020 more rows
```
