# Future population simulation

Simulate future age-specific population given a starting population and
models for fertility, mortality, and migration. If any model is NULL, it
is assumed there are no future births, deaths or net migrants,
respectively. This is an experimental function and has not been
thoroughly tested.

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
  population. If NULL, there are zero future net migrants.

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
