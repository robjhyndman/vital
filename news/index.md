# Changelog

## vital (development version)

- Updated to work with fabletools v0.7.0+
- Fixed
  [`generate()`](https://generics.r-lib.org/reference/generate.html) for
  LC models, which back-transformed simulations twice
- Fixed [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md)
  treating zero rates as log rates of 0 rather than as missing
- Fixed
  [`generate()`](https://generics.r-lib.org/reference/generate.html) for
  FMEAN models using the wrong standard deviation for each age
- Fixed [`tidy()`](https://generics.r-lib.org/reference/tidy.html) for
  FMEAN models, which understated standard errors
- Fixed
  [`life_expectancy()`](https://pkg.robjhyndman.com/vital/reference/life_expectancy.md)
  ignoring the `mortality` argument
- Fixed
  [`life_expectancy()`](https://pkg.robjhyndman.com/vital/reference/life_expectancy.md)
  failing when the age variable is not called `Age`
- [`life_table()`](https://pkg.robjhyndman.com/vital/reference/life_table.md)
  now uses sex-specific infant separation factors when sex is
  capitalised (e.g. “Female”)
- [`smooth_mortality_law()`](https://pkg.robjhyndman.com/vital/reference/smooth_mortality_law.md)
  now fits to deaths and population when available, as intended
- Fixed
  [`generate_population()`](https://pkg.robjhyndman.com/vital/reference/generate_population.md)
  not adding back the mean for coherent migration models
- `FDM(coherent = TRUE)` now validates `coherent_ts_model_fn` rather
  than `ts_model_fn`
- Fixed
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  with no variables failing on vital objects
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  on a mable of GAPC models (APC, CBD, etc.) now gives an error instead
  of infinite recursion
- [`rename()`](https://dplyr.tidyverse.org/reference/rename.html) and
  [`select()`](https://dplyr.tidyverse.org/reference/select.html) now
  keep vital variables (age, sex, etc.) that are renamed
- Fixed
  [`generate_population()`](https://pkg.robjhyndman.com/vital/reference/generate_population.md)
  failing when `female` is supplied
- Fixed
  [`generate_population()`](https://pkg.robjhyndman.com/vital/reference/generate_population.md)
  failing when `mortality_model` or `migration_model` is NULL
- Fixed
  [`generate_population()`](https://pkg.robjhyndman.com/vital/reference/generate_population.md)
  failing when `fertility_model` is NULL
- [`generate_population()`](https://pkg.robjhyndman.com/vital/reference/generate_population.md)
  now works with any names for the index, age, sex and population
  variables
- [`generate_population()`](https://pkg.robjhyndman.com/vital/reference/generate_population.md)
  now correctly checks that each mable contains only one model
- [`forecast()`](https://generics.r-lib.org/reference/forecast.html) and
  [`generate()`](https://generics.r-lib.org/reference/generate.html) for
  GAPC models now work when the index and age variables are not called
  `Year` and `Age`
- GAPC models with `link = "logit"` no longer fail when the data contain
  missing values
- [`net_migration()`](https://pkg.robjhyndman.com/vital/reference/net_migration.md)
  now works when births are stored as a population variable
- Fixed
  [`read_ktdb()`](https://pkg.robjhyndman.com/vital/reference/read_ktdb.md)
  ignoring the `triangle` argument
- Grouped vital objects now stay `grouped_vital` after
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html),
  [`rename()`](https://dplyr.tidyverse.org/reference/rename.html),
  [`relocate()`](https://dplyr.tidyverse.org/reference/relocate.html),
  [`slice()`](https://dplyr.tidyverse.org/reference/slice.html) and `[`
- Fixed
  [`generate_population()`](https://pkg.robjhyndman.com/vital/reference/generate_population.md)
  producing missing populations at the oldest ages from negative
  simulated mortality rates and undefined survivorship ratios
- [`generate_population()`](https://pkg.robjhyndman.com/vital/reference/generate_population.md)
  now returns the index and age variables with the same types as
  `starting_population`
- [`model()`](https://fabletools.tidyverts.org/reference/model.html) now
  estimates models in parallel when the `future` package is attached, as
  in fabletools (the previous implementation never ran)
- Fixed coherent
  [`FDM()`](https://pkg.robjhyndman.com/vital/reference/FDM.md) models
  fitting a second `geometric_mean` or `mean` series coherently when
  there are several such series (e.g. with keys other than sex)
- Removed uses of deprecated and superseded tidyverse functions, which
  gave tidyselect deprecation warnings
- Fixed
  [`collapse_ages()`](https://pkg.robjhyndman.com/vital/reference/collapse_ages.md)
  summing the age variable when age groups are unequal (e.g. 0, 1, 5,
  10, …)
- [`tsibble::fill_gaps()`](https://tsibble.tidyverts.org/reference/fill_gaps.html)
  now keeps vital objects and their attributes
- [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md) and the
  GAPC models
  ([`APC()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md),
  [`CBD()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md), etc.)
  now give an error, rather than misaligned fits, when some age and time
  combinations are missing
- [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html) on a
  mable of vital models no longer duplicates the `mdl_vtl_df` class
- Fixed
  [`life_table()`](https://pkg.robjhyndman.com/vital/reference/life_table.md)
  treating abridged ages (0, 1, 5, 10, …) as single years, and 5-year
  ages as abridged
- [`total_fertility_rate()`](https://pkg.robjhyndman.com/vital/reference/total_fertility_rate.md)
  now accepts a bare variable name for `fertility`
- Fixed bootstrapped simulations from
  [`FNAIVE()`](https://pkg.robjhyndman.com/vital/reference/FNAIVE.md)
  models containing missing values
- [`FNAIVE()`](https://pkg.robjhyndman.com/vital/reference/FNAIVE.md)
  models now work with data observed at intervals other than one time
  unit
- [`generate()`](https://generics.r-lib.org/reference/generate.html) for
  [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md) models,
  and hence `forecast(simulate = TRUE)` and
  [`generate_population()`](https://pkg.robjhyndman.com/vital/reference/generate_population.md),
  now uses the actual rates as the jump-off when
  `jump_choice = "actual"`
- Fixed
  [`generate_population()`](https://pkg.robjhyndman.com/vital/reference/generate_population.md)
  returning missing populations at age 0 when mortality rates are zero
  (including when `mortality_model` is NULL)
- [`as_vital()`](https://pkg.robjhyndman.com/vital/reference/as_vital.md)
  now works for `demogdata` objects containing only rates or only
  population
- [`generate()`](https://generics.r-lib.org/reference/generate.html) for
  [`FNAIVE()`](https://pkg.robjhyndman.com/vital/reference/FNAIVE.md)
  models is much faster
- [`as_vital()`](https://pkg.robjhyndman.com/vital/reference/as_vital.md)
  on a vital object now accepts `key` (it previously ignored it), and
  keeps the existing vital variables unless they are given
- [`forecast()`](https://generics.r-lib.org/reference/forecast.html),
  [`generate()`](https://generics.r-lib.org/reference/generate.html),
  [`augment()`](https://generics.r-lib.org/reference/augment.html) and
  [`interpolate()`](https://generics.r-lib.org/reference/interpolate.html)
  now keep the sex and other vital variables of the data, not just age
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  of [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md) and
  [`FDM()`](https://pkg.robjhyndman.com/vital/reference/FDM.md) models
  now works when there is more than one key other than age
- [`collapse_ages()`](https://pkg.robjhyndman.com/vital/reference/collapse_ages.md)
  no longer drops the open interval flag when `max_age` is the oldest
  age in the data
- [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md) now
  defaults to `adjust = "none"` when the data have no deaths or
  population (e.g. fertility), gives an error if `adjust = "dt"` or
  `"dxt"` is requested for such data, and reports missing deviances
  rather than zero
- [`life_expectancy()`](https://pkg.robjhyndman.com/vital/reference/life_expectancy.md)
  now returns only the index, keys and `ex`, without the `rx`, `nx` and
  `ax` columns of the life table
- [`generate_population()`](https://pkg.robjhyndman.com/vital/reference/generate_population.md)
  now gives an error unless the starting population has consecutive
  single-year ages
- [`collapse_ages()`](https://pkg.robjhyndman.com/vital/reference/collapse_ages.md)
  now sums all numeric variables other than age keys and constants,
  rather than truncating any variable that changes linearly with age
- [`interpolate()`](https://generics.r-lib.org/reference/interpolate.html)
  now works for
  [`FNAIVE()`](https://pkg.robjhyndman.com/vital/reference/FNAIVE.md),
  [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md) and
  [`FDM()`](https://pkg.robjhyndman.com/vital/reference/FDM.md) models,
  as well as
  [`FMEAN()`](https://pkg.robjhyndman.com/vital/reference/FMEAN.md)

## vital 2.0.3

CRAN release: 2026-02-13

- Added default ARFIMA modelling for coherent FDM
- Fixed to work with fabletools v0.6.0+

## vital 2.0.2

CRAN release: 2026-01-18

- Fixed to work with dplyr v1.2.0

## vital 2.0.1

CRAN release: 2025-10-05

- Updated HMDHFDplus dependency to v2.08+

## vital 2.0.0

CRAN release: 2025-08-20

- New data functions: read_ktdb(), read_ktdb_files(), read_stmf(),
  read_stmf_files()
- New model functions: GAPC(), PLAT(), M7(), RH(), CBD(), LC2()
- New smoothing function: smooth_mortality_law()
- New stochastic simulation function: generate_population()
- Added vignette introducing the package
- Added vignette on stochastic population forecasting
- Removed Australian data sets to reduce package size
- Updated Norwegian data sets
- Bug fixes and unit tests

## vital 1.1.0

CRAN release: 2024-06-21

- Improved formatting of vital objects when printed
- Added vital_vars function
- Added age_components methods for FNAIVE and FMEAN models
- Data before 1900 removed from norway_xxx data sets

## vital 1.0.0

CRAN release: 2024-06-04

- First CRAN submission
