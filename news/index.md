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
