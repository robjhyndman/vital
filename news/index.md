# Changelog

## vital (development version)

- Updated to work with fabletools v0.7.0+
- Fixed
  [`generate()`](https://generics.r-lib.org/reference/generate.html) for
  LC models, which back-transformed simulations twice
- Fixed [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md)
  treating zero rates as log rates of 0 rather than as missing
- Fixed [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md)
  giving wrong estimates when the rows of the data are not sorted by age
  and then year (e.g. when calling
  [`estimate()`](https://fabletools.tidyverts.org/reference/estimate.html)
  directly on `norway_mortality`)
- [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md) deviances
  are now missing rather than infinite with only two years of data
- Fixed
  [`generate()`](https://generics.r-lib.org/reference/generate.html) for
  FMEAN models using the wrong standard deviation for each age
- Fixed [`tidy()`](https://generics.r-lib.org/reference/tidy.html) for
  FMEAN models, which understated standard errors
- Fixed
  [`life_expectancy()`](https://pkg.robjhyndman.com/vital/reference/life_expectancy.md)
  ignoring the `mortality` argument
- [`life_expectancy()`](https://pkg.robjhyndman.com/vital/reference/life_expectancy.md)
  now warns when `from_age` contains ages that are not in the data
- Fixed
  [`life_expectancy()`](https://pkg.robjhyndman.com/vital/reference/life_expectancy.md)
  failing when the age variable is not called `Age`
- [`forecast()`](https://generics.r-lib.org/reference/forecast.html) and
  [`generate()`](https://generics.r-lib.org/reference/generate.html) now
  work when `new_data` does not start immediately after the training
  data, rather than giving missing values (LC, FDM), errors (GAPC) or
  the wrong forecast horizon (FNAIVE)
- Fixed
  [`total_fertility_rate()`](https://pkg.robjhyndman.com/vital/reference/total_fertility_rate.md)
  ignoring the width of age groups, which made it too small for 5-year
  age groups
- Fixed
  [`collapse_ages()`](https://pkg.robjhyndman.com/vital/reference/collapse_ages.md)
  giving wrong results when some years or groups are missing an age
- Fixed
  [`life_table()`](https://pkg.robjhyndman.com/vital/reference/life_table.md)
  failing for abridged life tables with only three or four age groups
- [`life_table()`](https://pkg.robjhyndman.com/vital/reference/life_table.md)
  now caps probabilities of death `qx` at 1, which previously exceeded 1
  at very high mortality rates, giving negative deaths `dx` and infinite
  life expectancies
- [`life_table()`](https://pkg.robjhyndman.com/vital/reference/life_table.md)
  now uses sex-specific infant separation factors when sex is
  capitalised (e.g. “Female”)
- [`smooth_mortality_law()`](https://pkg.robjhyndman.com/vital/reference/smooth_mortality_law.md)
  now fits to deaths and population when available, as intended
- [`smooth_loess()`](https://pkg.robjhyndman.com/vital/reference/smooth_vital.md)
  now weights rates by population divided by rate (their inverse
  variance), as
  [`smooth_spline()`](https://pkg.robjhyndman.com/vital/reference/smooth_vital.md)
  does, rather than by population times rate
- Fixed
  [`smooth_mortality()`](https://pkg.robjhyndman.com/vital/reference/smooth_vital.md)
  failing with “initial parameters not feasible” for some age ranges
- Fixed
  [`smooth_mortality_law()`](https://pkg.robjhyndman.com/vital/reference/smooth_mortality_law.md)
  giving infinite standard errors when some ages have zero population,
  and computing standard errors with the wrong units; they are now based
  on the residuals of the log rates
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
- [`FNAIVE()`](https://pkg.robjhyndman.com/vital/reference/FNAIVE.md)
  now starts its random walk from the last finite value at ages whose
  final value is zero or missing (e.g. the log of a zero rate), with a
  warning, rather than forecasting `-Inf` or `NaN`
- [`FMEAN()`](https://pkg.robjhyndman.com/vital/reference/FMEAN.md) now
  interpolates the mean from neighbouring ages, with a warning, at ages
  with no finite values, rather than forecasting `NaN`
- [`interpolate()`](https://generics.r-lib.org/reference/interpolate.html)
  now keeps all columns of `new_data` (not just the response), and works
  for GAPC models (APC, CBD, etc.)
- [`forecast()`](https://generics.r-lib.org/reference/forecast.html),
  [`generate()`](https://generics.r-lib.org/reference/generate.html) and
  [`interpolate()`](https://generics.r-lib.org/reference/interpolate.html)
  now accept `new_data` with keys in a different order from the models,
  and `new_data` as a tsibble without vital variables (e.g. from
  [`tsibble::new_data()`](https://tsibble.tidyverts.org/reference/new-data.html))
- Fixed the printed key summary of vital objects with several keys and
  no age variable, and with different ages for different series
- [`as_vital()`](https://pkg.robjhyndman.com/vital/reference/as_vital.md)
  on a vital object now keeps any vital variables not given in the call,
  also when `index` and `key` are not given
- [`rename()`](https://dplyr.tidyverse.org/reference/rename.html) and
  [`select()`](https://dplyr.tidyverse.org/reference/select.html) now
  keep vital variables (age, sex, etc.) that are renamed
- Fixed
  [`augment()`](https://generics.r-lib.org/reference/augment.html),
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) and
  [`residuals()`](https://rdrr.io/r/stats/residuals.html) failing when
  the response is a vital variable such as `Population`
- [`augment()`](https://generics.r-lib.org/reference/augment.html),
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) and
  [`residuals()`](https://rdrr.io/r/stats/residuals.html) now work on
  mables containing NULL models (from models that failed to fit), giving
  missing fitted values for those models
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  on a vital object with several variables
  (e.g. `vars(Deaths, Population)`) now plots each variable in its own
  row of panels, rather than joining them into one line
- [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  on a vital fable now returns a vital object rather than a tsibble
- [`rename()`](https://dplyr.tidyverse.org/reference/rename.html) now
  keeps mables and fables as vital objects, so that
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html) and
  other methods still use the vital methods
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
- Fixed
  [`generate_population()`](https://pkg.robjhyndman.com/vital/reference/generate_population.md)
  applying survivorship ratios and cohort deaths one age too young,
  which misstated deaths at every age and understated the open age group
  by about a third. It now follows
  [`demography::pop.sim()`](https://pkg.robjhyndman.com/demography/reference/pop.sim.html),
  including infant deaths in the cohort aged 0 at the start of the year,
  with net migrants indexed by age at the end of the year
- [`forecast()`](https://generics.r-lib.org/reference/forecast.html) and
  [`generate()`](https://generics.r-lib.org/reference/generate.html) for
  GAPC models now work when the index and age variables are not called
  `Year` and `Age`
- GAPC models (APC, CBD, etc.) with `use_weights = TRUE` no longer warn
  about cells with deaths but no exposure, which are given zero weight
- GAPC models with `link = "logit"` no longer fail when the data contain
  missing values
- [`net_migration()`](https://pkg.robjhyndman.com/vital/reference/net_migration.md)
  now works when births are stored as a population variable
- [`net_migration()`](https://pkg.robjhyndman.com/vital/reference/net_migration.md)
  now indexes net migrants by age at the end of the year, as in
  [`demography::netmigration()`](https://pkg.robjhyndman.com/demography/reference/migration.html):
  births are age 0 (rather than -1), and the open age group combines the
  two oldest ages at the start of the year. This fixes wrong net
  migration at the two oldest ages
- Fixed
  [`read_ktdb()`](https://pkg.robjhyndman.com/vital/reference/read_ktdb.md)
  ignoring the `triangle` argument
- [`read_hmd_files()`](https://pkg.robjhyndman.com/vital/reference/read_hmd_files.md)
  now orders rows by year when combining age-specific and
  non-age-specific files
- Fixed
  [`read_ktdb()`](https://pkg.robjhyndman.com/vital/reference/read_ktdb.md)
  failing for most countries, and rejecting Lithuania (code 37);
  countries without K-T data now give an informative error
- [`read_stmf()`](https://pkg.robjhyndman.com/vital/reference/read_stmf.md)
  now takes `username` and `password` arguments, as the HMD requires a
  login to download STMF data. Previously it failed when reading the
  login page returned by the HMD
- [`read_stmf()`](https://pkg.robjhyndman.com/vital/reference/read_stmf.md)
  now gives an informative error for countries without STMF data
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
- [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md) no longer
  adjusts to deaths by default when fitted to product-ratios from
  [`make_pr()`](https://pkg.robjhyndman.com/vital/reference/make_pr.md)
- [`life_expectancy()`](https://pkg.robjhyndman.com/vital/reference/life_expectancy.md)
  now returns only the index, keys and `ex`, without the `rx`, `nx` and
  `ax` columns of the life table
- [`generate_population()`](https://pkg.robjhyndman.com/vital/reference/generate_population.md)
  now gives an error unless the starting population has consecutive
  single-year ages
- [`collapse_ages()`](https://pkg.robjhyndman.com/vital/reference/collapse_ages.md)
  now sums all numeric variables other than age and rates, rather than
  truncating any variable that changes linearly with age or is constant
  over age
- [`interpolate()`](https://generics.r-lib.org/reference/interpolate.html)
  now works for
  [`FNAIVE()`](https://pkg.robjhyndman.com/vital/reference/FNAIVE.md),
  [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md) and
  [`FDM()`](https://pkg.robjhyndman.com/vital/reference/FDM.md) models,
  as well as
  [`FMEAN()`](https://pkg.robjhyndman.com/vital/reference/FMEAN.md)
- Errors reported by
  [`model()`](https://fabletools.tidyverts.org/reference/model.html) now
  include their underlying cause
- Fixed coherent
  [`FDM()`](https://pkg.robjhyndman.com/vital/reference/FDM.md) models
  failing when estimated in parallel with `future`
- [`FDM()`](https://pkg.robjhyndman.com/vital/reference/FDM.md) now
  requires `order` to be a positive integer, rather than failing at
  forecast time when `order = 0`
- [`FDM()`](https://pkg.robjhyndman.com/vital/reference/FDM.md) now uses
  the actual ages, rather than assuming they are equally spaced, so it
  handles abridged ages (0, 1, 5, 10, …) correctly. Results for
  single-year ages are unchanged
- Fixed [`FDM()`](https://pkg.robjhyndman.com/vital/reference/FDM.md)
  misaligning years with missing values at the oldest (or youngest)
  ages, such as zero rates on the log scale. Such years are now
  extrapolated linearly from their last observed ages
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  now plots the age, period and cohort components of GAPC models
  ([`GAPC()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md),
  [`LC2()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md),
  [`CBD()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md),
  [`APC()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md),
  [`RH()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md),
  [`M7()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md) and
  [`PLAT()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md))
- Fixed model formulas using
  [`vars()`](https://dplyr.tidyverse.org/reference/vars.html), and the
  error message for transformations that cannot be inverted, which
  failed with “could not find function”
- Joins such as
  [`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  on vital objects and vital fables no longer drop the vital variables
  (age, sex, etc.), and
  [`read_hmd()`](https://pkg.robjhyndman.com/vital/reference/read_hmd.md)
  keeps them when combining age-specific and other data
- Fixed
  [`life_table()`](https://pkg.robjhyndman.com/vital/reference/life_table.md),
  [`life_expectancy()`](https://pkg.robjhyndman.com/vital/reference/life_expectancy.md)
  and
  [`interpolate()`](https://generics.r-lib.org/reference/interpolate.html)
  for data with age group keys as well as age (e.g. `AgeGroup` in vitals
  from `demogdata` objects), and
  [`total_fertility_rate()`](https://pkg.robjhyndman.com/vital/reference/total_fertility_rate.md)
  for data with keys other than age. Forecasts now keep age group keys
- Fixed
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  with `new_data`, which failed for all models
- [`FMEAN()`](https://pkg.robjhyndman.com/vital/reference/FMEAN.md) and
  [`FNAIVE()`](https://pkg.robjhyndman.com/vital/reference/FNAIVE.md)
  now treat infinite values, such as logs of zero rates, as missing,
  rather than giving infinite means and standard deviations
- Fixed
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html) for
  GAPC models
  ([`LC2()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md),
  [`CBD()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md), etc.)
  failing when `h = 1`
- [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md) deviances
  reported by
  [`glance()`](https://generics.r-lib.org/reference/glance.html) are no
  longer `NaN` when some ages have zero population
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  for [`FDM()`](https://pkg.robjhyndman.com/vital/reference/FDM.md)
  models no longer fails when `show_order` is larger than the number of
  fitted components (e.g. `order = 1`)
- [`collapse_ages()`](https://pkg.robjhyndman.com/vital/reference/collapse_ages.md)
  now gives an informative error when `max_age` is not one of the ages,
  and no longer adds a second `+` to age group labels that already end
  in `+`
- Subsetting a mable of vital models with `[` no longer labels the
  result as a mable when it contains no models
- `LC(jump_choice = "actual")` now uses fitted rates, with a warning, as
  the jump-off for ages whose rate is zero or missing in the final year,
  rather than giving missing forecasts
- Clearer error messages from
  [`model()`](https://fabletools.tidyverts.org/reference/model.html)
  when there is no age variable,
  [`FDM()`](https://pkg.robjhyndman.com/vital/reference/FDM.md) when
  `order` is too large for the number of years, and
  [`total_fertility_rate()`](https://pkg.robjhyndman.com/vital/reference/total_fertility_rate.md)
  when no fertility variable is found
- Plots of
  [`FMEAN()`](https://pkg.robjhyndman.com/vital/reference/FMEAN.md) and
  [`FNAIVE()`](https://pkg.robjhyndman.com/vital/reference/FNAIVE.md)
  models now title the legend with the key name rather than an
  [`interaction()`](https://rdrr.io/r/base/interaction.html) call
- [`forecast()`](https://generics.r-lib.org/reference/forecast.html) and
  [`generate()`](https://generics.r-lib.org/reference/generate.html) now
  keep the type of the time index (e.g. integer years)
- The smoothing functions now keep integer ages when the smoothed ages
  are whole numbers
- [`FMEAN()`](https://pkg.robjhyndman.com/vital/reference/FMEAN.md) and
  [`FNAIVE()`](https://pkg.robjhyndman.com/vital/reference/FNAIVE.md)
  now interpolate standard deviations from neighbouring ages where they
  cannot be estimated, rather than simulating missing values. Bootstrap
  simulations from
  [`FMEAN()`](https://pkg.robjhyndman.com/vital/reference/FMEAN.md) no
  longer fail at ages with no finite residuals
- Fixed
  [`augment()`](https://generics.r-lib.org/reference/augment.html),
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) and
  [`residuals()`](https://rdrr.io/r/stats/residuals.html) failing for
  GAPC models
  ([`LC2()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md),
  [`APC()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md),
  [`CBD()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md), etc.)
- [`generate()`](https://generics.r-lib.org/reference/generate.html) and
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html) for
  GAPC models now give an error with `bootstrap = TRUE`, rather than
  silently ignoring it
- Fixed
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html) and
  [`generate()`](https://generics.r-lib.org/reference/generate.html) for
  [`FNAIVE()`](https://pkg.robjhyndman.com/vital/reference/FNAIVE.md)
  models failing for data with age group keys (e.g. `AgeGroup` in vitals
  from `demogdata` objects)
- [`forecast()`](https://generics.r-lib.org/reference/forecast.html),
  [`generate()`](https://generics.r-lib.org/reference/generate.html) and
  [`interpolate()`](https://generics.r-lib.org/reference/interpolate.html)
  now give a clear error when `new_data` is a list, rather than failing
  in an unsupported attempt to combine scenarios
- [`generate_population()`](https://pkg.robjhyndman.com/vital/reference/generate_population.md)
  is faster, computing survivorship ratios for all replicates at once
  rather than a life table for each
- [`make_pr()`](https://pkg.robjhyndman.com/vital/reference/make_pr.md)
  now sets zero values to 10^-5 before computing ratios, as documented,
  so ratios are no longer zero (and infinite on the log scale)
- [`life_table()`](https://pkg.robjhyndman.com/vital/reference/life_table.md)
  and
  [`life_expectancy()`](https://pkg.robjhyndman.com/vital/reference/life_expectancy.md)
  now interpolate missing mortality rates from neighbouring ages
  (log-linearly), with a warning, rather than silently setting them to
  0.5. This also affects `LC(adjust = "e0")` fits to data with zero or
  missing rates
- `LC(adjust = "dxt")` now excludes only cells with zero population when
  fitting to deaths, rather than all cells with fewer than one expected
  death
- [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md) now uses
  the nearest available age for `ax` at the youngest or oldest ages when
  they have no observed rates, rather than returning missing values
- [`generate_population()`](https://pkg.robjhyndman.com/vital/reference/generate_population.md)
  now simulates each model from the end of its data to the last year
  required, so models trained on data ending before the starting
  population no longer give missing years (e.g. zero births), and gives
  an error if a model is trained on data beyond the starting population
- [`tidy()`](https://generics.r-lib.org/reference/tidy.html) now returns
  the coefficients of
  [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md),
  [`FDM()`](https://pkg.robjhyndman.com/vital/reference/FDM.md),
  [`FNAIVE()`](https://pkg.robjhyndman.com/vital/reference/FNAIVE.md)
  and GAPC models in long form (`term` and `estimate`, with the age,
  time or birth year to which each refers), rather than nothing

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
