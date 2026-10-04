# vital (development version)

- Updated to work with fabletools v0.7.0+
- Fixed `generate()` for LC models, which back-transformed simulations twice
- Fixed `LC()` treating zero rates as log rates of 0 rather than as missing
- Fixed `generate()` for FMEAN models using the wrong standard deviation for each age
- Fixed `tidy()` for FMEAN models, which understated standard errors
- Fixed `life_expectancy()` ignoring the `mortality` argument
- Fixed `life_expectancy()` failing when the age variable is not called `Age`
- Fixed `total_fertility_rate()` ignoring the width of age groups, which made it too small for 5-year age groups
- Fixed `collapse_ages()` giving wrong results when some years or groups are missing an age
- `life_table()` now caps probabilities of death `qx` at 1, which previously exceeded 1 at very high mortality rates, giving negative deaths `dx` and infinite life expectancies
- `life_table()` now uses sex-specific infant separation factors when sex is capitalised (e.g. "Female")
- `smooth_mortality_law()` now fits to deaths and population when available, as intended
- Fixed `generate_population()` not adding back the mean for coherent migration models
- `FDM(coherent = TRUE)` now validates `coherent_ts_model_fn` rather than `ts_model_fn`
- Fixed `group_by()` with no variables failing on vital objects
- `autoplot()` on a mable of GAPC models (APC, CBD, etc.) now gives an error instead of infinite recursion
- `rename()` and `select()` now keep vital variables (age, sex, etc.) that are renamed
- Fixed `generate_population()` failing when `female` is supplied
- Fixed `generate_population()` failing when `mortality_model` or `migration_model` is NULL
- Fixed `generate_population()` failing when `fertility_model` is NULL
- `generate_population()` now works with any names for the index, age, sex and population variables
- `generate_population()` now correctly checks that each mable contains only one model
- Fixed `generate_population()` applying survivorship ratios and cohort deaths one age too young, which misstated deaths at every age and understated the open age group by about a third. It now follows `demography::pop.sim()`, including infant deaths in the cohort aged 0 at the start of the year, with net migrants indexed by age at the end of the year
- `forecast()` and `generate()` for GAPC models now work when the index and age variables are not called `Year` and `Age`
- GAPC models with `link = "logit"` no longer fail when the data contain missing values
- `net_migration()` now works when births are stored as a population variable
- `net_migration()` now indexes net migrants by age at the end of the year, as in `demography::netmigration()`: births are age 0 (rather than -1), and the open age group combines the two oldest ages at the start of the year. This fixes wrong net migration at the two oldest ages
- Fixed `read_ktdb()` ignoring the `triangle` argument
- Fixed `read_ktdb()` failing for most countries, and rejecting Lithuania (code 37); countries without K-T data now give an informative error
- `read_stmf()` now gives an informative error for countries without STMF data
- Grouped vital objects now stay `grouped_vital` after `mutate()`, `filter()`, `arrange()`, `rename()`, `relocate()`, `slice()` and `[`
- Fixed `generate_population()` producing missing populations at the oldest ages from negative simulated mortality rates and undefined survivorship ratios
- `generate_population()` now returns the index and age variables with the same types as `starting_population`
- `model()` now estimates models in parallel when the `future` package is attached, as in fabletools (the previous implementation never ran)
- Fixed coherent `FDM()` models fitting a second `geometric_mean` or `mean` series coherently when there are several such series (e.g. with keys other than sex)
- Removed uses of deprecated and superseded tidyverse functions, which gave tidyselect deprecation warnings
- Fixed `collapse_ages()` summing the age variable when age groups are unequal (e.g. 0, 1, 5, 10, ...)
- `tsibble::fill_gaps()` now keeps vital objects and their attributes
- `LC()` and the GAPC models (`APC()`, `CBD()`, etc.) now give an error, rather than misaligned fits, when some age and time combinations are missing
- `arrange()` on a mable of vital models no longer duplicates the `mdl_vtl_df` class
- Fixed `life_table()` treating abridged ages (0, 1, 5, 10, ...) as single years, and 5-year ages as abridged
- `total_fertility_rate()` now accepts a bare variable name for `fertility`
- Fixed bootstrapped simulations from `FNAIVE()` models containing missing values
- `FNAIVE()` models now work with data observed at intervals other than one time unit
- `generate()` for `LC()` models, and hence `forecast(simulate = TRUE)` and `generate_population()`, now uses the actual rates as the jump-off when `jump_choice = "actual"`
- Fixed `generate_population()` returning missing populations at age 0 when mortality rates are zero (including when `mortality_model` is NULL)
- `as_vital()` now works for `demogdata` objects containing only rates or only population
- `generate()` for `FNAIVE()` models is much faster
- `as_vital()` on a vital object now accepts `key` (it previously ignored it), and keeps the existing vital variables unless they are given
- `forecast()`, `generate()`, `augment()` and `interpolate()` now keep the sex and other vital variables of the data, not just age
- `autoplot()` of `LC()` and `FDM()` models now works when there is more than one key other than age
- `collapse_ages()` no longer drops the open interval flag when `max_age` is the oldest age in the data
- `LC()` now defaults to `adjust = "none"` when the data have no deaths or population (e.g. fertility), gives an error if `adjust = "dt"` or `"dxt"` is requested for such data, and reports missing deviances rather than zero
- `LC()` no longer adjusts to deaths by default when fitted to product-ratios from `make_pr()`
- `life_expectancy()` now returns only the index, keys and `ex`, without the `rx`, `nx` and `ax` columns of the life table
- `generate_population()` now gives an error unless the starting population has consecutive single-year ages
- `collapse_ages()` now sums all numeric variables other than age and rates, rather than truncating any variable that changes linearly with age or is constant over age
- `interpolate()` now works for `FNAIVE()`, `LC()` and `FDM()` models, as well as `FMEAN()`
- Errors reported by `model()` now include their underlying cause
- Fixed coherent `FDM()` models failing when estimated in parallel with `future`
- `FDM()` now requires `order` to be a positive integer, rather than failing at forecast time when `order = 0`
- `FDM()` now uses the actual ages, rather than assuming they are equally spaced, so it handles abridged ages (0, 1, 5, 10, ...) correctly. Results for single-year ages are unchanged
- Fixed `FDM()` misaligning years with missing values at the oldest (or youngest) ages, such as zero rates on the log scale. Such years are now extrapolated linearly from their last observed ages
- `autoplot()` now plots the age, period and cohort components of GAPC models (`GAPC()`, `LC2()`, `CBD()`, `APC()`, `RH()`, `M7()` and `PLAT()`)
- Fixed model formulas using `vars()`, and the error message for transformations that cannot be inverted, which failed with "could not find function"
- Joins such as `left_join()` on vital objects and vital fables no longer drop the vital variables (age, sex, etc.), and `read_hmd()` keeps them when combining age-specific and other data
- Fixed `life_table()`, `life_expectancy()` and `interpolate()` for data with age group keys as well as age (e.g. `AgeGroup` in vitals from `demogdata` objects), and `total_fertility_rate()` for data with keys other than age. Forecasts now keep age group keys
- Fixed `forecast()` with `new_data`, which failed for all models
- `FMEAN()` and `FNAIVE()` now treat infinite values, such as logs of zero rates, as missing, rather than giving infinite means and standard deviations
- Fixed `forecast()` for GAPC models (`LC2()`, `CBD()`, etc.) failing when `h = 1`
- `LC()` deviances reported by `glance()` are no longer `NaN` when some ages have zero population
- `autoplot()` for `FDM()` models no longer fails when `show_order` is larger than the number of fitted components (e.g. `order = 1`)
- `collapse_ages()` now gives an informative error when `max_age` is not one of the ages, and no longer adds a second `+` to age group labels that already end in `+`
- Subsetting a mable of vital models with `[` no longer labels the result as a mable when it contains no models
- `LC(jump_choice = "actual")` now uses fitted rates, with a warning, as the jump-off for ages whose rate is zero or missing in the final year, rather than giving missing forecasts
- Clearer error messages from `model()` when there is no age variable, `FDM()` when `order` is too large for the number of years, and `total_fertility_rate()` when no fertility variable is found
- Plots of `FMEAN()` and `FNAIVE()` models now title the legend with the key name rather than an `interaction()` call
- `forecast()` and `generate()` now keep the type of the time index (e.g. integer years)
- The smoothing functions now keep integer ages when the smoothed ages are whole numbers
- `FMEAN()` and `FNAIVE()` now interpolate standard deviations from neighbouring ages where they cannot be estimated, rather than simulating missing values. Bootstrap simulations from `FMEAN()` no longer fail at ages with no finite residuals
- Fixed `augment()`, `fitted()` and `residuals()` failing for GAPC models (`LC2()`, `APC()`, `CBD()`, etc.)
- `generate()` and `forecast()` for GAPC models now give an error with `bootstrap = TRUE`, rather than silently ignoring it
- Fixed `forecast()` and `generate()` for `FNAIVE()` models failing for data with age group keys (e.g. `AgeGroup` in vitals from `demogdata` objects)
- `forecast()`, `generate()` and `interpolate()` now give a clear error when `new_data` is a list, rather than failing in an unsupported attempt to combine scenarios
- `generate_population()` is faster, computing survivorship ratios for all replicates at once rather than a life table for each
- `make_pr()` now sets zero values to 10^-5 before computing ratios, as documented, so ratios are no longer zero (and infinite on the log scale)
- `life_table()` and `life_expectancy()` now interpolate missing mortality rates from neighbouring ages (log-linearly), with a warning, rather than silently setting them to 0.5. This also affects `LC(adjust = "e0")` fits to data with zero or missing rates
- `LC(adjust = "dxt")` now excludes only cells with zero population when fitting to deaths, rather than all cells with fewer than one expected death
- `LC()` now uses the nearest available age for `ax` at the youngest or oldest ages when they have no observed rates, rather than returning missing values
- `generate_population()` now simulates each model from the end of its data to the last year required, so models trained on data ending before the starting population no longer give missing years (e.g. zero births), and gives an error if a model is trained on data beyond the starting population
- `tidy()` now returns the coefficients of `LC()`, `FDM()`, `FNAIVE()` and GAPC models in long form (`term` and `estimate`, with the age, time or birth year to which each refers), rather than nothing

# vital 2.0.3

- Added default ARFIMA modelling for coherent FDM
- Fixed to work with fabletools v0.6.0+

# vital 2.0.2

- Fixed to work with dplyr v1.2.0

# vital 2.0.1

- Updated HMDHFDplus dependency to v2.08+

# vital 2.0.0

- New data functions: read_ktdb(), read_ktdb_files(), read_stmf(), read_stmf_files()
- New model functions: GAPC(), PLAT(), M7(), RH(), CBD(), LC2()
- New smoothing function: smooth_mortality_law()
- New stochastic simulation function: generate_population()
- Added vignette introducing the package
- Added vignette on stochastic population forecasting
- Removed Australian data sets to reduce package size
- Updated Norwegian data sets
- Bug fixes and unit tests

# vital 1.1.0

- Improved formatting of vital objects when printed
- Added vital_vars function
- Added age_components methods for FNAIVE and FMEAN models
- Data before 1900 removed from norway_xxx data sets

# vital 1.0.0

- First CRAN submission
