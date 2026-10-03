# vital (development version)

- Updated to work with fabletools v0.7.0+
- Fixed `generate()` for LC models, which back-transformed simulations twice
- Fixed `LC()` treating zero rates as log rates of 0 rather than as missing
- Fixed `generate()` for FMEAN models using the wrong standard deviation for each age
- Fixed `tidy()` for FMEAN models, which understated standard errors
- Fixed `life_expectancy()` ignoring the `mortality` argument
- Fixed `life_expectancy()` failing when the age variable is not called `Age`
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
- `forecast()` and `generate()` for GAPC models now work when the index and age variables are not called `Year` and `Age`
- GAPC models with `link = "logit"` no longer fail when the data contain missing values
- `net_migration()` now works when births are stored as a population variable
- Fixed `read_ktdb()` ignoring the `triangle` argument
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
