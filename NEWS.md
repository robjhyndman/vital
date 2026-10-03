# vital (development version)

* Updated to work with fabletools v0.7.0+
* Fixed `generate()` for LC models, which back-transformed simulations twice
* Fixed `LC()` treating zero rates as log rates of 0 rather than as missing
* Fixed `generate()` for FMEAN models using the wrong standard deviation for each age
* Fixed `tidy()` for FMEAN models, which understated standard errors
* Fixed `life_expectancy()` ignoring the `mortality` argument
* Fixed `life_expectancy()` failing when the age variable is not called `Age`
* `life_table()` now uses sex-specific infant separation factors when sex is capitalised (e.g. "Female")
* `smooth_mortality_law()` now fits to deaths and population when available, as intended
* Fixed `generate_population()` not adding back the mean for coherent migration models
* `FDM(coherent = TRUE)` now validates `coherent_ts_model_fn` rather than `ts_model_fn`
* Fixed `group_by()` with no variables failing on vital objects
* `autoplot()` on a mable of GAPC models (APC, CBD, etc.) now gives an error instead of infinite recursion

# vital 2.0.3

* Added default ARFIMA modelling for coherent FDM
* Fixed to work with fabletools v0.6.0+

# vital 2.0.2

* Fixed to work with dplyr v1.2.0

# vital 2.0.1

* Updated HMDHFDplus dependency to v2.08+

# vital 2.0.0

* New data functions: read_ktdb(), read_ktdb_files(), read_stmf(), read_stmf_files()
* New model functions: GAPC(), PLAT(), M7(), RH(), CBD(), LC2()
* New smoothing function: smooth_mortality_law()
* New stochastic simulation function: generate_population()
* Added vignette introducing the package
* Added vignette on stochastic population forecasting
* Removed Australian data sets to reduce package size
* Updated Norwegian data sets
* Bug fixes and unit tests

# vital 1.1.0

* Improved formatting of vital objects when printed
* Added vital_vars function
* Added age_components methods for FNAIVE and FMEAN models
* Data before 1900 removed from norway_xxx data sets

# vital 1.0.0

* First CRAN submission
