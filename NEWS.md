# vital (development version)

- Updated to work with fabletools v0.7.0+
- `read_stmf()` now takes `username` and `password` arguments, as the HMD now requires a login to download STMF data.
- `model()` now estimates models in parallel when the `future` package is attached, as in fabletools (the previous implementation never ran)
- `interpolate()` now works for `FNAIVE()`, `LC()`, `FDM()` and GAPC models (`APC()`, `CBD()`, etc.), as well as `FMEAN()`
- `autoplot()` now plots the age, period and cohort components of GAPC models (`GAPC()`, `LC2()`, `CBD()`, `APC()`, `RH()`, `M7()` and `PLAT()`)
- `tidy()` now returns the coefficients of `LC()`, `FDM()`, `FNAIVE()` and GAPC models in long form (`term` and `estimate`, with the age, time or birth year to which each refers)
- Many bug fixes, documentation improvements, more informative errors, and speed ups.

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
