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
