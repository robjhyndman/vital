# Changelog

## vital (development version)

- Updated to work with fabletools v0.7.0+
- [`read_stmf()`](https://pkg.robjhyndman.com/vital/reference/read_stmf.md)
  now takes `username` and `password` arguments, as the HMD now requires
  a login to download STMF data.
- [`model()`](https://fabletools.tidyverts.org/reference/model.html) now
  estimates models in parallel when the `future` package is attached, as
  in fabletools (the previous implementation never ran)
- [`interpolate()`](https://generics.r-lib.org/reference/interpolate.html)
  now works for
  [`FNAIVE()`](https://pkg.robjhyndman.com/vital/reference/FNAIVE.md),
  [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md),
  [`FDM()`](https://pkg.robjhyndman.com/vital/reference/FDM.md) and GAPC
  models
  ([`APC()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md),
  [`CBD()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md), etc.),
  as well as
  [`FMEAN()`](https://pkg.robjhyndman.com/vital/reference/FMEAN.md)
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  now plots the age, period and cohort components of GAPC models
  ([`GAPC()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md),
  [`LC2()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md),
  [`CBD()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md),
  [`APC()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md),
  [`RH()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md),
  [`M7()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md) and
  [`PLAT()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md))
- [`tidy()`](https://generics.r-lib.org/reference/tidy.html) now returns
  the coefficients of
  [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md),
  [`FDM()`](https://pkg.robjhyndman.com/vital/reference/FDM.md),
  [`FNAIVE()`](https://pkg.robjhyndman.com/vital/reference/FNAIVE.md)
  and GAPC models in long form (`term` and `estimate`, with the age,
  time or birth year to which each refers)
- [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md),
  [`FDM()`](https://pkg.robjhyndman.com/vital/reference/FDM.md),
  [`FMEAN()`](https://pkg.robjhyndman.com/vital/reference/FMEAN.md) and
  [`FNAIVE()`](https://pkg.robjhyndman.com/vital/reference/FNAIVE.md)
  now give an error for unused arguments, so misspelled arguments are no
  longer silently ignored
- Many bug fixes, documentation improvements, more informative errors,
  and speed ups.

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
