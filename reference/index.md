# Package index

## Create vitals

Functions for creating vital objects

- [`vital()`](https://pkg.robjhyndman.com/vital/reference/vital.md) :
  Create a vital object

- [`as_vital()`](https://pkg.robjhyndman.com/vital/reference/as_vital.md)
  : Coerce to a vital object

- [`vital_vars()`](https://pkg.robjhyndman.com/vital/reference/vital_vars.md)
  : Return vital variables

- [`read_hmd()`](https://pkg.robjhyndman.com/vital/reference/read_hmd.md)
  :

  Read data directly from HMD and construct a `vital` object for use in
  other functions

- [`read_hmd_files()`](https://pkg.robjhyndman.com/vital/reference/read_hmd_files.md)
  :

  Read data from files downloaded from HMD and construct a `vital`
  object for use in other functions

- [`read_hfd()`](https://pkg.robjhyndman.com/vital/reference/read_hfd.md)
  :

  Read data directly from HFD and construct a `vital` object for use in
  other functions

- [`read_hfd_files()`](https://pkg.robjhyndman.com/vital/reference/read_hfd_files.md)
  :

  Read data from files downloaded from HFD and construct a `vital`
  object for use in other functions

- [`read_stmf()`](https://pkg.robjhyndman.com/vital/reference/read_stmf.md)
  : Read Short-Term Mortality Fluctuations data from the Human Mortality
  Database

- [`read_stmf_files()`](https://pkg.robjhyndman.com/vital/reference/read_stmf_files.md)
  : Read STMF data from files downloaded from HMD

- [`net_migration()`](https://pkg.robjhyndman.com/vital/reference/net_migration.md)
  : Calculate net migration from a vital object

- [`read_ktdb()`](https://pkg.robjhyndman.com/vital/reference/read_ktdb.md)
  :

  Read old-age mortality from Kannisto-Thatcher (K-T) database and
  construct a `vital` object for use in other functions

- [`read_ktdb_files()`](https://pkg.robjhyndman.com/vital/reference/read_ktdb_files.md)
  : Read old-age mortality data from files downloaded from K-T database

## Data

Data sets included in the package

- [`norway_births`](https://pkg.robjhyndman.com/vital/reference/norway_births.md)
  [`norway_fertility`](https://pkg.robjhyndman.com/vital/reference/norway_births.md)
  [`norway_mortality`](https://pkg.robjhyndman.com/vital/reference/norway_births.md)
  : Norwegian mortality and births data

## Computing with vitals

Functions for computing on vital objects

- [`collapse_ages()`](https://pkg.robjhyndman.com/vital/reference/collapse_ages.md)
  : Collapse upper ages into a single age group. Counts are summed while
  rates are recomputed where possible.
- [`life_table()`](https://pkg.robjhyndman.com/vital/reference/life_table.md)
  : Compute period life tables from age-specific mortality rates
- [`life_expectancy()`](https://pkg.robjhyndman.com/vital/reference/life_expectancy.md)
  : Compute life expectancy from age-specific mortality rates
- [`total_fertility_rate()`](https://pkg.robjhyndman.com/vital/reference/total_fertility_rate.md)
  : Compute total fertility rate from age-specific fertility rates

## Smoothing

Functions for smoothing vital objects

- [`smooth_spline()`](https://pkg.robjhyndman.com/vital/reference/smooth_vital.md)
  [`smooth_mortality()`](https://pkg.robjhyndman.com/vital/reference/smooth_vital.md)
  [`smooth_fertility()`](https://pkg.robjhyndman.com/vital/reference/smooth_vital.md)
  [`smooth_loess()`](https://pkg.robjhyndman.com/vital/reference/smooth_vital.md)
  : Functions to smooth demographic data
- [`smooth_mortality_law()`](https://pkg.robjhyndman.com/vital/reference/smooth_mortality_law.md)
  : Function to smooth mortality rates using MortalityLaw package

## Modelling

Functions for fitting models to vitals

- [`model(`*`<vital>`*`)`](https://pkg.robjhyndman.com/vital/reference/model.md)
  : Estimate models for vital data
- [`GAPC()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md)
  [`LC2()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md)
  [`CBD()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md)
  [`RH()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md)
  [`APC()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md)
  [`M7()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md)
  [`PLAT()`](https://pkg.robjhyndman.com/vital/reference/GAPC.md) :
  Generalized APC stochastic mortality model
- [`FDM()`](https://pkg.robjhyndman.com/vital/reference/FDM.md) :
  Functional data model
- [`FMEAN()`](https://pkg.robjhyndman.com/vital/reference/FMEAN.md) :
  Functional mean model
- [`FNAIVE()`](https://pkg.robjhyndman.com/vital/reference/FNAIVE.md) :
  Functional naive model
- [`LC()`](https://pkg.robjhyndman.com/vital/reference/LC.md) :
  Lee-Carter model

## Coherent models

Functions for handling coherent models

- [`make_pr()`](https://pkg.robjhyndman.com/vital/reference/make_pr.md)
  : Do a product/ratio transformation
- [`make_sd()`](https://pkg.robjhyndman.com/vital/reference/make_sd.md)
  : Do a sum/difference transformation
- [`undo_pr()`](https://pkg.robjhyndman.com/vital/reference/undo_pr.md)
  : Undo a product/ratio transformation
- [`undo_sd()`](https://pkg.robjhyndman.com/vital/reference/undo_sd.md)
  : Undo a mean/difference transformation

## Methods for vital models

Functions that work with vital mables

- [`forecast(`*`<FDM>`*`)`](https://pkg.robjhyndman.com/vital/reference/forecast.md)
  [`forecast(`*`<LC>`*`)`](https://pkg.robjhyndman.com/vital/reference/forecast.md)
  [`forecast(`*`<GAPC>`*`)`](https://pkg.robjhyndman.com/vital/reference/forecast.md)
  [`forecast(`*`<FMEAN>`*`)`](https://pkg.robjhyndman.com/vital/reference/forecast.md)
  [`forecast(`*`<FNAIVE>`*`)`](https://pkg.robjhyndman.com/vital/reference/forecast.md)
  [`forecast(`*`<mdl_vtl_df>`*`)`](https://pkg.robjhyndman.com/vital/reference/forecast.md)
  : Produce forecasts from a vital model
- [`generate(`*`<mdl_vtl_df>`*`)`](https://pkg.robjhyndman.com/vital/reference/generate.md)
  : Generate responses from a mable
- [`generate_population()`](https://pkg.robjhyndman.com/vital/reference/generate_population.md)
  : Future population simulation
- [`interpolate(`*`<mdl_vtl_df>`*`)`](https://pkg.robjhyndman.com/vital/reference/interpolate.md)
  : Interpolate missing values using a vital model
- [`age_components()`](https://pkg.robjhyndman.com/vital/reference/age_components.md)
  : Extract age components from a model
- [`time_components()`](https://pkg.robjhyndman.com/vital/reference/time_components.md)
  : Extract time components from a model
- [`cohort_components()`](https://pkg.robjhyndman.com/vital/reference/cohort_components.md)
  : Extract cohort components from a model

## Graphics

Plotting functions for visualizing data and models

- [`autoplot(`*`<mdl_vtl_df>`*`)`](https://pkg.robjhyndman.com/vital/reference/autoplot.mdl_vtl_df.md)
  : Plot output from a vital model
- [`autoplot(`*`<fbl_vtl_ts>`*`)`](https://pkg.robjhyndman.com/vital/reference/autoplot.fbl_vtl_ts.md)
  : Plot forecasts from a vital model
- [`autoplot(`*`<vital>`*`)`](https://pkg.robjhyndman.com/vital/reference/autoplot.vital.md)
  : Rainbow plot of demographic data against age

## Re-exports

Exports from other packages

- [`reexports`](https://pkg.robjhyndman.com/vital/reference/reexports.md)
  [`autoplot`](https://pkg.robjhyndman.com/vital/reference/reexports.md)
  [`forecast`](https://pkg.robjhyndman.com/vital/reference/reexports.md)
  [`generate`](https://pkg.robjhyndman.com/vital/reference/reexports.md)
  [`model`](https://pkg.robjhyndman.com/vital/reference/reexports.md)
  [`report`](https://pkg.robjhyndman.com/vital/reference/reexports.md)
  [`interpolate`](https://pkg.robjhyndman.com/vital/reference/reexports.md)
  [`tidy`](https://pkg.robjhyndman.com/vital/reference/reexports.md)
  [`augment`](https://pkg.robjhyndman.com/vital/reference/reexports.md)
  [`estimate`](https://pkg.robjhyndman.com/vital/reference/reexports.md)
  [`glance`](https://pkg.robjhyndman.com/vital/reference/reexports.md) :
  Objects exported from other packages
