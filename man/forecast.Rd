% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FDM.R, R/LC.R, R/fmean.R, R/forecast.R
\name{forecast.FDM}
\alias{forecast.FDM}
\alias{forecast.LC}
\alias{forecast.FMEAN}
\alias{forecast.mdl_vtl_df}
\title{Produce forecasts from a vital model}
\usage{
\method{forecast}{FDM}(
  object,
  new_data = NULL,
  h = NULL,
  point_forecast = list(.mean = mean),
  simulate = FALSE,
  bootstrap = FALSE,
  times = 5000,
  seed = NULL,
  ...
)

\method{forecast}{LC}(
  object,
  new_data = NULL,
  h = NULL,
  point_forecast = list(.mean = mean),
  simulate = FALSE,
  bootstrap = FALSE,
  times = 5000,
  seed = NULL,
  se = c("innovdrift", "innovonly"),
  jumpchoice = c("fit", "actual"),
  ...
)

\method{forecast}{FMEAN}(
  object,
  new_data = NULL,
  h = NULL,
  point_forecast = list(.mean = mean),
  simulate = FALSE,
  bootstrap = FALSE,
  times = 5000,
  seed = NULL,
  ...
)

\method{forecast}{mdl_vtl_df}(
  object,
  new_data = NULL,
  h = NULL,
  point_forecast = list(.mean = mean),
  simulate = FALSE,
  bootstrap = FALSE,
  times = 5000,
  seed = NULL,
  ...
)
}
\arguments{
\item{object}{A mable containing one or more models.}

\item{new_data}{A `tsibble` containing future information used to forecast.}

\item{h}{Number of time steps ahead to forecast. This can be used instead of \code{new_data}
when there are no covariates in the model. It is ignored if \code{new_data} is provided.}

\item{point_forecast}{A list of functions used to compute point forecasts from the forecast distribution.}

\item{simulate}{If  `TRUE`, then forecast distributions are computed using simulation from a parametric model.}

\item{bootstrap}{If `TRUE`, then forecast distributions are computed using simulation with resampling.}

\item{times}{The number of sample paths to use in estimating the forecast distribution when `bootstrap = TRUE`.}

\item{seed}{The seed for the random generation from distributions.}

\item{...}{Additional arguments passed to the specific model method.}

\item{se}{Method used for computation of standard error.
Possibilities: \dQuote{innovdrift} (innovations and drift) and \dQuote{innovonly} (innovations only).}

\item{jumpchoice}{Method used for computation of jumpchoice.
Possibilities: \dQuote{actual} (use actual rates from final year) and \dQuote{fit} (use fitted rates).
The original Lee-Carter method used 'fit' (the default), but Lee and Miller (2001)
and most other authors prefer 'actual' (the default).}
}
\value{
A fable containing the following columns:
- `.model`: The name of the model used to obtain the forecast. Taken from
  the column names of models in the provided mable.
- The forecast distribution. The name of this column will be the same as the
  dependent variable in the model(s). If multiple dependent variables exist,
  it will be named `.distribution`.
- Point forecasts computed from the distribution using the functions in the
  `point_forecast` argument.
- All columns in `new_data`, excluding those whose names conflict with the
  above.
}
\description{
The forecast function allows you to produce future predictions of a vital
model, where the response is a function of age.
The forecasts returned contain both point forecasts and their distribution.
}
\author{
Rob J Hyndman and Mitchell O'Hara-Wild
}