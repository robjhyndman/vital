% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/australia.R
\name{aus_mortality}
\alias{aus_mortality}
\title{Australian mortality data}
\format{
Time series of class \code{vital}
}
\source{
Australian Human Mortality Database
}
\description{
\code{aus_mortality} is an annual \code{vital} with three values:
\tabular{ll}{
Mortality: \tab Mortality rate \cr
Exposure:  \tab Population at 30 June each year\cr
Deaths:    \tab Number of deaths \cr
}
}
\details{
The data is disaggregated using four keys:
\tabular{ll}{
Age:   \tab Age at death\cr
Sex:   \tab male or female \cr
State: \tab State of Australia\cr
Code: \tab Short code for state\cr
}
The age group 100 also includes people who died aged older than 100.
The data up to 1970 were taken from the Australian Demographic Data Bank
(\url{https://pkg.robjhyndman.com/addb/}). From 1971, the data come from the
Australian Human Mortality Database (\url{https://aushd.org}). There may be
some discontinuities introduced due to different methods being used to prepare
the data before and after 1971. Note that "ACTOT" includes both the ACT and overseas territories
and is only available up to 2003. The data exclusively from the ACT begins in 1971.
}
\examples{
library(ggplot2)
aus_mortality
aus_mortality |>
  dplyr::filter(State == "Victoria", Sex != "total") |>
  autoplot(Exposure) +
  ylab("Population at 30 June (thousands)")

}
\keyword{datasets}
