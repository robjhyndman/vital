# Function performs predictions of k and life expectancy based on leecarter results (in lcaout)

#' Forecasts using Lee-Carter method.
#'
#' The kt coefficients are forecast using a random walk with drift.
#' The forecast coefficients are then multiplied by bx to obtain a forecast
#' demographic rate curve.
#'
#' @param object Output from \code{\link{lee_carter}}.
#' @param h Number of years ahead to forecast.
#' @param se Method used for computation of standard error. Possibilities: \dQuote{innovdrift} (innovations and drift) and \dQuote{innovonly} (innovations only).
#' @param jumpchoice Method used for computation of jumpchoice. Possibilities: \dQuote{actual} (use actual rates from final year) and \dQuote{fit} (use fitted rates).
#' @param level Confidence level for prediction intervals.
#' @param ... Other arguments.
#'
#' @return Object of class \code{fm_forecast} with the following components:
#' \item{age}{Ages from \code{object}.}
#' \item{year}{Years from \code{object}.}
#' \item{rate}{List of matrices containing forecasts, lower bound and upper bound of prediction intervals.
#'   Point forecast matrix takes the same name as the series that has been forecast.}
#' \item{fitted}{Matrix of one-step forecasts for historical data}
#' Other components included are
#' \item{e0}{Forecasts of life expectancies (including lower and upper bounds)}
#' \item{kt.f}{Forecasts of coefficients from the model.}
#' \item{type}{Data type.}
#' \item{model}{Details about the fitted model}
#'
#' @author Rob J Hyndman
#' @examples
#' library(dplyr)
#' ausf_lca <- aus_mortality |>
#'   filter(Sex == "female", State == "Australia") |>
#'   lee_carter()
#' ausf_fcast <- forecast(ausf_lca, 50)
#' ausf_fcast |> filter(Age == 60) |> autoplot(aus_mortality)
#' @keywords models
#' @export
forecast.lca_model <- function(object,
      h = 50, se = c("innovdrift", "innovonly"),
      jumpchoice = c("fit", "actual"), level = 80, ...) {

  # Forecast all kt series using random walks with drift terms
  fc <- object$time |>
    fabletools::model(rw=fable::RW(kt ~ drift())) |>
    forecast(h = h)

  # Create forecasts of mortality series
  keys <- tsibble::key_vars(object$time)
  df <- tidyr::nest(object$age, agedf = -!!keys)
  time_df <- tidyr::nest(fc, timedf = -!!keys)
  df <- df |> dplyr::left_join(time_df, by=keys)
  mx.forecast <- purrr::map2(df[["agedf"]], df[["timedf"]],
              function(x,y,agevar) {
                h <- NROW(y)
                nages <- NROW(x)
                idx <- tsibble::index_var(y)
                out <- tidyr::expand_grid(Year = y[[idx]], Age = x[[agevar]]) |>
                  dplyr::left_join(x, by="Age") |>
                  dplyr::left_join(y, by="Year") |>
                  dplyr::mutate(Mortality = exp(ax + bx * kt))
              },
    agevar = object$agevar
  )
  # Package results as a fable object
  df |>
    dplyr::select(-agedf, -timedf) |>
    dplyr::mutate(mx = mx.forecast) |>
    tidyr::unnest(mx) |>
    dplyr::select(-ax, -bx, -kt, -.mean) |>
    fabletools::as_fable(index = Year, key = c("Age", keys, ".model"), dist=Mortality, response="Mortality")
}

globalVariables(c("agedf","timedf",".mean","Year","Mortality"))
