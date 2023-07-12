#' @export
forecast.mdl_vtl_df <- function(
    object, new_data = NULL, h = NULL, point_forecast = list(.mean = mean), ...
  ) {
  mdls <- mable_vars(object)
  if (!is.null(h) && !is.null(new_data)) {
    warn("Input forecast horizon `h` will be ignored as `new_data` has been provided.")
    h <- NULL
  }
  if (!is.null(new_data)) {
    keys <- key_vars(new_data)
    agevar <- attributes(new_data)$agevar
    keys_noage <- keys[keys != agevar]
    index <- index_var(new_data)
    object <- bind_new_data(object, new_data)
  }
  kv <- c(key_vars(object), ".model")
  object <- dplyr::mutate_at(as_tibble(object), vars(!!!mdls),
    forecast,
    new_data = object[["new_data"]], h = h, point_forecast = point_forecast,
    ..., key_data = key_data(object)
  )
  object <- tidyr::pivot_longer(object, !!mdls,
    names_to = ".model",
    values_to = ".fc"
  )
  fbl_attr <- attributes(object$.fc[[1]])
  out <- suppressWarnings(
    unnest_tsbl(as_tibble(object)[c(kv, ".fc")], ".fc", parent_key = kv)
  )
  build_fable(out, response = fbl_attr$response, distribution = fbl_attr$dist)
}

#' @export
forecast.mdl_vtl_ts <- function(
    object, new_data = NULL, h = NULL, bias_adjust = NULL,
    simulate = FALSE, bootstrap = FALSE, times = 5000,
    point_forecast = list(.mean = mean), ...) {
  if (!is.null(h) && !is.null(new_data)) {
    warn("Input forecast horizon `h` will be ignored as `new_data` has been provided.")
    h <- NULL
  }
  if (!is.null(bias_adjust)) {
    deprecate_warn("0.2.0", "forecast(bias_adjust = )", "forecast(point_forecast = )")
    point_forecast <- if (bias_adjust) {
      list(.mean = mean)
    } else {
      list(.median = stats::median)
    }
  }
  if (is.null(new_data)) {
    new_data <- make_future_data(object$data, h)
  }
  idx <- index_var(new_data)
  mv <- measured_vars(new_data)
  resp_vars <- vapply(object$response, expr_name, character(1L), USE.NAMES = FALSE)
  dist_col <- if (length(resp_vars) > 1) {
    ".distribution"
  } else {
    resp_vars
  }
  if (NROW(new_data) == 0) {
    new_data[[dist_col]] <- distributional::new_dist(dimnames = resp_vars)
    fbl <- build_fable(new_data, response = resp_vars, distribution = !!sym(dist_col))
    return(fbl)
  }
  if (simulate || bootstrap) {
    fc <- generate(object, new_data, bootstrap = bootstrap, times = times, ...)
    fc <- unname(split(
      object$transformation[[1]](fc[[".sim"]]),
      fc[[index_var(fc)]]
    ))
    fc <- distributional::dist_sample(fc)
  } else {
    object$model$stage <- "forecast"
    object$model$add_data(new_data)
    specials <- tryCatch(parse_model_rhs(object$model), error = function(e) {
      abort(sprintf(
        "%s\n  Unable to compute required variables from provided `new_data`.\n  Does your model require extra variables to produce forecasts?",
        e$message
      ))
    }, interrupt = function(e) {
      stop("Terminated by user", call. = FALSE)
    })
    object$model$remove_data()
    object$model$stage <- NULL
    fc <- forecast(object$fit, new_data,
      specials = specials,
      times = times, ...
    )
  }
  bt <- map(object$transformation, function(x) {
    trans <- x %@% "inverse"
    inv_trans <- `attributes<-`(x, NULL)
    req_vars <- setdiff(all.vars(body(trans)), names(formals(trans)))
    if (any(req_vars %in% names(new_data))) {
      trans <- lapply(vec_chop(new_data[req_vars]), function(transform_data) {
        set_env(trans, new_environment(
          transform_data,
          get_env(trans)
        ))
      })
      attr(trans, "inverse") <- lapply(
        vec_chop(new_data[req_vars]),
        function(transform_data) {
          set_env(inv_trans, new_environment(
            transform_data,
            get_env(inv_trans)
          ))
        }
      )
      trans
    } else {
      structure(list(trans), inverse = list(inv_trans))
    }
  })
  is_transformed <- vapply(
    bt, function(x) !is_symbol(body(x[[1]])),
    logical(1L)
  )
  if (length(bt) > 1) {
    if (any(is_transformed)) {
      abort("Transformations of multivariate forecasts are not yet supported")
    }
  }
  if (any(is_transformed)) {
    if (identical(unique(dist_types(fc)), "dist_sample")) {
      fc <- distributional::dist_sample(.mapply(exec, list(
        bt[[1]],
        distributional::parameters(fc)$x
      ), MoreArgs = NULL))
    } else {
      bt <- bt[[1]]
      fc <- distributional::dist_transformed(fc, `attributes<-`(
        bt,
        NULL
      ), bt %@% "inverse")
    }
  }
  dimnames(fc) <- resp_vars
  new_data[[dist_col]] <- fc
  point_fc <- compute_point_forecasts(fc, point_forecast)
  new_data[names(point_fc)] <- point_fc
  cn <- c(dist_col, names(point_fc))
  attrs <- attributes(new_data)
  agevar <- attrs$agevar
  fbl <- build_tsibble_meta(
    as_tibble(new_data)[unique(c(idx,agevar, cn, mv))],
    key_data(new_data), index = idx, index2 = idx,
    ordered = is_ordered(new_data), interval = attrs$interval
  )
  build_fable(fbl, response = resp_vars, distribution = !!sym(dist_col))
}

#' Forecasts using Lee-Carter method.
#'
#' The kt coefficients are forecast using a random walk with drift.
#' The forecast coefficients are then multiplied by bx to obtain a forecast
#' demographic rate curve.
#'
#' @param object Output from \code{\link{lee_carter}}.
#' @param h Number of years ahead to forecast.
#' @param se Method used for computation of standard error.
#' Possibilities: \dQuote{innovdrift} (innovations and drift) and \dQuote{innovonly} (innovations only).
#' @param jumpchoice Method used for computation of jumpchoice.
#' Possibilities: \dQuote{actual} (use actual rates from final year) and \dQuote{fit} (use fitted rates).
#' The original Lee-Carter method used 'fit' (the default), but Lee and Miller (2001)
#' and most other authors prefer 'actual' (the default).
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
#' @references Lee, R D, and Carter, L R (1992) Modeling and forecasting US mortality.
#' \emph{Journal of the American Statistical Association}, \bold{87}, 659-671.
#' @references Lee R D, and Miller T (2001). Evaluating the performance of the Lee-Carter
#'   method for forecasting mortality. \emph{Demography}, \bold{38}(4), 537–549.
#'
#' @seealso \code{\link{lee_carter}()}
#' @author Rob J Hyndman
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' ausf_lc <- aus_mortality |>
#'   filter(Sex == "female", State == "Australia") |>
#'   lee_carter()
#' ausf_fcast <- forecast(ausf_lc, 50)
#' ausf_fcast |>
#'   filter(Age == 60) |>
#'   autoplot(aus_mortality)
#' rainbow_plot(ausf_fcast, .vars = .mean) +
#'   scale_y_log10()
#'
#' @keywords models
#' @export
forecast.lc_model <- function(
    object, h = 50,
    se = c("innovdrift", "innovonly"), jumpchoice = c("fit", "actual"),
    level = 80, ...) {
  # Forecast all kt series using random walks with drift terms
  fc <- object$time |>
    fabletools::model(rw = fable::RW(kt ~ drift())) |>
    forecast(h = h)

  # Create forecasts of mortality series
  keys <- tsibble::key_vars(object$time)
  df <- tidyr::nest(object$age, agedf = -!!keys)
  time_df <- tidyr::nest(fc, timedf = -!!keys)
  df <- df |> dplyr::left_join(time_df, by = keys)
  mx.forecast <- purrr::map2(df[["agedf"]], df[["timedf"]],
    function(x, y, agevar) {
      h <- NROW(y)
      nages <- NROW(x)
      idx <- tsibble::index_var(y)
      out <- tidyr::expand_grid(Year = y[[idx]], Age = x[[agevar]]) |>
        dplyr::left_join(x, by = "Age") |>
        dplyr::left_join(y, by = "Year") |>
        dplyr::mutate(Mortality = exp(ax + bx * kt))
    },
    agevar = object$agevar
  )
  # Package results as a fable object
  out <- df |>
    dplyr::select(-agedf, -timedf) |>
    dplyr::mutate(mx = mx.forecast) |>
    tidyr::unnest(mx) |>
    dplyr::select(-ax, -bx, -kt, -.mean) |>
    fabletools::as_fable(index = Year, key = c("Age", keys, ".model"), dist = Mortality, response = "Mortality")
  out$.mean <- mean(out$Mortality)
  return(out)
}

make_future_data <- function (.data, h = NULL) {
    n <- get_frequencies(h, .data, .auto = "smallest")
    if (length(n) > 1) {
        warn("More than one forecast horizon specified, using the smallest.")
        n <- min(n)
    }
    if (is.null(h))
        n <- n * 2
    out <- tsibble::new_data(.data, round(n))
    indexvar <- index_var(out)
    agevar <- attributes(.data)$agevar
    .ages <- .data[[agevar]] |> unique() |> sort()
    out <- tidyr::expand_grid(out, .ages)
    colnames(out)[colnames(out) == ".ages"] <- agevar
    as_tsibble(out, index = indexvar, key = agevar) |>
      as_vital(.age = agevar)
}

compute_point_forecasts <- function (distribution, measures) {
  map(measures, calc, distribution)
}
calc <- function (f, ...) {
    f(...)
}

globalVariables(c("agedf", "timedf", ".mean", "Year", "Mortality"))
