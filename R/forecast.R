#' Produce forecasts from a vital model
#'
#' The forecast function allows you to produce future predictions of a vital
#' model, where the response is a function of age.
#' The forecasts returned contain both point forecasts and their distribution.
#'
#' @param object A mable containing one or more models.
#' @param new_data A `tsibble` containing future information used to forecast.
#' @param h Number of time steps ahead to forecast. This can be used instead of `new_data`
#' when there are no covariates in the model. It is ignored if `new_data` is provided.
#' @param point_forecast A list of functions used to compute point forecasts from the forecast distribution.
#' @param simulate If  `TRUE`, then forecast distributions are computed using simulation from a parametric model.
#' @param bootstrap If `TRUE`, then forecast distributions are computed using simulation with resampling.
#' @param times The number of sample paths to use in estimating the forecast distribution when `simulate = TRUE` or `bootstrap = TRUE`.
#' @param ... Additional arguments passed to the specific model method.
#' @author Rob J Hyndman and Mitchell O'Hara-Wild
#'
#' @return
#' A fable containing the following columns:
#' - `.model`: The name of the model used to obtain the forecast. Taken from
#'   the column names of models in the provided mable.
#' - The forecast distribution. The name of this column will be the same as the
#'   dependent variable in the model(s). If multiple dependent variables exist,
#'   it will be named `.distribution`.
#' - Point forecasts computed from the distribution using the functions in the
#'   `point_forecast` argument.
#' - All columns in `new_data`, excluding those whose names conflict with the
#'   above.
#' @examples
#' norway_mortality |>
#'   dplyr::filter(Sex == "Female") |>
#'   model(naive = FNAIVE(Mortality)) |>
#'   forecast(h = 10)
#'
#' @rdname forecast
#' @export
forecast.mdl_vtl_df <- function(
  object,
  new_data = NULL,
  h = NULL,
  point_forecast = list(.mean = mean),
  simulate = FALSE,
  bootstrap = FALSE,
  times = 5000,
  ...
) {
  mdls <- mable_vars(object)
  if (!is.null(h) && !is.null(new_data)) {
    warn(
      "Input forecast horizon `h` will be ignored as `new_data` has been provided."
    )
    h <- NULL
  }
  vvars <- unlist(mable_vital_vars(object))
  kv <- c(key_vars(object), ".model")
  if (!is.null(new_data)) {
    object <- bind_new_data(object, new_data)
  }
  new_data <- object[["new_data"]]
  dots <- list2(...)
  object <- mutate(
    as_tibble(object),
    across(all_of(mdls), function(x) {
      exec(
        forecast,
        x,
        new_data = new_data,
        h = h,
        point_forecast = point_forecast,
        simulate = simulate,
        bootstrap = bootstrap,
        times = times,
        !!!dots
      )
    })
  )
  object <- tidyr::pivot_longer(
    object,
    !!mdls,
    names_to = ".model",
    values_to = ".fc"
  )
  fbl_attr <- attributes(object$.fc[[1]])
  out <- suppressWarnings(
    unnest_tsbl(as_tibble(object)[c(kv, ".fc")], ".fc", parent_key = kv)
  )
  build_vital_fable(
    out,
    response = fbl_attr$response,
    distribution = fbl_attr$dist,
    vitals = vvars
  )
}

#' @export
forecast.mdl_vtl_ts <- function(
  object,
  new_data = NULL,
  h = NULL,
  simulate = FALSE,
  bootstrap = FALSE,
  times = 5000,
  point_forecast = list(.mean = mean),
  ...
) {
  if (!is.null(h) && !is.null(new_data)) {
    warn(
      "Input forecast horizon `h` will be ignored as `new_data` has been provided."
    )
    h <- NULL
  }
  if (is.null(new_data)) {
    new_data <- make_future_data(object$data, h)
  }
  idx <- index_var(new_data)
  mv <- measured_vars(new_data)
  resp_vars <- vapply(
    object$response,
    expr_name,
    character(1L),
    USE.NAMES = FALSE
  )
  dist_col <- if (length(resp_vars) > 1) {
    ".distribution"
  } else {
    resp_vars
  }
  agevar <- age_var(new_data)
  if (NROW(new_data) == 0) {
    new_data[[dist_col]] <- distributional::new_dist(dimnames = resp_vars)
    return(build_vital_fable(
      new_data,
      response = resp_vars,
      distribution = dist_col,
      vitals = vital_vars(object$data)
    ))
  }
  simulated <- simulate || bootstrap
  if (simulated) {
    # Simulations are already back-transformed. Collect them for each row
    sims <- generate(object, new_data, bootstrap = bootstrap, times = times, ...)
    rows <- paste(new_data[[idx]], new_data[[agevar]])
    fc <- split(
      sims[[".sim"]],
      factor(paste(sims[[idx]], sims[[agevar]]), levels = rows)
    )
    fc <- distributional::dist_sample(unname(fc))
  } else {
    filled <- fill_forecast_times(new_data, object$data)
    object$model$stage <- "forecast"
    object$model$add_data(filled$data)
    specials <- tryCatch(
      parse_model_rhs(object$model),
      error = function(e) {
        abort(sprintf(
          "%s\n  Unable to compute required variables from provided `new_data`.
Does your model require extra variables to produce forecasts?",
          e$message
        ))
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
    object$model$remove_data()
    object$model$stage <- NULL
    fc <- forecast(
      object$fit,
      filled$data,
      specials = specials,
      times = times,
      ...
    )[filled$rows]
  }
  is_transformed <- vapply(
    object$transformation,
    function(x) !is_symbol(body(x %@% "inverse")),
    logical(1L)
  )
  if (length(is_transformed) > 1 && any(is_transformed)) {
    abort("Transformations of multivariate forecasts are not yet supported")
  }
  # Back-transform forecast distributions (simulations already are)
  if (any(is_transformed) && !simulated) {
    bt <- map(object$transformation, function(x) {
      trans <- x %@% "inverse"
      inv_trans <- `attributes<-`(x, NULL)
      req_vars <- setdiff(all.vars(body(trans)), names(formals(trans)))
      if (any(req_vars %in% names(new_data))) {
        trans <- lapply(
          vctrs::vec_chop(new_data[req_vars]),
          function(transform_data) {
            set_env(
              trans,
              new_environment(
                transform_data,
                get_env(trans)
              )
            )
          }
        )
        attr(trans, "inverse") <- lapply(
          vctrs::vec_chop(new_data[req_vars]),
          function(transform_data) {
            set_env(
              inv_trans,
              new_environment(
                transform_data,
                get_env(inv_trans)
              )
            )
          }
        )
        trans
      } else {
        structure(list(trans), inverse = list(inv_trans))
      }
    })
    if (identical(unique(dist_types(fc)), "dist_sample")) {
      fc <- distributional::dist_sample(.mapply(
        exec,
        list(
          bt[[1]],
          distributional::parameters(fc)$x
        ),
        MoreArgs = NULL
      ))
    } else {
      bt <- bt[[1]]
      fc <- distributional::dist_transformed(
        fc,
        `attributes<-`(
          bt,
          NULL
        ),
        bt %@% "inverse"
      )
    }
  }
  dimnames(fc) <- resp_vars
  new_data[[dist_col]] <- fc
  point_fc <- compute_point_forecasts(fc, point_forecast)
  new_data[names(point_fc)] <- point_fc
  cn <- c(dist_col, names(point_fc))
  fbl <- tsibble::build_tsibble_meta(
    as_tibble(new_data)[unique(c(idx, key_vars(new_data), cn, mv))],
    key_data(new_data),
    index = idx,
    index2 = idx,
    ordered = is_ordered(new_data),
    interval = tsibble::interval(new_data)
  )
  build_vital_fable(
    fbl,
    response = resp_vars,
    distribution = dist_col,
    vitals = vital_vars(object$data)
  )
}

make_future_data <- function(.data, h = NULL) {
  n <- get_frequencies(h, .data, .auto = "smallest")
  if (length(n) > 1) {
    warn("More than one forecast horizon specified, using the smallest.")
    n <- min(n)
  }
  if (is.null(h)) {
    n <- n * 2
  }
  out <- tsibble::new_data(.data, round(n))
  indexvar <- index_var(out)
  # Keep the type of the index (e.g. integer years)
  out[[indexvar]] <- vctrs::vec_cast(out[[indexvar]], .data[[indexvar]])
  agevar <- age_var(.data)
  # Every future time for each age (and age group label)
  age_keys <- union(agevar, key_vars(.data))
  ages <- dplyr::distinct(as_tibble(.data)[age_keys])
  ages <- ages[order(ages[[agevar]]), ]
  out <- tidyr::expand_grid(unique(as_tibble(out)[indexvar]), ages)
  as_tsibble(out, index = indexvar, key = all_of(age_keys)) |>
    as_vital(.age = agevar)
}

# Forecasts are computed from the end of the training data, so add rows for
# any times missing between then and the end of new_data, copied from the first
# time in new_data. Returns the completed data and the positions of the rows of
# new_data within it.
fill_forecast_times <- function(new_data, train) {
  idx <- index_var(new_data)
  times <- new_data[[idx]]
  last <- max(train[[index_var(train)]])
  if (min(times) <= last) {
    abort("`new_data` must only contain times after the end of the training data.")
  }
  # Step forward by the interval of the training data (e.g. 5 for 5-yearly data)
  step <- tsibble::default_time_units(tsibble::interval(train))
  nsteps <- round(as.numeric(max(times) - last) / step)
  missing <- last + step * seq_len(nsteps)
  missing <- missing[!(missing %in% times)]
  if (length(missing) == 0L) {
    return(list(data = new_data, rows = seq_len(NROW(new_data))))
  }
  first <- as_tibble(new_data)[times == min(times), ]
  extra <- lapply(missing, function(time) {
    first[[idx]] <- vctrs::vec_cast(time, times)
    first
  })
  keys <- key_vars(new_data)
  full <- vctrs::vec_rbind(as_tibble(new_data), !!!extra) |>
    build_tsibble(
      index = !!idx,
      key = !!keys,
      interval = tsibble::interval(train)
    ) |>
    restore_vital(vital_var_list(new_data))
  row_id <- function(x) {
    do.call(paste, unname(as.list(as_tibble(x)[c(idx, keys)])))
  }
  list(data = full, rows = match(row_id(new_data), row_id(full)))
}

compute_point_forecasts <- function(distribution, measures) {
  map(measures, calc, distribution)
}
calc <- function(f, ...) {
  f(...)
}
