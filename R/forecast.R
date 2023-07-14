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
  final <- build_fable(out, response = fbl_attr$response, distribution = fbl_attr$dist)
  class(final) <- c("fbl_vtl_ts", class(final))
  return(final)
}

#' @export
forecast.mdl_vtl_ts <- function(
    object, new_data = NULL, h = NULL,
    simulate = FALSE, bootstrap = FALSE, times = 5000,
    point_forecast = list(.mean = mean), ...) {
  if (!is.null(h) && !is.null(new_data)) {
    warn("Input forecast horizon `h` will be ignored as `new_data` has been provided.")
    h <- NULL
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
    agevar <- attributes(new_data)$agevar
    fc <- generate(object, new_data, bootstrap = bootstrap, times = times, ...)
    fc_split <- paste(fc[[index_var(fc)]], fc[[agevar]])
    fc <- unname(split(object$transformation[[1]](fc[[".sim"]]), fc_split))
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
      trans <- lapply(vctrs::vec_chop(new_data[req_vars]), function(transform_data) {
        set_env(trans, new_environment(
          transform_data,
          get_env(trans)
        ))
      })
      attr(trans, "inverse") <- lapply(
        vctrs::vec_chop(new_data[req_vars]),
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
  is_transformed <- vapply(bt, function(x) !is_symbol(body(x[[1]])), logical(1L))
  if (length(bt) > 1) {
    if (any(is_transformed)) {
      abort("Transformations of multivariate forecasts are not yet supported")
    }
  }
  if (any(is_transformed)) {
    if (identical(unique(dist_types(fc)), "dist_sample")) {
      fc <- distributional::dist_sample(.mapply(exec, list(
        bt[[1]], distributional::parameters(fc)$x), MoreArgs = NULL))
    } else {
      bt <- bt[[1]]
      fc <- distributional::dist_transformed(fc, `attributes<-`(
        bt, NULL ), bt %@% "inverse")
    }
  }
  dimnames(fc) <- resp_vars
  new_data[[dist_col]] <- fc
  point_fc <- compute_point_forecasts(fc, point_forecast)
  new_data[names(point_fc)] <- point_fc
  cn <- c(dist_col, names(point_fc))
  attrs <- attributes(new_data)
  agevar <- attrs$agevar
  fbl <- tsibble::build_tsibble_meta(
    as_tibble(new_data)[unique(c(idx, agevar, cn, mv))],
    key_data(new_data), index = idx, index2 = idx,
    ordered = is_ordered(new_data), interval = tsibble::interval(new_data)
  )
  final <- build_fable(fbl, response = resp_vars, distribution = !!sym(dist_col))
  class(final) <- c("fbl_vtl_ts", class(final))
  attr(final, "agevar") <- agevar
  return(final)
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

globalVariables(c("agedf", "timedf", ".mean", "Year", "Mortality", "fc"))
