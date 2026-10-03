#' Functional data model
#'
#' Functional data model of mortality or fertility rates as a function of age.
#' `FDM()` returns a functional data model applied to the formula's response
#' variable as a function of age.
#'
#' @aliases report.FDM
#'
#' @param formula Model specification.
#' @param order Number of principal components to fit. Must be at least 1.
#' @param ts_model_fn Univariate time series modelling function for the coefficients. Any
#' model that works with the fable package is ok. Default is [fable::ARIMA()].
#' @param coherent If TRUE, fitted models are stationary, other than for the case of
#' a key variable taking the value `geometric_mean` or `mean`. This is designed to work with
#' vitals produced using \code{\link{make_pr}()} and \code{\link{make_sd}}.
#' Default is FALSE.
#' @param coherent_ts_model_fn Time series modelling function to be used for coherent fitting.
#' `ts_model_fn` will be used for the `geometric_mean` or `mean` variables, with the
#' other variables being modelled using `coherent_ts_model_fn`. Default is [fable::ARFIMA()].
#' @param ... Not used.
#'
#' @references Hyndman, R. J., and Ullah, S. (2007) Robust forecasting of
#' mortality and fertility rates: a functional data approach.
#' *Computational Statistics & Data Analysis*, 5, 4942-4956.
#' <https://robjhyndman.com/publications/funcfor/>
#' @references Hyndman, R. J., Booth, H., & Yasmeen, F. (2013). Coherent mortality
#' forecasting: the product-ratio method with functional time series models.
#' *Demography*, 50(1), 261-283.
#' <https://robjhyndman.com/publications/coherentfdm/>
#' @author Rob J Hyndman
#' @return A model specification.
#'
#' @examples
#' hu <- norway_mortality |>
#'   dplyr::filter(Sex == "Female", Year > 2010) |>
#'   smooth_mortality(Mortality) |>
#'   model(hyndman_ullah = FDM(log(.smooth)))
#' report(hu)
#' autoplot(hu)
#' @export
FDM <- function(
  formula,
  order = 6,
  ts_model_fn = fable::ARIMA,
  coherent = FALSE,
  coherent_ts_model_fn = fable::ARFIMA,
  ...
) {
  if (
    !is.numeric(order) || length(order) != 1L || order < 1 || order %% 1 != 0
  ) {
    stop("order must be a positive integer")
  }
  # Identify the coherent model here, as functions passed to parallel workers
  # may no longer be identical to those in the fable namespace
  coherent_ts_model <- if (identical(coherent_ts_model_fn, fable::ARIMA)) {
    "ARIMA"
  } else if (identical(coherent_ts_model_fn, fable::ARFIMA)) {
    "ARFIMA"
  } else {
    NULL
  }
  if (coherent && is.null(coherent_ts_model)) {
    stop("coherent_ts_model_fn must be fable::ARIMA or fable::ARFIMA")
  }
  if (!coherent) {
    coherent <- NULL
  }
  fd_model <- new_model_class("fdm", train = train_fdm)
  new_model_definition(
    fd_model,
    !!enquo(formula),
    order = order,
    ts_model_fn = ts_model_fn,
    coherent = coherent,
    coherent_ts_model = coherent_ts_model,
    ...
  )
}

train_fdm <- function(
  .data,
  specials,
  order,
  ts_model_fn,
  coherent,
  coherent_ts_model,
  ...
) {
  indexvar <- index_var(.data)
  vvar <- vital_var_list(.data)
  agevar <- vvar$age
  measures <- measured_vars(.data)
  measures <- measures[!(measures %in% c(agevar, vvar$population))]
  measures <- measures[1]
  out <- fdm(
    .data,
    order = order,
    ts_model_fn = ts_model_fn,
    coherent = coherent,
    coherent_ts_model = coherent_ts_model
  )

  fitted <- out$data |>
    mutate(
      .innov = .data[[measures]] - .fitted,
      .innov = if_else(.innov < -1e20, NA, .innov),
    ) |>
    select(all_of(c(indexvar, agevar, ".fitted", ".innov")))

  ts_models <- out$ts_models
  out$data <- out$ts_models <- NULL
  structure(
    list(
      model = out,
      fitted = fitted,
      ts_models = ts_models,
      nobs = sum(!is.na(.data[[measures]]))
    ),
    class = "FDM"
  )
}

#' @rdname forecast
#' @export

forecast.FDM <- function(
  object,
  new_data = NULL,
  h = NULL,
  point_forecast = list(.mean = mean),
  simulate = FALSE,
  bootstrap = FALSE,
  times = 5000,
  ...
) {
  # With simulate or bootstrap, forecast.mdl_vtl_ts() uses generate() rather
  # than this method. The arguments are included so they show in the docs.
  # Forecast all beta series using stored models
  h <- length(unique(new_data[[index_var(new_data)]]))
  fc <- purrr::map(object$ts_models, function(x) {
    forecast(x, h = h) |>
      select(-.mean, -.model) |>
      as_tibble()
  })
  indexvar <- index_var(object$model$by_t)
  fc <- purrr::reduce(fc, left_join, by = indexvar)

  # Create forecasts of response series
  agevar <- colnames(object$model$by_x)[1]
  fc <- new_data |>
    left_join(object$model$by_x, by = agevar) |>
    left_join(fc, by = indexvar)
  fc$out <- fc$mean
  for (i in seq_along(object$ts_models)) {
    fc$out <- fc$out + fc[[paste0("beta", i)]] * fc[[paste0("phi", i)]]
  }
  fc |>
    pull(out)
}

#' @export
generate.FDM <- function(
  x,
  new_data = NULL,
  h = NULL,
  bootstrap = FALSE,
  times = 1,
  ...
) {
  agevar <- age_var(new_data)
  indexvar <- index_var(new_data)
  if (times != length(unique(new_data$.rep))) {
    stop("`times` must equal the number of replicates (`.rep`) in `new_data`")
  }

  # Simulate all beta series using stored models
  h <- length(unique(new_data[[index_var(new_data)]]))
  fc <- purrr::map(x$ts_models, function(x) {
    out <- generate(x, h = h, bootstrap = bootstrap, times = times) |>
      as_tibble()
    out$.model <- out$.innov <- NULL
    return(out)
  })
  fc <- purrr::reduce(fc, left_join, by = c(indexvar, ".rep"))
  names(fc)[-(1:2)] <- names(x$ts_models)

  # Create simulations of response series
  fc <- new_data |>
    left_join(x$model$by_x, by = agevar) |>
    left_join(fc, by = c(indexvar, ".rep"))
  fc$out <- fc$mean
  for (i in seq_along(x$ts_models)) {
    fc$out <- fc$out + fc[[paste0("beta", i)]] * fc[[paste0("phi", i)]]
  }
  transmute(fc, .sim = out)
}

#' @export
glance.FDM <- function(x, ...) {
  tibble(
    nobs = x$nobs,
    varprop = sum(x$model$varprop)
  )
}

#' @export
tidy.FDM <- function(x, ...) {
  return(NULL)
}

#' @export
report.FDM <- function(object, ...) {
  cat("\n")
  cat("Basis functions\n")
  print(object$model$by_x, n = 5)
  cat("\nCoefficients\n")
  print(object$model$by_t, n = 5)
  cat("\nTime series models\n")
  models <- names(object$ts_models)
  for (i in seq_along(object$ts_models)) {
    cat("  ", models[i], ": ")
    cat(model_sum(object$ts_models[[i]]$fit[[1]]), "\n")
  }
  cat("\nVariance explained\n  ")
  cat(paste(round(object$model$varprop * 100, 2), collapse = " + "))
  cat(paste0(" = ", round(sum(object$model$varprop) * 100, 2), "%\n"))
}

#' @export
model_sum.FDM <- function(x) {
  paste0("FDM")
}


#' @export
time_components.FDM <- function(object, ...) {
  time_components.LC(object, ...)
}

#' @export
age_components.FDM <- function(object, ...) {
  age_components.LC(object, ...)
}

#' @export
autoplot.FDM <- function(object, show_order = 2, ...) {
  obj_time <- time_components(object)
  obj_x <- age_components(object)

  meanvar <- "mean"
  tmp <- colnames(obj_time)
  timevar <- tmp[grepl("beta", tmp)]
  tmp <- colnames(obj_x)
  agevar <- tmp[grepl("phi", tmp)]
  keys <- head(colnames(object), -1)
  # Cannot show more components than were fitted
  show_order <- min(show_order, length(agevar))

  # Set up list of plots
  p <- list()
  p[[1]] <- age_plot(obj_x, meanvar, keys) + ggplot2::ylab(meanvar)
  for (i in seq(show_order)) {
    p[[i + 1]] <- age_plot(obj_x, agevar[i], keys)
  }
  p[[show_order + 2]] <- patchwork::guide_area()
  for (i in seq(show_order)) {
    p[[i + 2 + show_order]] <- time_plot(obj_time, timevar[i], keys)
  }
  patchwork::wrap_plots(p) +
    patchwork::plot_layout(ncol = show_order + 1, nrow = 2, guides = "collect")
}

# Function based on demography::fdm() and ftsa::ftsm()
# But assumes transformation already done

fdm <- function(
  data,
  order = 6,
  ts_model_fn = fable::ARIMA,
  coherent = NULL,
  coherent_ts_model = "ARFIMA"
) {
  if (is.null(coherent)) {
    coherent <- FALSE
  }
  # Grab variable names
  indexvar <- index_var(data)
  vvar <- vital_var_list(data)
  agevar <- vvar$age
  measures <- measured_vars(data)
  measures <- measures[!(measures %in% c(agevar, vvar$population))]
  measures <- measures[1]

  # Create rates matrix
  year <- sort(unique(data[[indexvar]]))
  ages <- sort(unique(data[[agevar]]))
  mx <- data |>
    as_tibble() |>
    dplyr::select(all_of(c(indexvar, agevar, measures))) |>
    tidyr::pivot_wider(
      values_from = all_of(measures),
      names_from = all_of(agevar)
    )
  # Order rows by time and columns by age
  mx <- mx[match(year, mx[[indexvar]]), as.character(ages)]
  mx <- as.matrix(mx)
  mx[mx == -Inf] <- NA
  # PC decomposition
  y.pca <- fdpca(mx, x = ages, order = order)

  # Compute fitted values
  fits <- as.data.frame(y.pca$basis %*% t(y.pca$coeff))
  colnames(fits) <- year
  fits <- fits |>
    dplyr::mutate(Age = ages) |>
    tidyr::pivot_longer(-Age, names_to = "Year", values_to = ".fitted")
  colnames(fits)[1:2] <- c(agevar, indexvar)
  fits[[indexvar]] <- as.numeric(fits[[indexvar]])

  # Add fitted values and residuals to original data
  output <- data |>
    as_tibble() |>
    dplyr::left_join(fits, by = c(indexvar, agevar)) |>
    as_vital(index = sym(indexvar), key = sym(agevar), .age = agevar)

  by_x <- as_tibble(y.pca$basis)
  by_x[[agevar]] <- sort(unique(data[[agevar]]))
  by_x <- by_x |>
    select(sym(agevar), everything())
  by_t <- as_tibble(y.pca$coeff)
  by_t[[indexvar]] <- sort(unique(data[[indexvar]]))
  by_t <- as_tsibble(by_t, index = sym(indexvar)) |>
    select(sym(indexvar), everything())

  # Fit ts_models to coefficients
  ts_coefs <- names(by_t)
  ts_coefs <- ts_coefs[grepl("beta", ts_coefs)]
  fits <- purrr::map(ts_coefs, function(x) {
    if (coherent) {
      if (identical(coherent_ts_model, "ARIMA")) {
        mod <- by_t |>
          fabletools::model(
            fit = fable::ARIMA(
              !!sym(x),
              order_constraint = (p + q + P + Q <= 6) & (d + D == 0)
            )
          )
      } else {
        # ARFIMA, the only other choice allowed by FDM()
        mod <- by_t |>
          fabletools::model(
            fit = fable::ARFIMA(
              !!sym(x),
              order_constraint = (p + q <= 6)
            )
          ) |>
          suppressWarnings()
      }
    } else {
      mod <- by_t |>
        fabletools::model(fit = ts_model_fn(!!sym(x)))
    }
    return(mod)
  })
  names(fits) <- ts_coefs

  # Return object
  list(
    data = output,
    by_x = by_x,
    by_t = by_t,
    ts_models = fits,
    varprop = y.pca$varprop
  )
}


# Functional PCA

# Interpolating spline through (x, y) evaluated at xout, extrapolated linearly
# beyond the range of x using the slope of the spline at its ends
spline_extrapolate <- function(x, y, xout) {
  f <- stats::splinefun(x, y, method = "fmm")
  out <- f(xout)
  below <- xout < min(x)
  above <- xout > max(x)
  out[below] <- f(min(x)) + f(min(x), deriv = 1) * (xout[below] - min(x))
  out[above] <- f(max(x)) + f(max(x), deriv = 1) * (xout[above] - max(x))
  out
}

# X is a time by age matrix, and x contains the ages of its columns
fdpca <- function(X, x = seq(NCOL(X)), order = 2, ngrid = 500) {
  y <- t(X)
  n <- NCOL(y)
  if (order < 1) {
    stop("Order must be at least 1")
  }
  # Centred data from n time periods have at most n - 1 components
  if (order >= n) {
    stop(
      "order must be less than the number of time periods (",
      n,
      ")",
      call. = FALSE
    )
  }
  if (ngrid < NCOL(X)) {
    stop("Grid should be larger than number of observations per time period.")
  }
  # Interpolate data onto a common grid using interpolating splines
  xx <- seq(min(x), max(x), l = ngrid)
  yy <- matrix(NA, nrow = ngrid, ncol = n)
  for (i in seq(n)) {
    miss <- is.na(y[, i])
    yy[, i] <- spline_extrapolate(x[!miss], y[!miss, i], xx)
  }
  # Compute smooth means
  ax <- rowMeans(yy, na.rm = TRUE)
  # Centre data
  yy <- sweep(yy, 1, ax)
  # Mean term
  coeff <- matrix(1, nrow = n, ncol = 1)
  basis <- matrix(stats::approx(xx, ax, xout = x)$y, ncol = 1)
  colnames(coeff)[1] <- colnames(basis)[1] <- "mean"
  # Compute SVD
  s <- La.svd(t(yy))
  # Eigenvectors and eigenvalues
  Phi <- as.matrix(t(s$vt)[, seq(order)])
  varprop <- s$d^2 / sum(s$d^2)
  # Normalize eigenvectors so they integrate to 1 over age, weighting each
  # age by the width of its interval (all 1 for single years of age)
  Phinorm <- matrix(NA, length(x), order)
  Phinormngrid <- matrix(NA, ngrid, order)
  delta <- xx[2] - xx[1]
  widths <- c(diff(x), utils::tail(diff(x), 1))
  for (i in seq(order)) {
    phi <- stats::approx(xx, Phi[, i], xout = x)$y
    Phinorm[, i] <- phi / sqrt(sum(widths * phi^2))
    Phinormngrid[, i] <- stats::approx(x, Phinorm[, i], xout = xx)$y
  }
  # Extract coeff and basis matrices
  B <- t(yy) %*% Phinormngrid
  colnames(B) <- paste0("beta", seq(order))
  coeffdummy <- B * delta
  colmeanrm <- matrix(colMeans(coeffdummy), dim(B)[2], 1)
  coeff <- cbind(coeff, sweep(coeffdummy, 2, colmeanrm))
  m <- ncol(basis)
  basis <- basis + Phinorm %*% colmeanrm
  colnames(basis) <- "mean"
  for (i in seq(order)) {
    basis <- cbind(basis, Phinorm[, i])
    if (sum(basis[, i + m]) < 0) {
      basis[, i + m] <- -basis[, i + m]
      coeff[, i + m] <- -coeff[, i + m]
    }
  }
  colnames(basis)[m + seq(order)] <- paste0("phi", seq(order))

  # Return results
  return(list(
    basis = basis,
    coeff = coeff,
    varprop = varprop[seq(order)]
  ))
}


utils::globalVariables(c(".model", "out", "object", ".fitted", ".rep"))
utils::globalVariables(c("p", "P", "d", "D", "q", "Q"))
