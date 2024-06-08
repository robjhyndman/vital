#' Functional data model
#'
#' Functional data model of mortality or fertility rates as a function of age.
#' `FDM()` returns a functional data model applied to the formula's response
#' variable as a function of age.
#'
#' @aliases report.FDM
#'
#' @param formula Model specification.
#' @param order Number of principal components to fit.
#' @param ts_model_fn Univariate time series modelling function for the coefficients. Any
#' model that works with the fable package is ok. Default is [fable::ARIMA()].
#' @param coherent If TRUE, fitted models are stationary, other than for the case of
#' a key variable taking the value `geometric_mean`. This is designed to work with
#' vitals produced using \code{\link{make_pr}()}. Default is FALSE. It only works
#' when `ts_model_fn` is \code{\link[fable]{ARIMA}()}.
#' @param ... Not used.
#'
#' @references Hyndman, R. J., and Ullah, S. (2007) Robust forecasting of
#' mortality and fertility rates: a functional data approach.
#' *Computational Statistics & Data Analysis*, 5, 4942-4956.
#' <https://robjhyndman.com/publications/funcfor/>
#' Hyndman, R. J., Booth, H., & Yasmeen, F. (2013). Coherent mortality
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
FDM <- function(formula, order = 6, ts_model_fn = fable::ARIMA,
                coherent = FALSE, ...) {
  if(coherent & !identical(ts_model_fn, fable::ARIMA)) {
    stop("coherent = TRUE only works with ts_model_fn = fable::ARIMA")
  }
  if(!coherent) {
    coherent <- NULL
  }
  fd_model <- new_model_class("fdm", train = train_fdm)
  new_model_definition(fd_model, !!enquo(formula),
    order = order, ts_model_fn = ts_model_fn, coherent = coherent, ...)
}

train_fdm <- function(.data, specials, order, ts_model_fn, coherent, ...) {
  indexvar <- index_var(.data)
  vvar <- vital_var_list(.data)
  agevar <- vvar$age
  measures <- measured_vars(.data)
  measures <- measures[!(measures %in% c(agevar, vvar$population))]
  measures <- measures[1]
  out <- fdm(.data, order = order, ts_model_fn = ts_model_fn, coherent = coherent)

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

forecast.FDM <- function(object, new_data = NULL, h = NULL, point_forecast = list(.mean = mean),
  simulate = FALSE, bootstrap = FALSE, times = 5000, ...) {

# simulation/bootstrap not actually used here as forecast.mdl_vtl_ts
# handles this using generate() and forecast.FDM is never called.
# The arguments are included to avoid a warning message.

  # Forecast all beta series using stored models
  h <- length(unique(new_data[[index_var(new_data)]]))
  fc <- purrr::map(object$ts_model, function(x) {
      forecast(x, h = h) |>
      select(-.mean, -.model) |>
      as_tibble()
    }
  )
  indexvar <- index_var(object$model$by_t)
  fc <- purrr::reduce(fc, left_join, by=indexvar)

  # Create forecasts of response series
  agevar <- colnames(object$model$by_x)[1]
  fc <- new_data |>
    left_join(object$model$by_x, by = agevar) |>
    left_join(fc, by = indexvar)
  fc$out <- fc$mean
  for(i in seq_along(object$ts_model)) {
    fc$out <- fc$out + fc[[paste0("beta", i)]] * fc[[paste0("phi",i)]]
  }
  fc |>
    pull(out)
}

#' @export
generate.FDM <- function(x, new_data = NULL, h = NULL,
  bootstrap = FALSE, times = 1,
  forecast_fn, ...) {
  agevar <- age_var(new_data)
  indexvar <- index_var(new_data)
  if(times != length(unique(new_data$.rep)))
    stop("We have a problem")

  # Simulate all beta series using stored models
  h <- length(unique(new_data[[index_var(new_data)]]))
  fc <- purrr::map(x$ts_models, function(x) {
      out <- generate(x, h = h, bootstrap = bootstrap, times = times) |>
        as_tibble()
      out$.model <- out$.innov <- NULL
      return(out)
    }
  )
  fc <- purrr::reduce(fc, left_join, by=c(indexvar, ".rep"))
  names(fc)[-(1:2)] <- names(x$ts_models)

  # Create simulations of response series
  fc <- new_data |>
    left_join(x$model$by_x, by = agevar) |>
    left_join(fc, by = c(indexvar, ".rep"))
  fc$out <- fc$mean
  for(i in seq_along(x$ts_models)) {
    fc$out <- fc$out + fc[[paste0("beta", i)]] * fc[[paste0("phi",i)]]
  }
  fc |>
    select(.rep, sym(indexvar), !!agevar, .sim=out) |>
    transmute(group_by_key(new_data), .sim)
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
  print(object$model$by_x, n=5)
  cat("\nCoefficients\n")
  print(object$model$by_t, n=5)
  cat("\nTime series models\n")
  models <- names(object$ts_models)
  for(i in seq_along(object$ts_models)) {
    cat("  ", models[i], ": ")
    cat(model_sum(object$ts_model[[i]]$fit[[1]]), "\n")
  }
  cat("\nVariance explained\n  ")
  cat(paste(round(object$model$varprop*100, 2), collapse=" + "))
  cat(paste0(" = ", round(sum(object$model$varprop)*100, 2), "%\n"))
}

#' @export
model_sum.FDM <- function(x) {
  paste0("FDM")
}


#' @export
time_components.FDM <- function(object, ...) {
  modelname <- attributes(object)$model
  object <- object |>
    mutate(out = purrr::map(object[[modelname]], function(x){x$fit$model})) |>
    as_tibble()
  object[[modelname]] <- NULL
  index <- index_var(object$out[[1]]$by_t)
  keys <- head(colnames(object), -1)
  object$out <- lapply(object$out, function(x) as_tibble(x$by_t))
  object |>
    tidyr::unnest("out") |>
    as_tsibble(index = index, key=all_of(keys))
}

#' @export
age_components.FDM <- function(object, ...) {
  modelname <- attributes(object)$model
  object <- object |>
    mutate(out = purrr::map(object[[modelname]], function(x){x$fit$model})) |>
    as_tibble()
  object[[modelname]] <- NULL
  object$out  <- lapply(object$out, function(x) as_tibble(x$by_x))
  object |> tidyr::unnest("out")
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
  index <- index_var(obj_time)
  keys <- head(colnames(object), -1)

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

fdm <- function(data, order = 6, ts_model_fn = fable::ARIMA, coherent = NULL) {
  if(is.null(coherent)) {
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
    tidyr::pivot_wider(values_from = all_of(measures), names_from = all_of(agevar))
  mx[[indexvar]] <- NULL
  mx <- as.matrix(mx)
  mx[mx == -Inf] <- NA
  # PC decomposition
  y.pca <- fdpca(mx, order = order)

  # Compute fitted values
  fits <- as.data.frame(y.pca$basis %*% t(y.pca$coeff))
  colnames(fits) <- year
  fits <- fits |>
    dplyr::mutate(Age = ages) |>
    tidyr::pivot_longer(-Age, names_to = "Year", values_to = ".fitted") |>
    dplyr::mutate(Year = as.integer(Year))

  # Add fitted values and residuals to original data
  output <- data |>
    as_tibble() |>
    dplyr::left_join(fits, by = c("Year", "Age")) |>
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
    if(coherent) {
      mod <- by_t |>
        fabletools::model(fit = ts_model_fn(!!sym(x),
          order_constraint = (p + q + P + Q <= 6) & (d + D == 0))
        )
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

fdpca <- function(X, order = 2, ngrid = 500) {
  y <- t(X)
  x <- seq(NCOL(X))
  n <- NCOL(y)
  m <- length(x)
  if (order < 0) {
    stop("Order must be at least 0")
  }
  if (ngrid < n) {
    stop("Grid should be larger than number of observations per time period.")
  }
  # Interpolate data onto grid using interpolating splines
  yy <- matrix(NA, nrow = ngrid, ncol = n)
  for (i in seq(n)) {
    miss <- is.na(y[, i])
    yy[, i] <- stats::spline(x[!miss], y[!miss, i], n = ngrid)$y
  }
  xx <- seq(min(x), max(x), l = ngrid)
  # Compute smooth means
  ax <- rowMeans(yy, na.rm = TRUE)
  # Centre data
  yy <- sweep(yy, 1, ax)
  # Standard error in mean estimate
  axse <- stats::approx(xx, sqrt(apply(yy, 1, stats::var) / n), xout = x)$y
  # Set up coeff and basis for order 0
  coeff <- matrix(1, nrow=n, ncol=1)
  basis <- matrix(stats::approx(xx, ax, xout = x)$y, ncol=1)
  colnames(coeff)[1] <- colnames(basis)[1] <- "mean"
  if (order == 0) {
    return(list(
      basis = basis, coeff = coeff,
      v = rep(1, n), mean.se = axse
    ))
  }
  # Compute SVD
  s <- La.svd(t(yy))
  # Eigenvectors and eigenvalues
  Phi <- as.matrix(t(s$vt)[, seq(order)])
  eigen_value <- varprop <- s$d^2
  varprop <- varprop / sum(s$d^2)
  # Normalize eigenvectors
  Phinorm <- matrix(NA, length(x), order)
  Phinormngrid <- matrix(NA, ngrid, order)
  delta <- xx[2] - xx[1]
  for (i in seq(order)) {
    Phinorm[, i] <- stats::approx(xx, Phi[, i], xout = x)$y / delta /
      (sqrt(sum((stats::approx(xx, Phi[, i], xout = x)$y / delta)^2)))
    Phinormngrid[, i] <- stats::approx(x, Phinorm[, i], xout = xx)$y
  }
  # Extract coeff and basis matrices
  B <- t(yy) %*% Phinormngrid
  v <- colSums((yy - Phinormngrid %*% t(B))^2) * delta
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
    basis = basis, coeff = coeff,
    varprop = varprop[seq(order)], eigen_value = eigen_value,
    v = v, mean.se = axse
  ))
}



utils::globalVariables(c(".model", "out", "object", ".fitted", ".rep"))
utils::globalVariables(c("p", "P", "d", "D", "q", "Q", "constant"))
