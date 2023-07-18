#' Functional data model
#'
#' Functional data model of mortality or fertility rates as a function of age.
#' \code{FDM()} returns a functional data model applied to the formula's response
#' variable as a function of age.
#'
#' @aliases report.FDM
#'
#' @param formula Model specification.
#' @param order Number of principal components to fit.
#' @param ts_model_fn Univariate time series modelling function for the coefficients.
#' @param ... Not used.
#'
#' @references Hyndman, R.J., and Ullah, S. (2007) Robust forecasting of
#' mortality and fertility rates: a functional data approach.
#' \emph{Computational Statistics & Data Analysis}, \bold{51}, 4942-4956.
#' \url{https://robjhyndman.com/publications/funcfor/}
#'
#' @author Rob J Hyndman
#' @return A model specification.
#'
#' @examples
#' hu <- aus_mortality |>
#'   dplyr::filter(State == "Victoria", Sex == "female") |>
#'   model(hyndman_ullah = FDM(Mortality))
#' report(hu)
#' autoplot(hu)
#' @export
FDM <- function(formula, order = 6, ts_model_fn = fable::ARIMA, ...) {
  fd_model <- new_model_class("fdm", train = train_fdm)
  new_model_definition(fd_model, !!enquo(formula),
    order = order, ts_model_fn = ts_model_fn, ...)
}

#' @importFrom stats sd
train_fdm <- function(.data, specials, order, ts_model_fn, ...) {
  attrx <- attributes(.data)
  indexvar <- index_var(.data)
  agevar <- attrx$agevar
  measures <- measured_vars(.data)
  measures <- measures[!(measures %in% c(agevar, attrx$populationvar))]
  out <- fdm(.data, order = order, lambda = 0, ts_model_fn = ts_model_fn)

  fitted <- out$data |>
    mutate(
      .innov = log(.data[[measures]]) - .fitted,
      .innov = if_else(.innov < -1e20, NA, .innov),
      .fitted = exp(.fitted),
      .resid = .data[[measures]] - .fitted
    ) |>
    select(all_of(c(indexvar, agevar, ".fitted", ".resid", ".innov")))

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

#' @param se Method used for computation of standard error.
#' Possibilities: \dQuote{innovdrift} (innovations and drift) and \dQuote{innovonly} (innovations only).
#' @rdname forecast
#' @export

forecast.FDM <- function(object, new_data = NULL, h = NULL, point_forecast = list(.mean = mean),
  simulate = FALSE, bootstrap = FALSE, times = 5000, seed = NULL,
     ...) {

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
    transmute(out = exp(out)) |>
    pull(out)
}

#' @export
generate.FDM <- function(x, new_data = NULL, h = NULL,
  bootstrap = FALSE, times = 1, seed = NULL,
  forecast_fn, ...) {
  agevar <- attributes(new_data)$agevar
  indexvar <- index_var(new_data)
  if(times != length(unique(new_data$.rep)))
    stop("We have a problem")
  # Note that seed has already been set in generate.mdl_vtl_df
  # So it is not re-set here

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
    mutate(.sim = exp(.sim)) |>
    transmute(group_by_key(new_data), .sim)
}

#' @export
glance.FDM <- function(x, ...) {
  tibble(
    sigma2 = var(x$fitted$.resid, na.rm=TRUE),
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
autoplot.FDM <- function(object, show_order = 2, ...) {
  modelname <- attributes(object)$model
  object <- object |>
    mutate(out = purrr::map(object[[modelname]], function(x){x$fit$model})) |>
    as_tibble()
  object[[modelname]] <- NULL

  meanvar <- "mean"
  tmp <- colnames(object$out[[1]]$by_t)
  timevar <- tmp[grepl("beta", tmp)]
  tmp <- colnames(object$out[[1]]$by_x)
  agevar <- tmp[grepl("phi", tmp)]
  index <- index_var(object$out[[1]]$by_t)
  keys <- head(colnames(object), -1)

  # Construct time and age data frames
  obj_time <- obj_x <- object
  obj_time$out <- lapply(obj_time$out, function(x) as_tibble(x$by_t))
  obj_time <- obj_time |>
    tidyr::unnest("out") |>
    as_tsibble(index = index, key=all_of(keys))
  obj_x$out  <- lapply(obj_x$out, function(x) as_tibble(x$by_x))
  obj_x <- obj_x |> tidyr::unnest("out")

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

fdm <- function(data, order = 6, lambda = NULL, ts_model_fn = fable::ARIMA) {
  type <- "mortality"

  if (is.null(lambda)) {
    if (type == "mortality") {
      lambda <- 0
    } else {
      stop("Not yet written")
    }
  }

  # Create rates matrix
  year <- sort(unique(data$Year))
  ages <- sort(unique(data$Age))
  mx <- data |>
    as_tibble() |>
    dplyr::select(Year, Age, Mortality) |>
    tidyr::pivot_wider(values_from = Mortality, names_from = Age)
  mx$Year <- NULL
  mx <- as.matrix(mx)
  # Transform if necessary
  if (lambda != 1) {
    mx <- BoxCox(mx, lambda)
    mx[mx < -1e9] <- NA
  }
  # PC decomposition
  y.pca <- fdpca(mx, order = order)

  # Compute fitted values
  fits <- as.data.frame(y.pca$basis %*% t(y.pca$coeff))
  colnames(fits) <- year
  fits <- fits |>
    dplyr::mutate(Age = ages) |>
    tidyr::pivot_longer(-Age, names_to = "Year", values_to = ".fitted") |>
    dplyr::mutate(Year = as.integer(Year))

  # Grab variable names
  index <- index_var(data)
  agevar <- attributes(data)$agevar
  # Add fitted values and residuals to original data
  output <- data |>
    as_tibble() |>
    dplyr::left_join(fits, by = c("Year", "Age")) |>
    dplyr::mutate(
      .fitted = exp(.fitted),
      .residual = Mortality - .fitted
    ) |>
    as_vital(index = sym(index), key = sym(agevar), .age = agevar)

  by_x <- as_tibble(y.pca$basis)
  by_x[[agevar]] <- sort(unique(data[[agevar]]))
  by_x <- by_x |>
    select(sym(agevar), everything())
  by_t <- as_tibble(y.pca$coeff)
  by_t[[index]] <- sort(unique(data[[index]]))
  by_t <- as_tsibble(by_t, index = sym(index)) |>
    select(sym(index), everything())

  # Fit ts_models to coefficients
  ts_coefs <- names(by_t)
  ts_coefs <- ts_coefs[grepl("beta", ts_coefs)]
  fits <- purrr::map(ts_coefs, function(x) {
    by_t |>
      fabletools::model(fit = ts_model_fn(!!sym(x)))
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


BoxCox <- function(x, lambda) {
  if (lambda < 0) {
    x[x < 0] <- NA
  }
  if (lambda == 0) {
    out <- log(x)
  } else {
    out <- (sign(x) * abs(x)^lambda - 1) / lambda
  }
  if (!is.null(colnames(x))) {
    colnames(out) <- colnames(x)
  }
  attr(out, "lambda") <- lambda
  return(out)
}

InvBoxCox <- function(x, lambda, biasadj = FALSE, fvar = NULL) {
  if (lambda < 0) {
    x[x > -1 / lambda] <- NA
  }
  if (lambda == 0) {
    out <- exp(x)
  } else {
    xx <- x * lambda + 1
    out <- sign(xx) * abs(xx)^(1 / lambda)
  }
  if (!is.null(colnames(x))) {
    colnames(out) <- colnames(x)
  }

  if (is.null(biasadj)) {
    biasadj <- attr(lambda, "biasadj")
  }
  if (!is.logical(biasadj)) {
    warning("biasadj information not found, defaulting to FALSE.")
    biasadj <- FALSE
  }
  if (biasadj) {
    if (is.null(fvar)) {
      stop("fvar must be provided when biasadj=TRUE")
    }
    if (is.list(fvar)) { # Create fvar from forecast interval
      level <- max(fvar$level)
      if (NCOL(fvar$upper) > 1 && NCOL(fvar$lower)) {
        i <- match(level, fvar$level)
        fvar$upper <- fvar$upper[, i]
        fvar$lower <- fvar$lower[, i]
      }
      if (level > 1) {
        level <- level / 100
      }
      level <- mean(c(level, 1))
      # Note: Use BoxCox transformed upper and lower values
      fvar <- as.numeric((fvar$upper - fvar$lower) / stats::qnorm(level) / 2)^2
    }
    if (NCOL(fvar) > 1) {
      fvar <- diag(fvar)
    }
    out <- out * (1 + 0.5 * as.numeric(fvar) * (1 - lambda) / (out)^(2 * lambda))
  }
  return(out)
}

utils::globalVariables(c(".model", "out", "object", ".fitted", ".rep"))
