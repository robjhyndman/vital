#' Fit a functional principal components model
#'
#' Model mortality or fertility data using functional principal components
#' as proposed by Hyndman & Ullah (2007)
#' Missing rates are set to the geometric mean rate for the relevant age.
#'
#' @param .data A vital object including an age variable and a variable containing mortality or fertility rates.
#' @param rates Variable in `.data` containing mortality or fertility rates.
#' If omitted, it will search for a variable with one of the following names:
#' `mx`, `mortality`, `fx`, `fertility` or `rate` (not case sensitive).
#' @param order Number of principal components to fit.
#'
#' @return A list of class \code{fpc_model} containing various model objects.
#'
#' @references Hyndman, R.J., and Ullah, S. (2007) Robust forecasting of
#' mortality and fertility rates: a functional data approach.
#' \emph{Computational Statistics & Data Analysis}, \bold{51}, 4942-4956.
#' \url{https://robjhyndman.com/publications/funcfor/}
#' @author Rob J Hyndman.
#'
#' @keywords models
#' @examples
#' # Compute Hyndman-Ullah model for Australian females, males and total
#' aus_hu <- aus_mortality |>
#'   dplyr::filter(Code == "AUS") |>
#'   fpc_model()
#' aus_hu
#' autoplot(aus_hu) +
#'   patchwork::plot_annotation("Hyndman-Ullah components for Australia")
#' @export

fpc_model <- function(.data, rates, order = 6) {
  if(!inherits(.data, "vital"))
    stop(".data should be a vital object")
  agevar <- attributes(.data)$agevar
  if(is.null(agevar)) {
    stop("No age variable found")
  }

  # Index variable
  index <- tsibble::index_var(.data)
  # Keys including age
  keys <- tsibble::key_vars(.data)

  sex <- attributes(.data)$sexvar
  pop <- attributes(.data)$populationvar

  if (!missing(rates)) {
    rates <- {{ rates }}
  } else {
    rates <- find_measure(.data, c("mx", "mortality", "fx", "fertility", "rate"))
  }

  # Drop age as a key and nest results
  keys_noage <- keys[keys != agevar]
  nested_data <- tidyr::nest(.data, lst_data = c(-!!keys_noage))

  # Fit fdm to each functional time series
  out <- purrr::map2(nested_data[["lst_data"]], order, fdm)

  # Extract fitting information
  fit <- dplyr::select(nested_data, -lst_data) |>
    mutate(varprop = unlist(lapply(out, function(x) {sum(x$varprop)})))
  # Extract fitted values and residuals
  fits <- dplyr::select(nested_data, -lst_data) |>
    dplyr::mutate(fits = lapply(out, function(x) {as_tibble(x$data)})) |>
    tidyr::unnest(fits) |>
    tsibble::as_tsibble(index = index, key = keys) |>
    dplyr::select(Year, everything())
  # Extract time series
  year <- sort(unique(.data[[index]]))
  by_t <- lapply(out, function(x) {dplyr::mutate(x$coeff, Year = year)})
  time <- dplyr::select(nested_data, -lst_data) |>
    dplyr::mutate(by_t = by_t) |>
    tidyr::unnest(by_t) |>
    tsibble::as_tsibble(index = index, key = keys_noage) |>
    dplyr::select(Year, everything())
  # Extract basis functions
  ages <- sort(unique(.data[[agevar]]))
  by_x <- lapply(out, function(x) {dplyr::mutate(x$basis, Age = ages)})
  age <- dplyr::select(nested_data, -lst_data) |>
    dplyr::mutate(by_x = by_x) |>
    tidyr::unnest(by_x) |>
    dplyr::select(Age, everything())

  # Combine results
  final <- list(
    fit = fit,
    data = fits,
    time = time,
    age = age,
    call = match.call(),
    agevar = agevar,
    timevar = index
  )
  return(structure(final, class = "fpc_model"))
}

# Function based on demography::fdm() and ftsa::ftsm()

fdm <- function(data, order = 6, lambda = NULL) {
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

  # Add fitted values and residuals to original data
  output <- data |>
    dplyr::left_join(fits, by = c("Year", "Age")) |>
    dplyr::mutate(
      .fitted = exp(.fitted),
      .residual = Mortality - .fitted
    )

  # Return object
  list(
    data = output,
    coeff = as_tibble(y.pca$coeff),
    basis = as_tibble(y.pca$basis),
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

#' @export
print.fpc_model <- function(x, ...) {
  if(inherits(x, "lc_model")) {
    cat("Lee-Carter model\n")
  } else
  {
    cat("Functional data model\n")
  }
  cat("\nFit:\n")
  print(x$fit)
}

#' Plot of functional data model components
#'
#' Plot of mean, basis functions and PC scores from a functional data model
#'
#' @param object output from \code{\link{fpc_model}}.
#' @param show_order The maximum number of basis functions to plot
#' @param ... Other arguments not used.
#' @return A patchwork object
#' @export
autoplot.fpc_model <- function(object, show_order = 2, ...) {
  # Check if Lee-Carter model
  lc <- inherits(object, "lc_model")
  if (lc) {
    meanvar <- "ax"
    timevar <- "kt"
    agevar <- "bx"
  } else {
    meanvar <- "mean"
    tmp <- colnames(object$time)
    timevar <- tmp[grepl("beta", tmp)]
    tmp <- colnames(object$age)
    agevar <- tmp[grepl("phi", tmp)]
  }
  # Number of basis functions to plot
  show_order <- min(show_order, length(timevar))
  # Keys
  keys <- tsibble::key_vars(object$time)
  # Set up list of plots
  p <- list()
  p[[1]] <- age_plot(object$age, get(meanvar), keys) +
    ggplot2::ylab(meanvar)
  for (i in seq(show_order)) {
    p[[i + 1]] <- age_plot(object$age, get(agevar[i]), keys) +
      ggplot2::ylab(agevar[i])
  }
  p[[show_order + 2]] <- patchwork::guide_area()
  for (i in seq(show_order)) {
    p[[i + 2 + show_order]] <- fabletools::autoplot(object$time, get(timevar[i])) +
      ggplot2::xlab(tsibble::index_var(object$time)) +
      ggplot2::ylab(timevar[i])
  }
  patchwork::wrap_plots(p) +
    patchwork::plot_layout(ncol = show_order + 1, nrow = 2, guides = "collect")
}


# Plot a variable against age by key
age_plot <- function(object, .var, keys) {
  # Convert age to time and use fabletools::autoplot.tbl_ts
  names <- colnames(object)[!(colnames(object) %in% c(keys, deparse(substitute(.var))))]
  age <- names[grep("age", names, ignore.case=TRUE)]
  object_ts <- tsibble::as_tsibble(object, index=age, key = keys[keys != age])
  fabletools::autoplot(object_ts, {{ .var }}) + ggplot2::xlab(age)
}

globalVariables(c(".fitted"))
