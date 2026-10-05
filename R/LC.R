#' Lee-Carter model
#'
#' Lee-Carter model of mortality or fertility rates.
#' `LC()` returns a Lee-Carter model applied to the formula's response
#' variable as a function of age. This produces a standard Lee-Carter model by
#' default, although many other options are available. Missing rates are set to
#' the geometric mean rate for the relevant age.
#'
#' @aliases report.LC
#' @param formula Model specification. It should include the log of the variable to be modelled.
#' See the examples.
#' @param adjust method to use for adjustment of coefficients \eqn{k_t}.
#'   Possibilities are
#'   `"dt"` (Lee-Carter method),
#'   `"dxt"` (BMS method),
#'   `"e0"` (Lee-Miller method based on life expectancy) and
#'   `"none"`. If omitted, `"dt"` is used when the data contain deaths and
#'   population (see [vital_vars()]), and `"none"` otherwise, or when the data
#'   contain product-ratios from [make_pr()]. The `"dt"` and
#'   `"dxt"` methods require deaths and population.
#' @param jump_choice Method used for computation of jump-off point for forecasts.
#' Possibilities: `"actual"` (use actual rates from final year) and
#' `"fit"` (use fitted rates).
#' The original Lee-Carter method used `"fit"` (the default), but Lee and Miller (2001)
#' and most other authors prefer `"actual"`. With `"actual"`, fitted rates are
#' used (with a warning) for ages whose rate is zero or missing in the final year.
#' @param scale If TRUE, `bx` and `kt` are rescaled so that `kt` has drift parameter = -1 (i.e., `kt` decreases by 1 per year on average).
#' @param ... Not used. An error is given if any arguments are supplied here,
#' so that misspelled arguments are not silently ignored.
#'
#' @references Basellini, U, Camarda, C G, and Booth, H (2022) Thirty years on:
#' A review of the Lee-Carter method for forecasting mortality.
#' *International Journal of Forecasting*, 39(3), 1033-1049.
#' @references Booth, H., Maindonald, J., and Smith, L. (2002) Applying Lee-Carter
#' under conditions of variable mortality decline. *Population Studies*,
#' **56**, 325-336.
#' @references Lee, R D, and Carter, L R (1992) Modeling and forecasting US mortality.
#' *Journal of the American Statistical Association*, 87, 659-671.
#' @references Lee R D, and Miller T (2001). Evaluating the performance of the Lee-Carter
#' method for forecasting mortality. *Demography*, 38(4), 537–549.
#' @author Rob J Hyndman
#' @seealso [LC2()], [FDM()]
#' @return A model specification.
#'
#' @examples
#' lc <- norway_mortality |>
#'   dplyr::filter(Sex == "Female") |>
#'   model(lee_carter = LC(log(Mortality)))
#' report(lc)
#' autoplot(lc)
#' @export
LC <- function(
  formula,
  adjust = c("dt", "dxt", "e0", "none"),
  jump_choice = c("fit", "actual"),
  scale = FALSE,
  ...
) {
  rlang::check_dots_empty()
  # NULL means choose "dt" or "none" depending on the data
  adjust <- if (missing(adjust)) NULL else match.arg(adjust)
  jump_choice <- match.arg(jump_choice)
  lc_model <- new_model_class("lc", train = train_lc)
  new_model_definition(
    lc_model,
    !!enquo(formula),
    adjust = adjust,
    jump_choice = jump_choice,
    scale = scale
  )
}

train_lc <- function(
  .data,
  sex = NULL,
  specials,
  adjust,
  jump_choice,
  scale = FALSE,
  ...
) {
  # Variable names
  indexvar <- index_var(.data)
  vvar <- vital_var_list(.data)
  measures <- measured_vars(.data)
  measures <- measures[!(measures %in% c(vvar$age, vvar$population))]
  measures <- measures[1]

  # Compute Lee-Carter model
  out <- lca(
    .data,
    sex = sex,
    age = vvar$age,
    pop = vvar$population,
    deaths = vvar$deaths,
    rates = measures,
    adjust = adjust,
    scale = scale
  )

  # Save jump_choice for forecasting
  out$jump_choice <- jump_choice

  # Compute fitted values and residuals
  fits <- as_tibble(.data) |>
    left_join(out$by_t, by = indexvar) |>
    left_join(out$by_x, by = vvar$age) |>
    mutate(
      .fitted = ax + kt * bx,
      .innov = .data[[measures]] - .fitted,
      .innov = if_else(.innov < -1e20, NA, .innov),
    ) |>
    select(all_of(c(indexvar, vvar$age, ".fitted", ".innov")))

  # Jump-off adjustments: the final residuals for "actual", otherwise none
  last <- fits[fits[[indexvar]] == max(fits[[indexvar]]), ]
  jump <- if (jump_choice == "actual") last$.innov else rep(0, NROW(last))
  if (anyNA(jump)) {
    warning(
      "Rates are zero or missing in the final year for ages ",
      paste(last[[vvar$age]][is.na(jump)], collapse = ", "),
      ". Using fitted rates as the jump-off for these ages.",
      call. = FALSE
    )
    jump[is.na(jump)] <- 0
  }
  out$jump <- tibble(!!vvar$age := last[[vvar$age]], .jump = jump)

  structure(
    list(
      model = out,
      fitted = fits,
      nobs = sum(!is.na(.data[[measures]]))
    ),
    class = "LC"
  )
}

#' @rdname forecast
#' @export

forecast.LC <- function(
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
  h <- length(unique(new_data[[index_var(new_data)]]))
  agevar <- colnames(object$model$by_x)[1]
  indexvar <- index_var(object$model$by_t)

  # Time series estimation of kt as Random walk with drift
  fc <- object$model$fit_kt |>
    forecast(h = h)

  # Create forecasts of response series, adjusted to the jump-off rates
  new_data |>
    left_join(object$model$by_x, by = agevar) |>
    left_join(fc, by = indexvar) |>
    left_join(object$model$jump, by = agevar) |>
    transmute(fc = ax + bx * kt + .jump) |>
    pull(fc)
}

#' @export
generate.LC <- function(
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

  # Forecast kt series using random walk with drift term
  h <- length(unique(new_data[[index_var(new_data)]]))
  fc <- x$model$fit_kt |>
    generate(h = h, bootstrap = bootstrap, times = times)
  new_data <- new_data |>
    left_join(x$model$by_x, by = agevar) |>
    left_join(fc, by = c(indexvar, ".rep")) |>
    left_join(x$model$jump, by = agevar) |>
    mutate(fitted = ax + bx * .sim + .jump)

  transmute(new_data, .sim = fitted)
}

#' @export
glance.LC <- function(x, ...) {
  tibble(
    varprop = x$model$varprop,
    base_deviance = x$model$mdev[1],
    total_deviance = x$model$mdev[2]
  )
}

#' @export
tidy.LC <- function(x, ...) {
  tidy_coefficients(x$model$by_x, x$model$by_t)
}

#' @export
report.LC <- function(object, ...) {
  cat("\nOptions:")
  cat("\n  Adjust method: ")
  cat(object$model$adjust)
  cat("\n  Jump choice: ")
  cat(object$model$jump_choice)
  cat("\n\nAge functions\n")
  print(object$model$by_x, n = 5)
  cat("\nTime coefficients\n")
  print(object$model$by_t, n = 5)
  cat("\nTime series model: ")
  cat(model_sum(object$model$fit_kt$rw[[1]]$fit), "\n")
  cat("\nVariance explained: ")
  cat(paste0(round(object$model$varprop * 100, 2), "%\n"))
}

#' @export
model_sum.LC <- function(x) {
  paste0("LC")
}

# Based on demography::lca()
# But assumes any log transformation has already occurred

lca <- function(
  data,
  sex,
  age,
  rates,
  pop,
  deaths,
  adjust,
  scale
) {
  index <- tsibble::index_var(data)

  # Choose the adjustment method
  counts_available <- !is.null(deaths) && !is.null(pop)
  if (is.null(adjust)) {
    adjust <- if (counts_available) "dt" else "none"
  } else if (adjust %in% c("dt", "dxt") && !counts_available) {
    stop(
      "adjust = \"", adjust, "\" requires deaths and population variables. ",
      "Use adjust = \"none\" or \"e0\" instead."
    )
  }

  # Check transformation
  if (substr(rates, 1, 3) != "log") {
    stop(
      "Lee-Carter models require a log transformation of the response variable."
    )
  }

  # Extract mortality rates and population numbers
  year <- sort(unique(data[[index]]))
  ages <- sort(unique(data[[age]]))
  n <- length(ages)
  m <- length(year)
  check_complete_grid(data, age, index)
  # The matrices below are filled by age and then year
  data <- data[order(data[[age]], data[[index]]), ]

  logrates <- t(matrix(data[[rates]], nrow = n, ncol = m, byrow = TRUE))
  logrates[logrates == -Inf] <- NA

  if (!is.null(pop)) {
    pop <- t(matrix(data[[pop]], nrow = n, ncol = m, byrow = TRUE))
    pop[is.na(pop)] <- 0
  }
  if (!is.null(deaths)) {
    deaths <- t(matrix(data[[deaths]], nrow = n, ncol = m, byrow = TRUE))
    deaths[is.na(deaths)] <- 0
  }

  # Do SVD
  ax <- colMeans(logrates, na.rm = TRUE) # ax is mean of logrates by column
  if (any(ax < -1e9) || anyNA(ax)) {
    # Estimate troublesome values with interpolation
    ax[ax < -1e9] <- NA
    ax <- stats::approx(seq_along(ax), ax, xout = seq_along(ax), rule = 2)$y
  }
  clogrates <- sweep(logrates, 2, ax) # central log rates (with ax subtracted) (dimensions m*n)
  # Set missing central rates to 0 (effectively setting mx to ax)
  clogrates[is.na(clogrates)] <- 0
  # Take SVD
  svd.mx <- svd(clogrates)

  # Extract first principal component
  sumv <- sum(svd.mx$v[, 1])
  bx <- svd.mx$v[, 1] / sumv
  kt <- svd.mx$d[1] * svd.mx$u[, 1] * sumv

  # Adjust kt to match deaths or life expectancy
  ktadj <- kt

  # Use regression to guess suitable range for root finding method
  ktse <- stats::predict(stats::lm(kt ~ seq(m)), se.fit = TRUE)$se.fit
  ktse[is.na(ktse)] <- 1

  if (adjust == "dxt") {
    # Fit to age-specific deaths.
    # Offset
    z <- log(t(pop)) + ax
    for (i in seq(m)) {
      y <- as.numeric(deaths[i, ])
      zi <- as.numeric(z[, i])
      weight <- as.numeric(is.finite(zi)) # Avoid -infinity due to zero population
      # Prevent warnings if population is non-integer
      yearglm <- stats::glm(
        y ~ offset(zi) - 1 + bx,
        family = stats::poisson,
        weights = weight
      ) |>
        suppressWarnings()
      ktadj[i] <- yearglm$coefficients[1]
    }
  } else if (adjust == "dt") {
    # Fit to total deaths
    FUN <- function(p, Dt, bx, ax, popi) {
      Dt - sum(exp(p * bx + ax) * popi)
    }
    for (i in seq(m)) {
      sum_deaths <- sum(as.numeric(deaths[i, ]))
      if (sum_deaths > 0) {
        if (i == 1) {
          guess <- kt[1]
        } else {
          guess <- mean(c(ktadj[i - 1], kt[i]))
        }
        ktadj[i] <- findroot(
          FUN,
          guess = guess,
          margin = 10 * ktse[i],
          ax = ax,
          bx = bx,
          popi = pop[i, ],
          Dt = sum_deaths
        )
      }
    }
  } else if (adjust == "e0") {
    # Fit to life expectancy
    mx <- exp(logrates)
    e0 <- apply(mx, 1, get.e0, agegroup = ages, sex = sex)
    FUN2 <- function(p, e0i, ax, bx, ages, sex) {
      e0i - estimate_e0(p, ax, bx, ages, sex)
    }
    for (i in seq(m)) {
      if (!is.na(e0[i])) {
        if (i == 1) {
          guess <- kt[1]
        } else {
          guess <- mean(c(ktadj[i - 1], kt[i]))
        }
        ktadj[i] <- findroot(
          FUN2,
          guess = guess,
          margin = 10 * ktse[i],
          e0i = e0[i],
          ax = ax,
          bx = bx,
          ages = ages,
          sex = sex
        )
      }
    }
  }

  kt <- ktadj

  # Rescaling bx, kt
  if (scale) {
    avdiffk <- -mean(diff(kt))
    bx <- bx * avdiffk
    kt <- kt / avdiffk
  }

  # Compute deviances (which need more than two years for their degrees of freedom)
  mdev <- c(NA_real_, NA_real_)
  if (counts_available && m > 2) {
    logfit <- fitmx(kt, ax, bx, transform = TRUE)
    deathsadjfit <- exp(logfit) * pop
    drift <- mean(diff(kt))
    ktlinfit <- mean(kt) + drift * (1:m - (m + 1) / 2)
    deathslinfit <- fitmx(ktlinfit, ax, bx, transform = FALSE) * pop
    # Drop zero deaths from mdev calculation
    d_nozero <- deaths
    d_nozero[deaths == 0] <- 0.000001
    # Cells with no population have no expected deaths, so are omitted
    exposed <- pop > 0
    deviance <- function(fit) {
      sum((deaths * log(d_nozero / fit) - (deaths - fit))[exposed])
    }
    mdev[1] <- 2 / ((m - 2) * (n - 1)) * deviance(deathsadjfit)
    mdev[2] <- 2 / ((m - 2) * n) * deviance(deathslinfit)
  }
  names(mdev) <- c("Mean deviance base", "Mean deviance total")

  # First object contains ages
  output1 <- tibble::tibble(
    age = ages,
    ax = ax,
    bx = bx
  )
  colnames(output1)[1] <- age

  # Second object contains years
  output2 <- tibble::tibble(
    year = year,
    kt = kt
  )
  colnames(output2)[1] <- index
  output2 <- as_tsibble(output2, index = index)

  # Fit model to kt series
  fit_kt <- output2 |>
    fabletools::model(rw = fable::RW(kt ~ drift()))

  # Return
  list(
    by_x = output1,
    by_t = output2,
    fit_kt = fit_kt,
    varprop = svd.mx$d[1]^2 / sum(svd.mx$d^2),
    mdev = mdev,
    adjust = adjust
  )
}

estimate_e0 <- function(kt, ax, bx, agegroup, sex) {
  if (length(kt) > 1) {
    stop("Length of kt greater than 1")
  }
  mx <- c(fitmx(kt, ax, bx))
  return(get.e0(mx, agegroup, sex))
}

# Compute expected age from single year mortality rates
# x contains vector of mortality rates
# agegroup is vector of ages
# sex is a string
get.e0 <- function(x, agegroup, sex) {
  lt(
    tibble::tibble(age = agegroup, sex = sex, mx = x),
    "sex",
    "age",
    "mx"
  )$ex[1]
}


fitmx <- function(kt, ax, bx, transform = FALSE) {
  # Derives mortality rates from kt mortality index,
  # per Lee-Carter method
  clogratesfit <- outer(kt, bx)
  logratesfit <- sweep(clogratesfit, 2, ax, "+")
  if (transform) {
    return(logratesfit)
  } else {
    return(exp(logratesfit))
  }
}

findroot <- function(FUN, guess, margin, attempt = 1, ...) {
  # First try in successively larger intervals around best guess
  for (i in 1:5) {
    rooti <- try(
      stats::uniroot(FUN, interval = guess + i * margin / 3 * c(-1, 1), ...),
      silent = TRUE
    )
    if (!(inherits(rooti, "try-error"))) {
      return(rooti$root)
    }
  }
  # No luck. Try really big intervals
  rooti <- try(
    stats::uniroot(FUN, interval = guess + 10 * margin * c(-1, 1), ...),
    silent = TRUE
  )
  if (!(inherits(rooti, "try-error"))) {
    return(rooti$root)
  }

  # Still no luck. Try guessing root using quadratic approximation
  if (attempt < 3) {
    root <- try(quadroot(FUN, guess, 10 * margin, ...), silent = TRUE)
    if (!(inherits(root, "try-error"))) {
      return(findroot(FUN, root, margin, attempt + 1, ...))
    }
    root <- try(quadroot(FUN, guess, 20 * margin, ...), silent = TRUE)
    if (!(inherits(root, "try-error"))) {
      return(findroot(FUN, root, margin, attempt + 1, ...))
    }
  }

  # Finally try optimization
  root <- try(newroot(FUN, guess, ...), silent = TRUE)
  if (!(inherits(root, "try-error"))) {
    return(root)
  } else {
    root <- try(newroot(FUN, guess - margin, ...), silent = TRUE)
  }
  if (!(inherits(root, "try-error"))) {
    return(root)
  } else {
    root <- try(newroot(FUN, guess + margin, ...), silent = TRUE)
  }
  if (!(inherits(root, "try-error"))) {
    return(root)
  } else {
    stop("Unable to find root")
  }
}

quadroot <- function(FUN, guess, margin, ...) {
  x1 <- guess - margin
  x2 <- guess + margin
  y1 <- FUN(x1, ...)
  y2 <- FUN(x2, ...)
  y0 <- FUN(guess, ...)
  if (is.na(y1) || is.na(y2) || is.na(y0)) {
    stop("Function not defined on interval")
  }
  b <- 0.5 * (y2 - y1) / margin
  a <- (0.5 * (y1 + y2) - y0) / (margin^2)
  tmp <- b^2 - 4 * a * y0
  if (tmp < 0) {
    stop("No real root")
  }
  tmp <- sqrt(tmp)
  r1 <- 0.5 * (tmp - b) / a
  r2 <- 0.5 * (-tmp - b) / a
  if (abs(r1) < abs(r2)) {
    root <- guess + r1
  } else {
    root <- guess + r2
  }
  return(root)
}

# Try finding root using minimization
newroot <- function(FUN, guess, ...) {
  fred <- function(x, ...) {
    (FUN(x, ...)^2)
  }
  junk <- stats::nlm(fred, guess, ...)
  if (abs(junk$minimum) / fred(guess, ...) > 1e-6) {
    warning("No root exists. Returning closest")
  }
  return(junk$estimate)
}

#' @export
time_components.LC <- function(object, ...) {
  index <- index_var(first_fit(object)$fit$model$by_t)
  unnest_time_components(object, function(x) as_tibble(x$by_t), index)
}

#' @export
age_components.LC <- function(object, ...) {
  unnest_components(object, function(x) as_tibble(x$by_x))
}

#' @export
autoplot.LC <- function(object, ...) {
  obj_time <- time_components(object)
  obj_x <- age_components(object)
  index <- index_var(obj_time)
  keys <- colnames(obj_time)
  keys <- keys[!(keys %in% c(index, "kt"))]
  agevar <- colnames(first_fit(object)$fit$model$by_x)[1]

  # Set up list of plots
  p <- list()
  p[[1]] <- key_plot(obj_x, sym(agevar), "ax", keys) + ggplot2::ylab("ax")
  p[[2]] <- key_plot(obj_x, sym(agevar), "bx", keys) + ggplot2::ylab("bx")
  p[[3]] <- patchwork::guide_area()
  p[[4]] <- time_plot(obj_time, "kt", keys) + ggplot2::labs(x = index, y = "kt")
  patchwork::wrap_plots(p) +
    patchwork::plot_layout(ncol = 2, nrow = 2, guides = "collect")
}
