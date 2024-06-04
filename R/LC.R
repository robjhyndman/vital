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
#'   `"dt"` (Lee-Carter method, the default),
#'   `"dxt"` (BMS method),
#'   `"e0"` (Lee-Miller method based on life expectancy) and
#'   `"none"`.
#' @param jump_choice Method used for computation of jump-off point for forecasts.
#' Possibilities: `"actual"` (use actual rates from final year) and
#' `"fit"` (use fitted rates).
#' The original Lee-Carter method used `"fit"` (the default), but Lee and Miller (2001)
#' and most other authors prefer `"actual"`.
#' @param scale If TRUE, `bx` and `kt` are rescaled so that `kt` has drift parameter = 1.
#' @param ... Not used.
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
#' @return A model specification.
#'
#' @examples
#' lc <- aus_mortality |>
#'   dplyr::filter(State == "Victoria", Sex == "female") |>
#'   model(lee_carter = LC(log(Mortality)))
#' report(lc)
#' autoplot(lc)
#' @export
LC <- function(formula, adjust = c("dt", "dxt", "e0", "none"),
               jump_choice = c("fit", "actual"), scale = FALSE,
               ...) {
  adjust <- match.arg(adjust)
  jump_choice <- match.arg(jump_choice)
  lc_model <- new_model_class("lc", train = train_lc)
  new_model_definition(lc_model, !!enquo(formula), adjust = adjust,
                       jump_choice = jump_choice, scale = scale, ...)
}

train_lc <- function(.data, sex = NULL, specials,  adjust,
                     jump_choice, scale = FALSE, ...) {
  # Variable names
  attrx <- attributes(.data)
  indexvar <- index_var(.data)
  agevar <- attrx$agevar
  measures <- measured_vars(.data)
  measures <- measures[!(measures %in% c(agevar, attrx$populationvar))]
  measures <- measures[1]

  # Compute Lee-Carter model
  out <- lca(.data, sex=sex, age=attrx$agevar, pop = attrx$populationvar,
             deaths = attrx$deathsvar,
             rates = colnames(.data)[2],
             adjust = adjust, jump_choice = jump_choice, scale = scale)

  # Save jump_choice for forecasting
  out$jump_choice <- jump_choice

  # Compute fitted values and residuals
  fits <- as_tibble(.data) |>
    left_join(out$by_t, by = indexvar) |>
    left_join(out$by_x, by = agevar) |>
    mutate(
      .fitted = ax + kt*bx,
      .innov = .data[[measures]] - .fitted,
      .innov = if_else(.innov < -1e20, NA, .innov),
    ) |>
    select(all_of(c(indexvar, agevar, ".fitted", ".innov")))

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

forecast.LC <- function(object, new_data = NULL, h = NULL, point_forecast = list(.mean = mean),
  simulate = FALSE, bootstrap = FALSE, times = 5000, ...) {
  jump_choice <- object$model$jump_choice

# simulation/bootstrap not actually used here as forecast.mdl_vtl_ts
# handles this using generate() and forecast.LC is never called.
# The arguments are included to avoid a warning message, and because this is how it
# appears to work to the user.

  h <- length(unique(new_data[[index_var(new_data)]]))
  agevar <- colnames(object$model$by_x)[1]
  indexvar <- index_var(object$model$by_t)

  # Time series estimation of kt as Random walk with drift
  fc <- object$model$fit_kt |>
    forecast(h = h)

  # Create forecasts of response series
  fc2 <- new_data |>
    left_join(object$model$by_x, by = agevar) |>
    left_join(fc, by = indexvar) |>
    transmute(fc = ax + bx * kt)

  if(jump_choice == "actual") {
    # Adjust forecasts based on last year
    lastresid <- object$fitted[object$fitted[[indexvar]] == max(object$fitted[[indexvar]]),] |>
      dplyr::select(all_of(c(agevar, ".innov")))
    fc2 <- fc2 |>
      left_join(lastresid, by=agevar) |>
      mutate(fc = fc + .innov)
  }

  fc2 |> pull(fc)
}

#' @export
generate.LC <- function(x, new_data = NULL, h = NULL,
  bootstrap = FALSE, times = 1, ...) {
  agevar <- attributes(new_data)$agevar
  indexvar <- index_var(new_data)
  if(times != length(unique(new_data$.rep)))
    stop("We have a problem")

  # Forecast kt series using random walk with drift term
  h <- length(unique(new_data[[index_var(new_data)]]))
  fc <- x$model$fit_kt |>
    generate(h = h, bootstrap = bootstrap, times = times)
  new_data <- new_data |>
    left_join(x$model$by_x, by = agevar) |>
    left_join(fc, by = c(indexvar, ".rep")) |>
    mutate(fitted = exp(ax + bx*.sim))

  transmute(group_by_key(new_data), ".sim" := fitted)
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
  return(NULL)
}

#' @export
report.LC <- function(object, ...) {
  cat("\nOptions:")
  cat("\n  Adjust method: ")
  cat(object$model$adjust)
  cat("\n  Jump choice: ")
  cat(object$model$jump_choice)
  cat("\n\nAge functions\n")
  print(object$model$by_x, n=5)
  cat("\nTime coefficients\n")
  print(object$model$by_t, n=5)
  cat("\nTime series model: ")
  cat(model_sum(object$model$fit_kt$rw[[1]]$fit), "\n")
  cat("\nVariance explained: ")
  cat(paste0(round(object$model$varprop*100, 2),"%\n"))
}

#' @export
model_sum.LC <- function(x) {
  paste0("LC")
}

# @examples
# # Compute Lee-Carter model for Victorian females, males and total
# aus_lc <- aus_mortality |>
#   dplyr::filter(Code == "VIC") |>
#   lee_carter()
# aus_lc
# autoplot(aus_lc) +
#   patchwork::plot_annotation("Lee Carter components for Victoria")
# autoplot(aus_lc$time, kt)


# Based on demography::lca()
# But assumes any log transformation has already occurred

lca <- function(data, sex, age, rates, pop, deaths,
                adjust, jump_choice, scale) {
  index <- tsibble::index_var(data)

  # Check transformation
  if(substr(rates,1,3) != "log")
    stop("Lee-Carter models require a log transformation of the response variable.")

  # Extract mortality rates and population numbers
  year <- sort(unique(data[[index]]))
  deltat <- year[2] - year[1]
  ages <- sort(unique(data[[age]]))
  n <- length(ages)
  m <- length(year)

  logrates <- t(matrix(data[[rates]], nrow = n, ncol = m, byrow=TRUE))
  logrates[logrates == -Inf] <- NA
  logrates[is.na(logrates)] <- 0

  if(!is.null(pop)) {
    pop <- t(matrix(data[[pop]], nrow = n, ncol = m, byrow=TRUE))
    pop[is.na(pop)] <- 0
  }
  if(!is.null(deaths)) {
    deaths <- t(matrix(data[[deaths]], nrow = n, ncol = m, byrow=TRUE))
    deaths[is.na(deaths)] <- 0
  }


  # Do SVD
  ax <- apply(logrates, 2, mean, na.rm = TRUE) # ax is mean of logrates by column
  if (any(ax < -1e9) | any(is.na(ax))) {
    # Estimate troublesome values with interpolation
    ax[ax < -1e9] <- NA
    ax <- stats::approx(seq_along(ax), ax, xout=seq_along(ax))$y
  }
  clogrates <- sweep(logrates, 2, ax) # central log rates (with ax subtracted) (dimensions m*n)
  # Set missing central rates to 0 (effectively setting mx to ax)
  clogrates[clogrates == -Inf] <- NA
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
      weight <- as.numeric(zi > -1e-8) # Avoid -infinity due to zero population
      # Prevent warnings if population is non-integer
      yearglm <- stats::glm(y ~ offset(zi) - 1 + bx, family = stats::poisson, weights = weight) |>
        suppressWarnings()
      ktadj[i] <- yearglm$coef[1]
    }
  } else if (adjust == "dt") {
    # Fit to total deaths
    FUN <- function(p, Dt, bx, ax, popi) {
      Dt - sum(exp(p * bx + ax) * popi)
    }
    for (i in seq(m)) {
      sum_deaths <- sum(as.numeric(deaths[i, ]))
      if(sum_deaths > 0) {
        if (i == 1) {
          guess <- kt[1]
        } else {
          guess <- mean(c(ktadj[i - 1], kt[i]))
        }
        ktadj[i] <- findroot(FUN, guess = guess, margin = 10 * ktse[i],
                             ax = ax, bx = bx, popi = pop[i, ], Dt = sum_deaths)
      }
    }
  } else if (adjust == "e0") {
    # Fit to life expectancy
    #stop("Not yet working")
    startage <- min(data[[age]])
    agegroup <- ages[4] - ages[3]
    mx <- exp(logrates)
    e0 <- apply(mx, 1, get.e0, agegroup = ages, sex = sex, startage = startage)
    FUN2 <- function(p, e0i, ax, bx, ages, sex, startage) {
      e0i - estimate_e0(p, ax, bx, ages, sex, startage)
    }
    for (i in seq(m)) {
      if(!is.na(e0[i])) {
        if (i == 1) {
          guess <- kt[1]
        } else {
          guess <- mean(c(ktadj[i - 1], kt[i]))
        }
        ktadj[i] <- findroot(FUN2, guess = guess, margin = 10 * ktse[i], e0i = e0[i],
          ax = ax, bx = bx, ages = ages, sex = sex, startage = startage)
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



  # Compute deviances
  logfit <- fitmx(kt, ax, bx, transform = TRUE)
  deathsadjfit <- exp(logfit) * pop
  drift <- mean(diff(kt))
  ktlinfit <- mean(kt) + drift * (1:m - (m + 1) / 2)
  deathslinfit <- fitmx(ktlinfit, ax, bx, transform = FALSE) * pop
  dflogadd <- (m - 2) * (n - 1)
  # Drop zero deaths from mdev calculation
  d_nozero <- deaths
  d_nozero[deaths == 0] <- .000001
  mdevlogadd <- 2 / dflogadd * sum(deaths * log(d_nozero / deathsadjfit) - (deaths - deathsadjfit))
  dfloglin <- (m - 2) * n
  mdevloglin <- 2 / dfloglin * sum(deaths * log(d_nozero / deathslinfit) - (deaths - deathslinfit))
  mdev <- c(mdevlogadd, mdevloglin)
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
  output2 <- as_tsibble(output2, index=index)


  # Fit model to kt series
  fit_kt <- output2 |>
    fabletools::model(rw = fable::RW(kt ~ drift()))

  # Return
  list(
    by_x = output1, by_t = output2,
    fit_kt = fit_kt,
    varprop = svd.mx$d[1]^2 / sum(svd.mx$d^2), mdev = mdev,
    adjust = adjust
  )
}

estimate_e0 <- function(kt, ax, bx, agegroup, sex, startage = 0) {
  if (length(kt) > 1) {
    stop("Length of kt greater than 1")
  }
  mx <- c(fitmx(kt, ax, bx))
  return(get.e0(mx, agegroup, sex, startage = startage))
}

# Compute expected age from single year mortality rates
# x contains vector of mortality rates
# agegroup is vector of ages
# sex is a string
get.e0 <- function(x, agegroup, sex, startage = 0) {
  lt(
    tibble::tibble(age = agegroup, sex = sex, mx = x),
    "sex", "age", "mx"
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

findroot <- function(FUN, guess, margin, try = 1, ...) {
  # First try in successively larger intervals around best guess
  for (i in 1:5)
  {
    rooti <- try(stats::uniroot(FUN, interval = guess + i * margin / 3 * c(-1, 1), ...), silent = TRUE)
    if (!("try-error" %in% class(rooti))) {
      return(rooti$root)
    }
  }
  # No luck. Try really big intervals
  rooti <- try(stats::uniroot(FUN, interval = guess + 10 * margin * c(-1, 1), ...), silent = TRUE)
  if (!("try-error" %in% class(rooti))) {
    return(rooti$root)
  }

  # Still no luck. Try guessing root using quadratic approximation
  if (try < 3) {
    root <- try(quadroot(FUN, guess, 10 * margin, ...), silent = TRUE)
    if (!("try-error" %in% class(root))) {
      return(findroot(FUN, root, margin, try + 1, ...))
    }
    root <- try(quadroot(FUN, guess, 20 * margin, ...), silent = TRUE)
    if (!("try-error" %in% class(root))) {
      return(findroot(FUN, root, margin, try + 1, ...))
    }
  }

  # Finally try optimization
  root <- try(newroot(FUN, guess, ...), silent = TRUE)
  if (!("try-error" %in% class(root))) {
    return(root)
  } else {
    root <- try(newroot(FUN, guess - margin, ...), silent = TRUE)
  }
  if (!("try-error" %in% class(root))) {
    return(root)
  } else {
    root <- try(newroot(FUN, guess + margin, ...), silent = TRUE)
  }
  if (!("try-error" %in% class(root))) {
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
  if (is.na(y1) | is.na(y2) | is.na(y0)) {
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
  modelname <- attributes(object)$model
  object <- object |>
    mutate(out = purrr::map(object[[modelname]], function(x){x$fit$model})) |>
    as_tibble()
  object[[modelname]] <- NULL
  index <- index_var(object$out[[1]]$by_t)
  keys <- head(colnames(object), -1)
  # Construct time and age data frames
  obj_time <- object
  obj_time$out <- lapply(obj_time$out, function(x) as_tibble(x$by_t))
  obj_time <- obj_time |>
    tidyr::unnest("out") |>
    as_tsibble(index = index, key=all_of(keys))
}

#' @export
age_components.LC <- function(object, ...) {
  modelname <- attributes(object)$model
  object <- object |>
    mutate(out = purrr::map(object[[modelname]], function(x){x$fit$model})) |>
    as_tibble()
  # Construct time and age data frames
  obj_x <- object
  obj_x$out  <- lapply(obj_x$out, function(x) as_tibble(x$by_x))
  obj_x <- obj_x |> tidyr::unnest("out")
  obj_x[[modelname]] <- NULL
  return(obj_x)
}

#' @export
autoplot.LC <- function(object, age = "Age", ...) {
  obj_time <- time_components(object)
  obj_x <- age_components(object)
  index <- index_var(obj_time)
  keys <- colnames(obj_time)
  keys <- keys[!(keys %in% c(index, "kt"))]

    # Set up list of plots
  p <- list()
  p[[1]] <- age_plot(obj_x, "ax", keys) + ggplot2::ylab("ax")
  p[[2]] <- age_plot(obj_x, "bx", keys) + ggplot2::ylab("bx")
  p[[3]] <- patchwork::guide_area()
  p[[4]] <- time_plot(obj_time, "kt") + ggplot2::labs(x = index, y = "kt")
  patchwork::wrap_plots(p) +
    patchwork::plot_layout(ncol = 2, nrow = 2, guides = "collect")
}

utils::globalVariables(c("kt", "ax", "bx", "varprop", "lst_data", "by_x", "by_t"))
