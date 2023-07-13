#' Model mortality or fertility data using Lee-Carter approach
#'
#' Lee-Carter model of mortality or fertility rates. \code{lee_carter} produces a
#' standard Lee-Carter model by default, although many other options are
#' available. Missing rates are set to the geometric mean rate for the relevant age.
#' \code{LC()} returns a Lee-Carter model applied to the formula's response variable as a function of age.
#'
#' @aliases report.model_lc
#' @param adjust method to use for adjustment of coefficients \eqn{k_t kt}.
#'   Possibilities are
#'   \dQuote{dt} (Lee-Carter method, the default),
#'   \dQuote{dxt} (BMS method),
#'   \dQuote{e0} (Lee-Miller method based on life expectancy) and
#'   \dQuote{none}.
#' @param scale If TRUE, bx and kt are rescaled so that kt has drift parameter = 1.
#'
#' @param formula Model specification.
#' @param ... Not used.
#'
#' @references Basellini, U, Camarda, C G, and Booth, H (2022) Thirty years on:
#' A review of the Lee-Carter method for forecasting mortality.
#' @references Booth, H., Maindonald, J., and Smith, L. (2002) Applying Lee-Carter
#' under conditions of variable mortality decline. \emph{Population Studies}, \bold{56}, 325-336.
#' @references Lee, R D, and Carter, L R (1992) Modeling and forecasting US mortality.
#' \emph{Journal of the American Statistical Association}, \bold{87}, 659-671.
#' @references Lee R D, and Miller T (2001). Evaluating the performance of the Lee-Carter
#'   method for forecasting mortality. \emph{Demography}, \bold{38}(4), 537–549.
#' \emph{International Journal of Forecasting}, to appear.
#' @author Rob J Hyndman.
#' @return A model specification.
#'
#' @examples
#' library(dplyr)
#' aus_mortality |>
#'   filter(State == "Victoria", Sex == "female") |>
#'   model(lee_carter = LC(Mortality))
#' @export
LC <- function(formula, adjust = c("dt", "dxt", "e0", "none"),
               scale = FALSE, ...) {
  adjust <- match.arg(adjust)
  lc_model <- new_model_class("lc", train = train_lc)
  new_model_definition(lc_model, !!enquo(formula), adjust = adjust, scale = scale, ...)
}

#' @importFrom stats sd
train_lc <- function(.data, sex = NULL, specials,  adjust = c("dt", "dxt", "e0", "none"),
                     scale = FALSE, ...) {
  attrx <- attributes(.data)
  indexvar <- index_var(.data)
  agevar <- attrx$agevar
  measures <- measured_vars(.data)
  measures <- measures[!(measures %in% c(agevar, attrx$populationvar))]
  out <- lca(.data, sex=sex, age=attrx$agevar, pop = attrx$populationvar,
      rates = find_measure(.data, c("mx", "mortality", "fx", "fertility", "rate")))

  fits <- as_tibble(.data) |>
    left_join(out$by_t, by = indexvar) |>
    left_join(out$by_x, by = agevar) |>
    mutate(
      .fitted = ax + kt*bx,
      .resid = .data[[measures]] - .fitted
    ) |>
    select(all_of(c(indexvar, agevar, ".fitted", ".resid")))

  structure(
    list(
      model = out,
      fitted = fits,
      nobs = sum(!is.na(.data[[measures]]))
    ),
    class = "model_lc"
  )
}
#' @param se Method used for computation of standard error.
#' Possibilities: \dQuote{innovdrift} (innovations and drift) and \dQuote{innovonly} (innovations only).
#' @param jumpchoice Method used for computation of jumpchoice.
#' Possibilities: \dQuote{actual} (use actual rates from final year) and \dQuote{fit} (use fitted rates).
#' The original Lee-Carter method used 'fit' (the default), but Lee and Miller (2001)
#' and most other authors prefer 'actual' (the default).
#' @rdname forecast
#' @export
forecast.model_lc <- function(object, new_data, bootstrap = FALSE, times = 5000,
    se = c("innovdrift", "innovonly"), jumpchoice = c("fit", "actual"), ...) {
  se <- match.arg(se)
  jumpchoice <- match.arg(jumpchoice)

  # Forecast all kt series using random walks with drift terms
  h <- length(unique(new_data[[index_var(new_data)]]))
  fc <- object$model$by_t |>
    fabletools::model(rw = fable::RW(kt ~ drift())) |>
    forecast(h = h)

  # Create forecasts of response series
  agevar <- colnames(object$model$by_x)[1]
  indexvar <- index_var(object$model$by_t)

  if(bootstrap) {
    sim <- map(seq_len(times), function(x) {
        generate(object, new_data, bootstrap = TRUE)[[".sim"]]
      }) |>
      transpose() |>
      map(as.numeric)
    distributional::dist_sample(sim)
  } else {
    new_data |>
      left_join(object$model$by_x, by = agevar) |>
      left_join(fc, by = indexvar) |>
      transmute(fc = exp(ax + bx * kt)) |>
      pull(fc)
  }
}

#' Generate from a mean model
#'
#' @param x A mable.
#' @param new_data A `tsibble` containing future information used to forecast.
#' @param bootstrap If `TRUE`, then forecast distributions are computed using simulation with resampled errors.
#' @param ... Additional arguments not used.
#' @importFrom stats na.omit
#'
#' @export
generate.model_lc <- function(x, new_data, bootstrap = FALSE, ...) {
  agevar <- attributes(new_data)$agevar
  new_data <- new_data |>
    dplyr::left_join(x$model, by = agevar)

  if (!(".innov" %in% names(new_data))) {
    if (bootstrap) {
      res <- residuals(x)
      new_data$.innov <- sample(na.omit(res), NROW(new_data), replace = TRUE)
    }
    else {
      new_data$.innov <- stats::rnorm(NROW(new_data), sd = x$model$sigma)
    }
  }

  transmute(group_by_key(new_data), ".sim" := mean + .innov)
}

#' Interpolate missing values
#'
#' Uses a fitted model to interpolate missing values from a dataset.
#'
#' @param object A mable containing a single model column.
#' @param new_data A dataset with the same structure as the data used to fit the model.
#' @param ... Other arguments passed to interpolate methods.
#'
#' @rdname interpolate
#' @export
interpolate.model_lc <- function(object, new_data, ...) {
  agevar <- attributes(new_data)$agevar
  measures <- measured_vars(new_data)
  measures <- measures[measures != agevar]
  y <- new_data[[measures]]
  miss_val <- is.na(y)
  fits <- object$fitted$.fitted

  new_data[[measures]][miss_val] <- fits[miss_val]
  new_data
}

#' Fitted values from a mean model
#'
#' @param object TBD
#' @param ... Not used.
#'
#' @export
fitted.model_lc <- function(object, ...) {
  object$fitted$.fitted
}

#' Residuals from a mean model
#'
#' @param object TBD
#' @param ... Not used.
#'
#' @export
residuals.model_lc <- function(object, ...) {
  object$fitted$.resid
}

#' Glance a average method model
#'
#' Construct a single row summary of the average method model.
#'
#' Contains the variance of residuals (`sigma2`).
#'
#' @inheritParams generics::glance
#'
#' @return A one row tibble summarising the model's fit.
#'
#' @export
glance.model_lc <- function(x, ...) {
  tibble(sigma2 = var(x$fitted$.resid, na.rm=TRUE))
}

#' Extract model coefficients from a mable
#'
#' This function will obtain the coefficients (and associated statistics) for
#' each model in the mable.
#'
#' @param x A mable.
#' @param ... Arguments for model methods.
#'
#' @rdname tidy
#' @export
tidy.model_lc <- function(x, ...) {
  return(NULL)
}

#' @export
report.model_lc <- function(object, ...) {
  cat("\n")
  print(object$model)
}

#' @export
model_sum.model_lc <- function(x) {
  paste0("LC")
}

# @examples
# # Compute Lee-Carter model for Australian females, males and total
# aus_lc <- aus_mortality |>
#   dplyr::filter(Code == "AUS") |>
#   lee_carter()
# aus_lc
# autoplot(aus_lc) +
#   patchwork::plot_annotation("Lee Carter components for Australia")
# autoplot(aus_lc$time, kt)


# Based on demography::lca()

lca <- function(data, sex, age, rates, pop,
                adjust = c("dt", "dxt", "e0", "none"), scale = FALSE) {
  adjust <- match.arg(adjust)

  index <- tsibble::index_var(data)
  startage <- min(data[[age]])

  # Extract mortality rates and population numbers
  year <- sort(unique(data[[index]]))
  deltat <- year[2] - year[1]
  ages <- sort(unique(data[[age]]))
  n <- length(ages)
  m <- length(year)
  mx <- matrix(data[[rates]], nrow = n, ncol = m)
  pop <- matrix(data[[pop]], nrow = n, ncol = m)

  # Transpose data and get deaths and logrates
  mx <- t(mx)
  mx[mx == 0] <- NA
  logrates <- log(mx)

  pop <- t(pop)
  deaths <- pop * mx
  deaths[is.na(deaths)] <- 0

  # Do SVD
  ax <- apply(logrates, 2, mean, na.rm = TRUE) # ax is mean of logrates by column
  if (any(ax < -1e9) | any(is.na(ax))) {
    # Estimate troublesome values with interpolation
    ax[ax < -1e9] <- NA
    ax <- stats::approx(seq_along(ax), ax, xout=seq_along(ax))$y
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

  # Adjust kt
  ktadj <- kt
  logdeathsadj <- matrix(NA, n, m)
  z <- log(t(pop)) + ax

  # Use regression to guess suitable range for root finding method
  x <- seq(m)
  ktse <- stats::predict(stats::lm(kt ~ x), se.fit = TRUE)$se.fit
  ktse[is.na(ktse)] <- 1
  agegroup <- ages[4] - ages[3]

  if (adjust == "dxt") {
    options(warn = -1) # Prevent warnings on non-integer population values
    for (i in seq(m)) {
      y <- as.numeric(deaths[i, ])
      zi <- as.numeric(z[, i])
      weight <- as.numeric(zi > -1e-8) # Avoid -infinity due to zero population
      yearglm <- stats::glm(y ~ offset(zi) - 1 + bx, family = stats::poisson, weights = weight)
      ktadj[i] <- yearglm$coef[1]
      logdeathsadj[, i] <- z[, i] + bx * ktadj[i]
    }
    options(warn = 0)
  } else if (adjust == "dt") {
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
        ktadj[i] <- findroot(FUN, guess = guess, margin = 10 * ktse[i], ax = ax, bx = bx, popi = pop[i, ], Dt = sum_deaths)
      }
      logdeathsadj[, i] <- z[, i] + bx * ktadj[i]
    }
  } else if (adjust == "e0") {
    e0 <- apply(mx, 1, get.e0, agegroup = ages, sex = sex, startage = startage)
    FUN2 <- function(p, e0i, ax, bx, ages, sex, startage) {
      e0i - estimate.e0(p, ax, bx, ages, sex, startage)
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

  names(ax) <- names(bx) <- ages

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
  mdevlogadd <- 2 / dflogadd * sum(deaths * log(deaths / deathsadjfit) - (deaths - deathsadjfit))
  dfloglin <- (m - 2) * n
  mdevloglin <- 2 / dfloglin * sum(deaths * log(deaths / deathslinfit) - (deaths - deathslinfit))
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

  # Return
  list(
    by_x = output1, by_t = output2,
    varprop = svd.mx$d[1]^2 / sum(svd.mx$d^2), mdev = mdev,
    adjust = adjust
  )
}

estimate.e0 <- function(kt, ax, bx, agegroup, sex, startage = 0) {
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


utils::globalVariables(c("kt", "ax", "bx", "varprop", "lst_data", "by_x", "by_t"))
