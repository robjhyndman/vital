#' Model mortality or fertility data using Lee-Carter approach
#'
#' Lee-Carter model of mortality or fertility rates. \code{lee_carter} produces a
#' standard Lee-Carter model by default, although many other options are
#' available. Missing rates are set to the geometric mean rate for the relevant age.
#'
#' @param .data A tsibble including an age variable and a variable containing mortality or fertility rates.
#' @param age Variable in `.data` containing start year of age intervals. If omitted, the variable with name `Age` or `Age_group` will be used (not case sensitive).
#' @param sex Optional variable in `.data` containing sex information. If omitted, the variable with name `Sex` or `Group` will be used (not case sensitive).
#' @param rates Variable in `.data` containing mortality or fertility rates.
#' If omitted, it will search for a variable with one of the following names:
#' `mx`, `mortality`, `fx`, `fertility` or `rate` (not case sensitive).
#' @param pop Variable in `.data` containing population numbers. If omitted, it
#' will search for a variable with one of the following names:
#' `pop`, `population`, `ex`, `exposure`.
#' @param adjust method to use for adjustment of coefficients \eqn{k_t kt}.
#'   Possibilities are
#'   \dQuote{dt} (Lee-Carter method, the default),
#'   \dQuote{dxt} (BMS method),
#'   \dQuote{e0} (Lee-Miller method based on life expectancy) and
#'   \dQuote{none}.
#' @param scale If TRUE, bx and kt are rescaled so that kt has drift parameter = 1.
#'
#' @return A list containing various model objects.
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
#'
#' @author Rob J Hyndman.
#'
#' @keywords models
#' @examples
#' # Compute Lee-Carter model for Australian females, males and total
#' aus_lca <- aus_mortality |>
#'   dplyr::filter(Code == "AUS") |>
#'   lee_carter()
#' aus_lca
#' autoplot(aus_lca, "Lee Carter components for Australia")
#' autoplot(aus_lca$time, kt)
#' @export

lee_carter <- function(.data, age, sex, rates, pop,
                       adjust = c("dt", "dxt", "e0", "none"),
                       scale = FALSE) {
  adjust <- match.arg(adjust)

  # Index variable
  index <- tsibble::index_var(.data)
  # Keys including age
  keys <- tsibble::key_vars(.data)

  # Find age and mortality columns
  if (!missing(age)) {
    age <- {{ age }}
  } else {
    age <- find_key(.data, c("age", "age_group"))
  }
  if (!missing(sex)) {
    sex <- {{ sex }}
  } else {
    sex <- find_key(.data, c("sex", "group"), return_error = FALSE)
  }
  if (!missing(rates)) {
    rates <- {{ rates }}
  } else {
    rates <- find_measure(.data, c("mx", "mortality", "fx", "fertility", "rate"))
  }
  if (!missing(pop)) {
    pop <- {{ pop }}
  } else {
    pop <- find_measure(.data, c("pop", "population", "ex", "exposure"))
  }

  # Drop Age as a key and nest results
  keys_noage <- keys[keys != age]
  .data <- tidyr::nest(.data, lst_data = c(-!!keys_noage))

  if (sex %in% colnames(.data)) {
    sex_col <- .data[[sex]]
  } else {
    sex_col <- sex
  }

  out <- purrr::map2(.data[["lst_data"]], sex_col, lca,
    age = age, rates = rates,
    pop = pop, adjust = adjust, scale = scale
  )
  final <- dplyr::select(.data, -lst_data)
  final$by_x <- lapply(out, function(x){x$by_x})
  final$by_t <- lapply(out, function(x){x$by_t})
  final$varprop <- unlist(lapply(out, function(x){x$varprop}))
  final$adjust <- unlist(lapply(out, function(x){x$adjust}))

  # Split return object into three pieces
  final <- list(
    fit = final |>
      select(-by_x, -by_t),
    age = final |>
      select(-varprop, -adjust, -by_t) |>
      tidyr::unnest(by_x),
    time = final |>
      select(-varprop, -adjust, -by_x) |>
      tidyr::unnest(by_t) |>
      as_tsibble(index = index, key = keys_noage),
    call = match.call(),
    agevar = age,
    timevar = as.character(index)
  )

  # Split return object into three pieces
  return(structure(final, class = "lca_model"))
}

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


# Replace zeros with interpolated values
fill.zero <- function(x, method = "constant") {
  tt <- 1:length(x)
  zeros <- abs(x) < 1e-9
  xx <- x[!zeros]
  tt <- tt[!zeros]
  x <- stats::approx(tt, xx, 1:length(x), method = method, f = 0.5, rule = 2)
  return(x$y)
}

#' @export
print.lca_model <- function(x, ...) {
  cat("Lee-Carter model\n")
  # call <- deparse(x$call)
  # Remove new lines
  # call <- paste0(call, collapse="")
  # Remove multiple spaces
  # call <- gsub("\\s\\s+", " ", call)
  # cat(paste("\nCall:", call, "\n"))
  cat("\nFit:\n")
  print(x$fit)
  #cat("\nAge components:\n")
  #print(x$age, n = 5)
  #cat("\nTime components:\n")
  #print(x$time, n = 5)
  #cat("\n")
}

#' Plot of Lee-Carter model components
#'
#' Plot of ax, bx and kt from a Lee-Carter model.
#'
#' @param object output from \code{\link{lee_carter}}.
#' @param ... Other arguments not used.
#' @return A list of ggplot objects.
#' @export
autoplot.lca_model <- function(object, ...) {
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))
  keys <- tsibble::key_vars(object$time)
  p1 <- age_plot(object$age, ax, keys)
  p2 <- age_plot(object$age, bx, keys)
  p3 <- fabletools::autoplot(object$time, kt) +
    ggplot2::xlab(tsibble::index_var(object$time))

  structure(list(p1, p2, p3), class = c("life_components", "gg"))
}

# Plot a variable against age by key
age_plot <- function(object, .var, keys) {
  # Convert age to time and use fabletools::autoplot.tbl_ts
  names <- colnames(object)[!(colnames(object) %in% c(keys, deparse(substitute(.var))))]
  age <- names[grep("age", names, ignore.case=TRUE)]
  object_ts <- tsibble::as_tsibble(object, index=age, key = keys[keys != age])
  fabletools::autoplot(object_ts, {{ .var }}) + ggplot2::xlab(age)
}

#' @export
`+.life_components` <- function(e1, e2) {
  e1[[1]] <- e1[[1]] + e2
  e1
}

#' @export
print.life_components <- function(x, ...) {
  x <- lapply(x, ggplot2::ggplotGrob)
  gt <- gtable::gtable(
    name = "life_components",
    heights = grid::unit(rep(1, 2), "null"),
    widths = grid::unit(rep(1, 2), "null")
  )
  gt <- gtable::gtable_add_grob(gt, x,
    t = c(1, 1, 2), b = c(1, 1, 2), l = c(1, 2, 2), r = c(1, 2, 2),
    z = seq_along(x), clip = "off"
  )
  grid.draw(gt)
}


#' @importFrom grid grid.draw
#' @method grid.draw life_components
#' @export
grid.draw.life_components <- function(x, recording = TRUE) {
  print(x)
}

utils::globalVariables(c("kt", "ax", "bx", "varprop", "lst_data", "by_x", "by_t"))
