#' Generalized APC stochastic mortality model
#'
#' A Generalized Age-Period-Cohort (GAPC) stochastic mortality mode is defined
#' in Villegas et al. (2018). The StMoMo package is used to fit the model. Separate
#' functions are available to fit various special cases of the GAPC model.
#'
#' `LC2()` provides an alternative implementation of the Lee-Carter model based
#' on a GAPC specification. The advantage of this approach over `LC()` is that
#' it allows for 0 rates in the mortality data. Note that it does not return
#' identical results to `LC()` because the model formulation is different.
#' For `LC2()`, do not take logs of the mortality rates because this is handled
#' with the link function. For `LC()`, you need to take logs of the mortality rates
#' when calling the function.
#'
#' The Renshaw-Haberman (RH) model due to Renshaw and Haberman (2006) is another
#' special case of a GAPC model, that can be considered an extension of a Lee-Carter
#' model with an age-specific cohort effect.
#'
#' The Age-Period-Cohort (APC) model is a special case of a GAPC model discussed
#' by Renshaw and Haberman (2011).
#'
#' The Cairns-Blake-Dowd (CBD) model due to Cairns et al (2006) can be considered
#' another  special case of a GAPC model that is primarily intended for forecasting
#' mortality patterns in older populations.
#'
#' Cairns et al (2009) extended the CBD model by adding a cohort effect and a quadratic
#' age effect, giving the M7 model.
#'
#' Plat (2009) combined the CBD model with some features of the Lee-Carter model
#' to produce a model that is suitable for full age ranges and captures the cohort effect.
#'
#' Each of these functions returns a GAPC model applied to the formula's response
#' variable as a function of age. Use `autoplot()` on the fitted models to plot
#' their age, period and cohort components.
#' The model will optionally call \code{\link[StMoMo]{genWeightMat}} with arguments `clip` and `zeroCohorts`.
#' All other arguments are passed to \code{\link[StMoMo]{StMoMo}}.
#'
#' @aliases report.GAPC
#' @seealso [LC()]
#' @param formula Model specification
#' @param link The link function to use. Either "log" or "logit". When using
#' logit, the mortality rates need to be between 0 and 1. If they are not, most
#' likely you need to use initial rather than central population values when
#' computing them.
#' @param const defines the constraint to impose on the period index of the model
#' ensure identifiability. The alternatives are "sum" (default), "last" and "first"
#' which apply constraints \eqn{\sum_{t=1}^T \kappa_t=0}, \eqn{\kappa_T = 0} and
#' \eqn{\kappa_1 = 0} respectively.
#' @param cohortAgeFun A function defining the cohort age modulating parameter
#' \eqn{\beta_x^{(0)}}. It can take values: "NP" for a non-parametric age term or "1" for
#' \eqn{\beta_x^{(0)} = 1} (the default).
#' @param use_weights If `TRUE`, will call \code{\link[StMoMo]{genWeightMat}} with arguments `clip` and `zeroCohorts`.
#' @param clip Passed to \code{\link[StMoMo]{genWeightMat}()}
#' @param zeroCohorts Passed to \code{\link[StMoMo]{genWeightMat}()}
#' @param ... All other arguments passed to \code{\link[StMoMo]{StMoMo}()}
#'
#' @references Cairns, AJG, Blake, D, and Dowd, K (2006). A two-factor model for
#' stochastic mortality with parameter uncertainty: Theory and calibration.
#' *Journal of Risk and Insurance*, **73**(4), 687-718.
#' <doi:10.1111/j.1539-6975.2006.00195.x>
#' @references Cairns AJG, Blake D, Dowd K, Coughlan GD, Epstein D, Ong A, Balevich I (2009).
#' A quantitative comparison of stochastic mortality models using data from
#' England and Wales and the United States.
#' *North American Actuarial Journal*, **13**(1), 1–35.
#' <doi:10.1080/10920277.2009.10597538>
#' @references Lee, RD, and Carter, LR (1992) Modeling and forecasting US mortality.
#' *Journal of the American Statistical Association*, 87, 659-671.
#' <doi:10.1080/01621459.1992.10475265>
#' @references Plat R (2009). On stochastic mortality modeling.
#' *Insurance: Mathematics and Economics*, **45**(3), 393–404.
#' <doi:10.1016/j.insmatheco.2009.08.006>
#' @references Renshaw, AE, and Haberman, S (2006). A cohort-based extension to
#' the Lee-Carter model for mortality reduction factors.
#' *Insurance: Mathematics and Economics*, **38**(3), 556-570.
#' <doi:10.1016/j.insmatheco.2005.12.001>
#' @references Renshaw, AE, and Haberman, S (2011). A comparative study of
#' parametric mortality projection models.
#' *Insurance: Mathematics and Economics*, **48**(1), 35–55.
#' <doi:10.1016/j. insmatheco.2010.09.003>
#' @references Villegas, AM, Millossovich, P, and Kaishev, VK (2018).
#' StMoMo: An R package for stochastic mortality modelling.
#' *Journal of Statistical Software*, **84**(3), 1-38.
#' <doi:10.18637/jss.v084.i03>
#' @author Rob J Hyndman
#' @return A model specification.
#'
#' @examples
#' # Fit the same CBD model using GAPC() and CBD()
#' gapc <- norway_mortality |>
#'   dplyr::filter(Sex == "Female", Age > 50, Year > 2000) |>
#'   model(
#'     cbd1 = GAPC(Mortality,
#'       link = "log",
#'       staticAgeFun = FALSE,
#'       periodAgeFun = c("1", function(x, ages) x - mean(ages))
#'     ),
#'     cbd2 = CBD(Mortality)
#'   )
#' glance(gapc)
#' gapc |>
#'   dplyr::select(cbd2) |>
#'   report()
#' gapc |>
#'   dplyr::select(cbd2) |>
#'   autoplot()
#' @export
GAPC <- function(
  formula,
  use_weights = TRUE,
  clip = 0,
  zeroCohorts = NULL,
  ...
) {
  stmomo_model <- new_model_class("stmomo", train = train_stmomo)
  new_model_definition(
    stmomo_model,
    !!enquo(formula),
    use_weights = use_weights,
    clip = clip,
    zeroCohorts = zeroCohorts,
    ...
  )
}

#' @rdname GAPC
#' @export
LC2 <- function(
  formula,
  link = c("log", "logit"),
  const = c("sum", "last", "first"),
  use_weights = TRUE,
  clip = 0,
  zeroCohorts = NULL,
  ...
) {
  # Based on StMoMo::lc
  link <- match.arg(link)
  const <- match.arg(const)
  constLC <- function(ax, bx, kt, b0x, gc, wxt, ages) {
    c1 <- switch(
      const,
      sum = mean(kt[1, ], na.rm = TRUE),
      first = kt[1, 1],
      last = tail(kt[1, ], 1)
    )
    ax <- ax + c1 * bx[, 1]
    kt[1, ] <- kt[1, ] - c1
    c2 <- sum(bx[, 1], na.rm = TRUE)
    bx[, 1] <- bx[, 1] / c2
    kt[1, ] <- kt[1, ] * c2
    list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
  }
  stmomo_model <- new_model_class("stmomo", train = train_stmomo)
  new_model_definition(
    stmomo_model,
    !!enquo(formula),
    use_weights = use_weights,
    clip = clip,
    zeroCohorts = zeroCohorts,
    link = link,
    staticAgeFun = TRUE,
    periodAgeFun = "NP",
    constFun = constLC,
    ...
  )
}

#' @rdname GAPC
#' @export
CBD <- function(
  formula,
  link = c("log", "logit"),
  use_weights = TRUE,
  clip = 0,
  zeroCohorts = NULL,
  ...
) {
  # Based on StMoMo::cbd
  link <- match.arg(link)
  stmomo_model <- new_model_class("stmomo", train = train_stmomo)
  new_model_definition(
    stmomo_model,
    !!enquo(formula),
    use_weights = use_weights,
    clip = clip,
    zeroCohorts = zeroCohorts,
    link = link,
    staticAgeFun = FALSE,
    periodAgeFun = c("1", function(x, ages) x - mean(ages)),
    ...
  )
}

#' @rdname GAPC
#' @export
RH <- function(
  formula,
  link = c("log", "logit"),
  cohortAgeFun = c("1", "NP"),
  use_weights = TRUE,
  clip = 0,
  zeroCohorts = NULL,
  ...
) {
  # Based on StMoMo::rh
  link <- match.arg(link)
  cohortAgeFun <- match.arg(cohortAgeFun)
  constRHgeneral <- function(
    ax,
    bx,
    kt,
    b0x,
    gc,
    wxt,
    ages,
    cohortAgeFun,
    approxConst
  ) {
    c1 <- mean(kt[1, ], na.rm = TRUE)
    ax <- ax + c1 * bx[, 1]
    kt[1, ] <- kt[1, ] - c1
    c2 <- sum(bx[, 1], na.rm = TRUE)
    bx[, 1] <- bx[, 1] / c2
    kt[1, ] <- kt[1, ] * c2
    c3 <- mean(gc, na.rm = TRUE)
    ax <- ax + c3 * b0x
    gc <- gc - c3
    if (cohortAgeFun == "NP") {
      c4 <- sum(b0x, na.rm = TRUE)
      b0x <- b0x / c4
      gc <- gc * c4
    }
    list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
  }
  constRH <- function(ax, bx, kt, b0x, gc, wxt, ages) {
    constRHgeneral(
      ax,
      bx,
      kt,
      b0x,
      gc,
      wxt,
      ages,
      cohortAgeFun,
      FALSE
    )
  }

  stmomo_model <- new_model_class("stmomo", train = train_stmomo)
  new_model_definition(
    stmomo_model,
    !!enquo(formula),
    use_weights = use_weights,
    clip = clip,
    zeroCohorts = zeroCohorts,
    link = link,
    staticAgeFun = TRUE,
    periodAgeFun = "NP",
    cohortAgeFun = cohortAgeFun,
    constFun = constRH,
    ...
  )
}

#' @rdname GAPC
#' @export
APC <- function(
  formula,
  link = c("log", "logit"),
  use_weights = TRUE,
  clip = 0,
  zeroCohorts = NULL,
  ...
) {
  # Based on StMoMo::apc
  link <- match.arg(link)
  constAPC <- function(ax, bx, kt, b0x, gc, wxt, ages) {
    nYears <- dim(kt)[2]
    x <- ages
    t <- 1:nYears
    c <- (1 - tail(ages, 1)):(nYears - ages[1])
    phiReg <- stats::lm(gc ~ 1 + c, na.action = na.omit)
    phi <- stats::coef(phiReg)
    gc <- gc - phi[1] - phi[2] * c
    ax <- ax + phi[1] - phi[2] * x
    kt <- kt + phi[2] * t
    c1 <- mean(kt, na.rm = TRUE)
    kt <- kt - c1
    ax <- ax + c1
    list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
  }
  stmomo_model <- new_model_class("stmomo", train = train_stmomo)
  new_model_definition(
    stmomo_model,
    !!enquo(formula),
    use_weights = use_weights,
    clip = clip,
    zeroCohorts = zeroCohorts,
    link = link,
    staticAgeFun = TRUE,
    periodAgeFun = "1",
    cohortAgeFun = "1",
    constFun = constAPC,
    ...
  )
}

#' @rdname GAPC
#' @export
M7 <- function(
  formula,
  link = c("log", "logit"),
  use_weights = TRUE,
  clip = 0,
  zeroCohorts = NULL,
  ...
) {
  # Based on StMoMo::m7
  link <- match.arg(link)
  f1 <- function(x, ages) x - mean(ages)
  f2 <- function(x, ages) {
    xbar <- mean(ages)
    s2 <- mean((ages - xbar)^2)
    (x - xbar)^2 - s2
  }
  constM7 <- function(ax, bx, kt, b0x, gc, wxt, ages) {
    nYears <- dim(kt)[2]
    x <- ages
    t <- 1:nYears
    c <- (1 - tail(ages, 1)):(nYears - ages[1])
    xbar <- mean(x)
    s2 <- mean((x - xbar)^2)
    phiReg <- stats::lm(gc ~ 1 + c + I(c^2), na.action = na.omit)
    phi <- stats::coef(phiReg)
    gc <- gc - phi[1] - phi[2] * c - phi[3] * c^2
    kt[3, ] <- kt[3, ] + phi[3]
    kt[2, ] <- kt[2, ] - phi[2] - 2 * phi[3] * (t - xbar)
    kt[1, ] <- kt[1, ] +
      phi[1] +
      phi[2] * (t - xbar) +
      phi[3] *
        ((t - xbar)^2 + s2)
    list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
  }
  stmomo_model <- new_model_class("stmomo", train = train_stmomo)
  new_model_definition(
    stmomo_model,
    !!enquo(formula),
    use_weights = use_weights,
    clip = clip,
    zeroCohorts = zeroCohorts,
    link = link,
    staticAgeFun = FALSE,
    periodAgeFun = c("1", f1, f2),
    cohortAgeFun = "1",
    constFun = constM7,
    ...
  )
}

#' @rdname GAPC
#' @export
PLAT <- function(
  formula,
  link = c("log", "logit"),
  use_weights = TRUE,
  clip = 0,
  zeroCohorts = NULL,
  ...
) {
  link <- match.arg(link)
  f2 <- function(x, ages) mean(ages) - x
  constPlat <- function(ax, bx, kt, b0x, gc, wxt, ages) {
    nYears <- dim(wxt)[2]
    x <- ages
    t <- 1:nYears
    c <- (1 - tail(ages, 1)):(nYears - ages[1])
    xbar <- mean(x)
    phiReg <- stats::lm(gc ~ 1 + c + I(c^2), na.action = na.omit)
    phi <- stats::coef(phiReg)
    gc <- gc - phi[1] - phi[2] * c - phi[3] * c^2
    kt[2, ] <- kt[2, ] + 2 * phi[3] * t
    kt[1, ] <- kt[1, ] + phi[2] * t + phi[3] * (t^2 - 2 * xbar * t)
    ax <- ax + phi[1] - phi[2] * x + phi[3] * x^2
    ci <- rowMeans(kt, na.rm = TRUE)
    ax <- ax + ci[1] + ci[2] * (xbar - x)
    kt[1, ] <- kt[1, ] - ci[1]
    kt[2, ] <- kt[2, ] - ci[2]
    list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
  }
  stmomo_model <- new_model_class("stmomo", train = train_stmomo)
  new_model_definition(
    stmomo_model,
    !!enquo(formula),
    use_weights = use_weights,
    clip = clip,
    zeroCohorts = zeroCohorts,
    link = link,
    staticAgeFun = TRUE,
    periodAgeFun = c("1", f2),
    cohortAgeFun = "1",
    constFun = constPlat,
    ...
  )
}

# Training function

train_stmomo <- function(
  .data,
  sex = NULL,
  specials,
  use_weights = TRUE,
  clip = 0,
  zeroCohorts = NULL,
  ...
) {
  # Variable names
  vvar <- vital_var_list(.data)
  measures <- measured_vars(.data)
  measures <- measures[!(measures %in% c(vvar$age, vvar$population))]
  measures <- measures[1]

  # Compute StMoMo model
  model <- StMoMo::StMoMo(...)
  data2 <- vital_to_stmomo(.data)
  if (model$link == "logit" && any(data2$Dxt / data2$Ext > 1, na.rm = TRUE)) {
    stop(
      "Mortality rates must be less than 1 for logit link.
    Perhaps you need to use initial rather than central population values."
    )
  }

  if (use_weights) {
    wxt <- StMoMo::genWeightMat(
      ages = data2$ages,
      years = data2$years,
      clip = clip,
      zeroCohorts = zeroCohorts
    )
    miss <- is.na(data2$Dxt / data2$Ext)
    wxt[miss] <- 0
    data2$Dxt[miss] <- 0
    data2$Ext[miss] <- 1
  } else {
    wxt <- NULL
  }
  out <- StMoMo::fit(
    model,
    Dxt = data2$Dxt,
    Ext = data2$Ext,
    ages = data2$ages,
    years = data2$years,
    wxt = wxt,
    verbose = FALSE
  )
  out$data$series <- sex
  out$data$label <- "vital"

  # Fitted rates (ages in rows, years in columns) in long form
  indexvar <- index_var(.data)
  fits <- tibble(
    !!indexvar := rep(data2$years, each = length(data2$ages)),
    !!vvar$age := rep(data2$ages, length(data2$years)),
    .fitted = c(fitted(out, type = "rates"))
  )
  fits <- as_tibble(.data)[c(indexvar, vvar$age, measures)] |>
    left_join(fits, by = c(indexvar, vvar$age))
  fits$.innov <- fits[[measures]] - fits$.fitted
  fits[[measures]] <- NULL

  structure(
    list(
      model = out,
      fitted = fits,
      nobs = sum(!is.na(.data[[measures]]))
    ),
    class = "GAPC"
  )
}

#' @rdname forecast
#' @export

forecast.GAPC <- function(
  object,
  new_data = NULL,
  h = NULL,
  point_forecast = list(.mean = mean),
  simulate = FALSE,
  bootstrap = FALSE,
  times = 5000,
  ...
) {
  # Uncertainty does not work here. Users should call with simulate = TRUE for PI
  warning("Use simulate = TRUE to get distributional forecasts")
  indexvar <- index_var(new_data)
  agevar <- age_var(new_data)
  h <- length(unique(new_data[[indexvar]]))
  # StMoMo drops the years when h = 1, so forecast at least 2 years
  pred <- forecast(object$model, h = max(2, h))
  df <- as.data.frame(pred$rates) |>
    mutate(age = pred$ages) |>
    pivot_longer(-age, names_to = "year", values_to = ".mean")
  df$year <- as.numeric(df$year)
  df <- df[df$year %in% new_data[[indexvar]], ]
  if (!setequal(df$year, new_data[[indexvar]])) {
    stop("Years don't match")
  }
  if (!setequal(df$age, new_data[[agevar]])) {
    stop("Ages don't match")
  }
  df <- df[, c("year", "age", ".mean")]
  colnames(df)[1:2] <- c(indexvar, agevar)
  left_join(new_data, df, by = c(agevar, indexvar)) |>
    dplyr::pull(.mean) |>
    distributional::dist_degenerate()
}

#' @export
generate.GAPC <- function(
  x,
  new_data = NULL,
  h = NULL,
  bootstrap = FALSE,
  times = 1,
  ...
) {
  agevar <- age_var(new_data)
  indexvar <- index_var(new_data)
  h <- length(unique(new_data[[indexvar]]))
  if (times != length(unique(new_data$.rep))) {
    stop("`times` must equal the number of replicates (`.rep`) in `new_data`")
  }
  pred <- stats::simulate(x$model, nsim = times, h = max(2, h))
  df <- as.data.frame(pred$rates) |>
    mutate(age = pred$ages) |>
    pivot_longer(-age, names_to = "year", values_to = ".sim") |>
    mutate(
      .rep = sub(".*?(\\d*)$", "\\1", year),
      year = as.numeric(sub("\\.\\d*$", "", year))
    )
  df <- df[, c(".rep", "year", "age", ".sim")]
  colnames(df)[2:3] <- c(indexvar, agevar)
  left_join(new_data, df, by = c(".rep", indexvar, agevar))
}

#' @export
glance.GAPC <- function(x, ...) {
  tibble(
    loglik = x$model$loglik,
    deviance = x$model$deviance,
    nobs = x$model$nobs,
    npar = x$model$npar
  )
}

#' @export
tidy.GAPC <- function(x, ...) {
  return(NULL)
}

#' @export
report.GAPC <- function(object, ...) {
  print(object$model)
}

#' @export
model_sum.GAPC <- function(x) {
  paste0("GAPC")
}

# Convert vital object to a StMoMoData object
vital_to_stmomo <- function(.data) {
  # Variable names
  indexvar <- index_var(.data)
  vvar <- vital_var_list(.data)
  if (!("deaths" %in% names(vvar))) {
    stop("Deaths variable is required")
  }
  if (!("population" %in% names(vvar))) {
    stop("Population variable is required")
  }
  ages <- sort(unique(.data[[vvar$age]]))
  years <- sort(unique(.data[[indexvar]]))
  check_complete_grid(.data, vvar$age, indexvar)
  .data <- dplyr::arrange(
    as_tibble(.data),
    !!rlang::sym(indexvar),
    !!rlang::sym(vvar$age)
  )
  Dxt <- round(matrix(
    .data[[vvar$deaths]],
    nrow = length(ages),
    ncol = length(years)
  ))
  Ext <- matrix(
    .data[[vvar$population]],
    nrow = length(ages),
    ncol = length(years)
  )
  list(Dxt = Dxt, Ext = Ext, ages = ages, years = years)
}


#' @export
time_components.GAPC <- function(object, ...) {
  index <- index_var(first_fit(object)$data)
  unnest_time_components(
    object,
    function(x) {
      kt <- t(x$kt)
      if (NCOL(kt) > 1) {
        colnames(kt) <- paste0("k", seq(NCOL(kt)), "t")
      } else {
        colnames(kt) <- "kt"
      }
      yr <- tibble(Year = x$years)
      colnames(yr) <- index
      dplyr::bind_cols(yr, kt)
    },
    index
  )
}


#' @export
age_components.GAPC <- function(object, ...) {
  agevar <- age_var(first_fit(object)$data)
  unnest_components(object, function(x) {
    bx <- x$bx
    if (NCOL(bx) > 1 | !is.null(x$b0x)) {
      colnames(bx) <- paste0("b", seq(NCOL(bx)), "x")
    } else {
      colnames(bx) <- "bx"
    }
    ax <- tibble(Age = x$ages, ax = x$ax, b0x = x$b0x)
    colnames(ax)[1] <- agevar
    dplyr::bind_cols(ax, bx)
  })
}

#' @export
cohort_components.GAPC <- function(object, ...) {
  unnest_time_components(
    object,
    function(x) tibble(Birth_Year = as.integer(names(x$gc)), gc = x$gc),
    "Birth_Year"
  )
}


#' @export
autoplot.GAPC <- function(object, ...) {
  fit <- first_fit(object)
  agevar <- age_var(fit$data)
  obj_x <- age_components(object)
  obj_time <- time_components(object)
  index <- index_var(obj_time)
  keys <- setdiff(colnames(as_tibble(object)), attributes(object)$model)

  # Each column pairs an age function (top) with the index it multiplies (bottom)
  kvars <- setdiff(colnames(obj_time), c(keys, index))
  bvars <- if (length(kvars) == 1L) {
    intersect(c("bx", "b1x"), colnames(obj_x))
  } else {
    paste0("b", seq_along(kvars), "x")
  }
  top <- lapply(bvars, function(b) gapc_age_panel(obj_x, agevar, b, keys))
  bottom <- map2(kvars, bvars, function(k, b) {
    key_plot(obj_time, sym(index), k, keys) +
      ggplot2::ylab(gapc_index_label(obj_x, k, b))
  })
  # Cohort effect, against year of birth
  if (!is.null(fit$fit$model$gc)) {
    top <- c(top, list(gapc_age_panel(obj_x, agevar, "b0x", keys)))
    bottom <- c(bottom, list(
      key_plot(cohort_components(object), sym("Birth_Year"), "gc", keys) +
        ggplot2::labs(x = "Birth year", y = gapc_index_label(obj_x, "gc", "b0x"))
    ))
  }
  # Static age function, with the legend beneath it as in the LC plot.
  # Without one, put the legend in the top left if that space is blank.
  if ("ax" %in% colnames(obj_x)) {
    top <- c(list(key_plot(obj_x, sym(agevar), "ax", keys)), top)
    bottom <- c(list(patchwork::guide_area()), bottom)
  } else if (inherits(top[[1]], "spacer")) {
    top[[1]] <- patchwork::guide_area()
  }
  patchwork::wrap_plots(c(top, bottom), nrow = 2) +
    patchwork::plot_layout(guides = "collect")
}

# Value of an age function that does not vary with age, or NULL if it varies
constant_age_function <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) > 0 && all(abs(x - x[1]) <= 1e-8 * max(1, abs(x[1])))) {
    x[1]
  } else {
    NULL
  }
}

# Plot an age function, or leave a blank space if it is constant
gapc_age_panel <- function(obj_x, agevar, b, keys) {
  if (is.null(constant_age_function(obj_x[[b]]))) {
    key_plot(obj_x, sym(agevar), b, keys)
  } else {
    patchwork::plot_spacer()
  }
}

# Label an index, noting the value of its age function when that is constant
gapc_index_label <- function(obj_x, k, b) {
  value <- constant_age_function(obj_x[[b]])
  if (is.null(value)) {
    k
  } else {
    paste0(k, " (", b, " = ", format(signif(value, 4)), ")")
  }
}

utils::globalVariables(c(
  "Birth_Year",
  "age",
  "year"
))
