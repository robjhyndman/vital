#' Functions to smooth demographic data
#'
#' These smoothing functions allow smoothing of a variable in a vital object.
#' The vital object is returned along with some additional columns containing
#' information about the smoothed variable: usually `.smooth` containing the
#' smoothed values, and `.smooth_se` containing the corresponding standard errors.
#'
#' `smooth_mortality()` use penalized regression splines applied to log mortality
#' with a monotonicity constraint above age `b`. The methodology is based on Wood (1994).
#' `smooth_fertility()` uses weighted regression B-splines with a concavity constraint,
#' based on He and Ng (1999). The function `smooth_loess()` uses locally quadratic
#' regression, while `smooth_spline()` uses penalized regression splines.
#' @param .data A vital object
#' @param .var name of variable to smooth
#' @param age_spacing Spacing between ages for smoothed vital. Default is 1.
#' @param power Power transformation for age variable before smoothing. Default is 0.4 (for mortality data).
#' @param b Lower age for monotonicity. Above this, the smooth curve is assumed to be monotonically increasing.
#' @param k Number of knots to use for penalized regression spline estimate.
#' @param span Span for loess smooth.
#' @param lambda Penalty for constrained regression spline.
#' @return vital with added columns containing smoothed values and their standard errors
#' @references Hyndman, R.J., and Ullah, S. (2007) Robust forecasting of
#' mortality and fertility rates: a functional data approach.
#' *Computational Statistics & Data Analysis*, 51, 4942-4956.
#' <https://robjhyndman.com/publications/funcfor/>

#' @keywords smooth
#' @rdname smooth_vital
#' @author Rob J Hyndman
#' @examples
#' library(dplyr)
#' aus_mortality |>
#' 	filter(State == "Victoria", Sex == "female", Year > 2000) |>
#' 	smooth_mortality(Mortality)
#' aus_fertility |>
#'  filter(Year > 2000) |>
#' 	smooth_fertility(Fertility)
#' @export
smooth_spline <- function(.data, .var, age_spacing = 1, k = -1) {
	smooth_vital(.data, {{ .var }}, age_spacing, smooth_spline_x, k = k)
}

smooth_spline_x <- function(data, var, age_spacing, age, popvar, k = -1) {
	# smoothing with penalized spline
  weights <- smooth_weights(data, var, popvar, 1)
	form <- as.formula(paste(var, "~ s(", age, ",k =", k, ")"))
	fit <- mgcv::gam(form, weights = weights, data = data)
	new_data <- data.frame(
	  age = seq(min(data[[age]]), max(data[[age]]), by = age_spacing)
	)
	colnames(new_data) <- age
	out <- dplyr::bind_cols(
	  age = new_data[[age]],
	  as_tibble(mgcv::predict.gam(fit, se.fit = TRUE, newdata = new_data))
	 )
	colnames(out) <- c(age, ".smooth", ".smooth_se")
	return(out)
}

## Function to smooth mortality curves
## Divides age into three sections: 0-a, a-b and b+
## Will interpolate first period (0-a)
## Will smooth second period (a-b)
## For third period (b+) it uses montonically increasing smooths if monotonic TRUE

#' @rdname smooth_vital
#' @export
smooth_mortality <- function(.data, .var, age_spacing = 1, b = 65, power = 0.4, k = 30) {
	smooth_vital(.data, {{ .var }}, age_spacing, smooth_mortality_x, b = b, power = power, k = k)
}

smooth_mortality_x <- function(data, var, age_spacing, age, popvar, b = 65, power = 0.4, k = 30) {
	y <- data[[var]]
	x <- data[[age]]
  x_trans <- x^power
	y_trans <- log(y + 0.0000001)
	age_grid <- seq(min(data[[age]]), max(data[[age]]), by = age_spacing)
	xgrid <- age_grid^power
	weights <- smooth_weights(data, var, popvar, lambda = 0)
	smooth.fit <- smooth.monotonic(x_trans, y_trans, b^power, w = weights, k = k, newx = xgrid)
	out <- tibble(
		age = age_grid,
		.smooth = exp(smooth.fit$fit), #* (1 + 0.5 * smooth.fit$se.fit^2),
		.smooth_se = .smooth * smooth.fit$se.fit
	)
	colnames(out)[1] <- age
	return(out)
}

#' @rdname smooth_vital
#' @export
smooth_fertility <- function(.data, .var, age_spacing = 1, lambda = 1e-10) {
	smooth_vital(.data, {{ .var }}, age_spacing, smooth_fertility_x, lambda = lambda)
}

smooth_fertility_x <- function(data, var, age_spacing, age, popvar, lambda = 1e-10) {
	y <- data[[var]]
	x <- data[[age]]
	y_trans <- log(y + 0.0000001)
	age_grid <- seq(min(data[[age]]), max(data[[age]]), by = age_spacing)
  weights <- smooth_weights(data, var, popvar, lambda = 0)
	smooth_y <- fert.curve(x, y_trans, weights, lambda, age_grid)
	out <- tibble(
		age = age_grid,
		.smooth = exp(smooth_y$fit),
		.smooth_se = .smooth * smooth_y$se
	)
	colnames(out)[1] <- age
	return(out[c(age, ".smooth", ".smooth_se")])
}

#' @rdname smooth_vital
#' @export
smooth_loess <- function(.data, .var, age_spacing = 1, span = 0.2) {
	smooth_vital(.data, {{ .var }}, age_spacing, smooth_loess_x, span = span)
}

smooth_loess_x <- function(data, var, age_spacing, age, popvar, span = 0.2) {
	x <- data[[age]]
	y <- data[[var]]
	weights <- smooth_weights(data, var, popvar, lambda = 1)
	fit <- stats::loess(y ~ x, span = span, degree = 2, weights = weights, surface = "direct")
	age_grid <- seq(min(data[[age]]), max(data[[age]]), by = age_spacing)
	smooth_y <- predict(fit, se = TRUE, newdata = data.frame(x = age_grid))
  out <- tibble(
		age = age_grid,
		.smooth = smooth_y$fit,
		.smooth_se = smooth_y$se.fit
	)
	colnames(out)[1] <- age
	return(out[c(age, ".smooth", ".smooth_se")])
}

# Concave smoothing of fertility data

fert.curve <- function(x, y, w, lambda = 1, newx = x, ...) {
  w <- w / sum(w)
	fred <- stats::predict(
		cobs::cobs(
			x,
			y,
			w = w,
			constraint = "concave",
			lambda = lambda,
			print.warn = FALSE,
			print.mesg = FALSE,
			maxiter = 1e4
		),
		interval = "conf",
		nz = 200
	) |>
	  suppressWarnings()

	fit <- stats::approx(fred[, 1], fred[, 2], xout = newx, rule = 1)$y
	se <- stats::approx(fred[, 1], (fred[, 4] - fred[, 3]) / 2 / 1.96, xout = newx, rule = 1)$y
	return(list(fit = fit, se = se))
}

# Function to do cubic smoothing spline fit to y ~ x
# with constraint of monotonic increasing for x>b.
# Based on code provided by Simon Wood
# Last updated: 1 February 2014

smooth.monotonic <- function(x, y, b, k = -1, w = NULL, newx = x) {
	weight <- !is.null(w)
	if (k < 3 & k != -1) {
		stop("Inappropriate value of k")
	}

	# Unconstrained smooth.
	miss <- is.na(y)
	if (weight) {
		miss <- miss | w < 1e-9
	}
	yy <- y[!miss]
	xx <- x[!miss]
	if (weight) {
		w <- w[!miss]
		w <- w / sum(w) * length(w)
		f.ug <- mgcv::gam(yy ~ s(xx, k = k), weights = w)
		#        assign("w",w,pos=1)
	} else {
		f.ug <- mgcv::gam(yy ~ s(xx, k = k))
	}

	if (max(xx) <= b) {
		return(mgcv::predict.gam(f.ug, newdata = data.frame(xx = newx), se.fit = TRUE))
	}

	# Create Design matrix, constraints etc. for monotonic spline....
	mgcv::gam(yy ~ s(xx, k = k), data = data.frame(xx = xx, yy = yy), fit = FALSE) -> G
	if (weight) {
		G$w <- w
	}
	nc <- 200 # number of constraints
	xc <- seq(b, max(xx), l = nc + 1) # points at which to impose constraints
	A0 <- mgcv::predict.gam(f.ug, data.frame(xx = xc), type = "lpmatrix")
	# A0%*%p will evaluate spline at the xc points
	A1 <- mgcv::predict.gam(f.ug, data.frame(xx = xc + 1e-6), type = "lpmatrix")
	A <- (A1 - A0) / 1e-6 # approximate constraint matrix
	# (A%%p is  -ve gradient of spline at points xc)

	G$Ain <- A # constraint matrix
	G$bin <- rep(0, nc + 1) # constraint vector
	G$sp <- f.ug$sp # use smoothing parameters from un-constrained fit
	k <- G$smooth[[1]]$df + 1
	G$p <- rep(0, k)
	G$p[k] <- 0.1 # get monotonic starting parameters, by
	# setting coefficiants of polynomial part of term
	G$p[k - 1] <- -mean(0.1 * xx) # must ensure that gam side conditions are
	# met so that sum of smooth over x's is zero
	#    G$p <- rep(0,k+1)
	#    G$p[k+1] <- 0.1
	#    G$p[k] <- -mean(0.1*xx)
	G$y <- yy
	G$off <- G$off - 1 # indexing inconsistency between pcls and internal gam
	G$C <- matrix(0, 0, 0) # fixed constraint matrix (there are none)
	p <- mgcv::pcls(G) # fit spline (using s.p. from unconstrained fit)

	# now modify the gam object from unconstrained fit a little, to use it
	# for predicting and plotting constrained fit.
	f.ug$coefficients <- p
	return(mgcv::predict.gam(f.ug, newdata = data.frame(xx = newx), se.fit = TRUE))
}

smooth_vital <- function(.data, .var, age_spacing, smooth_fn, ...) {
  if(rlang::quo_is_missing(enquo(.var))) {
    stop("Please specify which variable to smooth. .var is missing with no default.")
  }
  # Index variable
	index <- tsibble::index_var(.data)
	# Keys including age
	keys <- tsibble::key_vars(.data)
	attrx <- attributes(.data)
	age <- attrx$agevar
	if (is.null(age)) {
		stop("No age variable found")
	}
	pop <- attrx$populationvar
	# Drop Age as a key and nest results
	keys_noage <- keys[!(keys %in% c(age, "AgeGroup", "Age_Group"))]
	# Turn .var into character
	resp <- names(eval_select(enquo(.var), data = .data))
	nested_data <- tidyr::nest(as_tibble(.data), .by = tidyr::all_of(c(index, keys_noage)))
	smooth <- purrr::map(nested_data[["data"]],
    \(x) smooth_fn(x, var = resp, age_spacing = age_spacing, age = age, pop = pop, ...)
	)
	nested_data$sm <- smooth
	nested_data$data <- NULL
	out <- tibble::as_tibble(nested_data) |>
		tidyr::unnest(cols = sm) |>
	  left_join(as_tibble(.data), by = c(index, keys_noage, age))
	cols <- c(colnames(.data), ".smooth", ".smooth_se")
	out[cols] |>
	  as_tsibble(index = index, key = all_of(keys)) |>
		as_vital(
			index = index,
			key = all_of(keys),
			.age = age,
			.sex = attrx$sexvar,
			.population = attrx$populationvar,
			.deaths = attrx$deathsvar,
			.births = attrx$birthsvar,
			reorder = TRUE
		)
}


smooth_weights <- function(data, var, popvar, lambda) {
  rate <- data[[var]]
  pop <- data[[popvar]]
  if(!is.null(pop)) {
    pop <- pop/max(pop, na.rm = TRUE)
    weight <- pop * rate^(1 - 2*lambda)
    if (mean(weight, na.rm = TRUE) < 0)
      stop("There's a problem. Do you have negative rates?")
    weight[weight < 0 | is.na(weight) | abs(weight) > 1e50] <- 0
  } else {
    weight <- rep(1, NROW(data))
  }
  return(weight / sum(weight, na.rm = TRUE))
}

utils::globalVariables(c("sm","rate", ".smooth"))
