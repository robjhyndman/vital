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
#' @param power Power transformation for age variable before smoothing. Default is 0.4 for mortality data and 1 (no transformation) for fertility or migration data.
#' @param b Lower age for monotonicity. Above this, the smooth curve is assumed to be monotonically increasing.
#' @param k Number of knots to use for penalized regression spline estimate.
#' @param span Span for loess smooth.
#' @param lambda Penalty for constrained regression spline.
#' @param weights Vector of weights.
#' @return vital with added columns containing smoothed values and their standard errors
#' @references Hyndman, R.J., and Ullah, S. (2007) Robust forecasting of
#' mortality and fertility rates: a functional data approach.
#' *Computational Statistics & Data Analysis*, **51**, 4942-4956.
#' <https://robjhyndman.com/publications/funcfor/>

#' @keywords smooth
#' @rdname smooth_vital
#' @author Rob J Hyndman
#' @examples
#' library(dplyr)
#' aus_mortality |>
#' 	filter(State == "Victoria", Sex == "female") |>
#' 	smooth_mortality(Mortality)
#' aus_fertility |>
#' 	smooth_fertility(Fertility)
#' @export
smooth_spline <- function(.data, .var, k = -1, weights = NULL) {
	smooth_vital(.data, {{ .var }}, smooth_spline_x, k = k, weights = weights)
}

smooth_spline_x <- function(data, var, age, k = -1, weights = NULL) {
	# smoothing with penalized spline
	form <- as.formula(paste(var, "~ s(", age, ",k =", k, ")"))
	fit <- mgcv::gam(form, weights = weights, data = data)
	out <- as_tibble(mgcv::predict.gam(fit, se.fit = TRUE))
	out[[age]] <- data[[age]]
	colnames(out) <- c(".smooth", ".smooth_se", age)
	return(out[c(age, ".smooth", ".smooth_se")])
}

## Function to smooth mortality curves
## Divides age into three sections: 0-a, a-b and b+
## Will interpolate first period (0-a)
## Will smooth second period (a-b)
## For third period (b+) it uses montonically increasing smooths if monotonic TRUE

#' @rdname smooth_vital
#' @export
smooth_mortality <- function(.data, .var, b = 65, power = 0.4, k = 30, weights = NULL) {
	smooth_vital(.data, {{ .var }}, smooth_mortality_x, b = b, power = power, k = k, weights = weights)
}

smooth_mortality_x <- function(data, var, age, b = 65, power = 0.4, k = 30, weights = NULL) {
	y <- data[[var]]
	x <- data[[age]]
	if (sum(!is.na(y)) < 3) {
		out <- tibble(
			age = x,
			.smooth = y,
			.smooth_se = 0
		)
	} else {
	  x_trans <- x^power
		y_trans <- log(y + 0.0000001)
		smooth.fit <- smooth.monotonic(x_trans, y_trans, b^power, w = weights, k = k)
		out <- tibble(
			age = x,
			.smooth = exp(smooth.fit$fit) * (1 + 0.5 * smooth.fit$se.fit^2),
			.smooth_se = exp(smooth.fit$fit) * smooth.fit$se.fit
		)
	}
	colnames(out)[1] <- age
	return(out[c(age, ".smooth", ".smooth_se")])
}

#' @rdname smooth_vital
#' @export
smooth_fertility <- function(.data, .var, lambda = 1e-10) {
	smooth_vital(.data, {{ .var }}, smooth_fertility_x, lambda = lambda)
}

smooth_fertility_x <- function(data, var, age, lambda = 1e-10) {
	y <- data[[var]]
	x <- data[[age]]
	y_trans <- y^0.4
	smooth_y <- fert.curve(x, y_trans, lambda)
	out <- tibble(
		age = x,
		.smooth = smooth_y$fit^2.5,
		.smooth_se = smooth_y$se
	)
	colnames(out)[1] <- age
	return(out[c(age, ".smooth", ".smooth_se")])
}

#' @rdname smooth_vital
#' @export
smooth_loess <- function(.data, .var, span = 0.2, weights = NULL) {
	smooth_vital(.data, {{ .var }}, smooth_loess_x, span = span)
}

smooth_loess_x <- function(data, var, age, span = 0.2, weights = NULL) {
	x <- data[[age]]
	y <- data[[var]]
	fit <- stats::loess(y ~ x, span = span, degree = 2, weights = weights, surface = "direct")
	smooth_y <- predict(fit, se = TRUE)
	out <- tibble(
		age = x,
		.smooth = smooth_y$fit,
		.smooth_se = smooth_y$se.fit
	)
	colnames(out)[1] <- age
	return(out[c(age, ".smooth", ".smooth_se")])
}

# Concave smoothing of fertility data

fert.curve <- function(x, y, lambda = 1, ...) {
	oldwarn <- options(warn = -1)
	fred <- stats::predict(
		cobs::cobs(
			x,
			y,
			constraint = "concave",
			lambda = lambda,
			print.warn = FALSE,
			print.mesg = FALSE,
			maxiter = 1e4
		),
		interval = "conf",
		nz = 200
	)
	options(warn = oldwarn$warn)

	fit <- stats::approx(fred[, 1], fred[, 2], xout = x, rule = 1)$y
	se <- stats::approx(fred[, 1], (fred[, 4] - fred[, 3]) / 2 / 1.96, xout = x, rule = 1)$y
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

smooth_vital <- function(.data, .var, smooth_fn, ...) {
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
	# Drop Age as a key and nest results
	keys_noage <- keys[!(keys %in% c(age, "AgeGroup", "Age_Group"))]
	# Turn .var into character
	resp <- names(eval_select(enquo(.var), data = .data))
	nested_data <- tidyr::nest(as_tibble(.data), .by = tidyr::all_of(c(index, keys_noage)))
	smooth <- purrr::map(nested_data[["data"]], \(x) smooth_fn(x, var = resp, age = age, ...))
	nested_data$sm <- smooth
	nested_data$data <- NULL
	out <- tibble::as_tibble(nested_data) |>
		tidyr::unnest(cols = sm)
	as_tibble(.data) |>
		left_join(out, by = c(index, keys_noage, age)) |>
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


use_weight_mortality <- function(data, type=c("mortality", "fertility")) {
  type = match.arg(type)
  attrx <- attributes(data)
  if(is.null(attrx$populationvar)) {
    warning("No population variable found, so weighting not used.")
    return(data)
  }
  data[[attrx$populationvar]] <- data[[attrx$populationvar]]/ max(data[[attrx$populationvar]])
  if(type=="fertility") {
    data$.weights <- data[[attrx$populationvar]] * rate^(1-2*data$lambda)
  }
  data$.weights = data[[attrx$populationvar]]
  if(mean(data$.weights, na.rm=TRUE) < 0)
    stop("There's a problem. Do you have negative rates?")

    #w[[i]][w[[i]] > 1e9] <- 0
    #w[[i]][w[[i]] < 0] <- 0
    #w[[i]][log(rate) > -1e-9] <- 0
    #w[[i]] <- apply(w[[i]],2,standardize,sumx=rate.dim[1])
    #w[[i]][is.na(w[[i]])] <- 0

    return(data)
}


utils::globalVariables(c("sm","rate"))
