#' Functions to smooth demographic data
#'
#' `smooth_vital()` allows smoothing of a vital object. It effectively groups
#' the data by keys other than age, and the index, and then applies the smoothing
#' functions specified. So it is like a group_by() followed by a mutate(), where the
#' grouping is determined by the attributes of the vital object.
#'
#' `smooth_spline()` and `smooth_mortality()` use penalized regression splines, the latter with a
#' monotonicity constraint above age `b`. The methodology is based on Wood (1994).
#' `smooth_fertility()` uses weighted regression B-splines with a concavity constraint,
#' based on He and Ng (1999). Finally, `smooth_loess()` uses locally quadratic
#' regression.
#' @param x vector of ages
#' @param y vector to smooth (e.g., mortality or fertility rates)
#' @param power Power transformation for age variable before smoothing. Default is 0.4 for mortality data and 1 (no transformation) for fertility or migration data.
#' @param b Lower age for monotonicity. Above this, the smooth curve is assumed to be monotonically increasing.
#' @param k Number of knots to use for penalized regression spline estimate.
#' @param span Span for loess smooth.
#' @param lambda Penalty for constrained regression spline.
#' @param weights Vector of weights.
#' @return Vector of smoothed values
#' @keywords smooth
#' @rdname smooth_vital
#' @author Rob J Hyndman
#' @examples
#' library(dplyr)
#' aus_mortality |>
#'   smooth_vital(sm_mort = smooth_mortality(Age, Mortality))
#' @export
smooth_vital <- function(data, ...) {
  keys <- tsibble::key_vars(data)
  attrx <- attributes(data)
  agevar <- attributes(data)$agevar
  indexvar <- index_var(data)
  keys_noage <- keys[keys != agevar]
  data <- as_tibble(data) |>
    group_by(dplyr::vars(c(indexvar, keys_noage))) |>
    mutate(...)
  as_vital(data, index=indexvar, keys = keys, age = agevar,
           population = attrx$populationvar,
           deaths = attrx$deathsvar,
           births = attrx$birthsvar
  )
}


#' @rdname smooth_vital
#' @export
smooth_spline <- function(x, y, k = -1, weights = NULL) {
	# smoothing with penalized spline
	fit <- mgcv::gam(y ~ x, weights = weights)
	mgcv::predict.gam(fit)
}

## Function to smooth mortality curves
## Divides age into three sections: 0-a, a-b and b+
## Will interpolate first period (0-a)
## Will smooth second period (a-b)
## For third period (b+) it uses montonically increasing smooths if monotonic TRUE

#' @rdname smooth_vital
#' @export
smooth_mortality <- function(x, y, b = 65, power = 0.4, weights = NULL) {
	x_trans <- x^power
	y_trans <- log(y)
	smooth.fit <- smooth.monotonic(x_trans, y_trans, b^power, w = weights)
	return(exp(smooth.fit$fit))
}

#' @rdname smooth_vital
#' @export
smooth_fertility <- function(x, y, b = 65, k = 30, lambda = 1e-10) {
	y_trans <- y^0.4
	if (is.null(weights)) {
		weights <- rep(1, length(y))
	}
	smooth_y <- fert.curve(x, y_trans, weights)$fit
	return(smooth_y^2.5)
}

#' @rdname smooth_vital
#' @export
smooth_loess <- function(x, y, span = 0.2, weights = NULL) {
	fit <- loess(y ~ x, span = span, degree = 2, weights = weights, surface = "direct")
	return(stats::predict(fit)$fit)
}

# Concave smoothing of fertility data

fert.curve <- function(x, y, lambda = 1, ...) {
	oldwarn <- options(warn = -1)
	fred <- stats::predict(
		cobs(x, y, constraint = "concave", lambda = lambda,
		     print.warn = FALSE, print.mesg = FALSE, maxiter = 1e4 ),
		interval = "conf",
		nz = 200
	)
	options(warn = oldwarn$warn)

	fit <- stats::approx(fred[,1], fred[,2], xout = x, rule = 1)$y
	se <- stats::approx(fred[,1], (fred[,4] - fred[,3])/2/1.96, xout = x, rule = 1)$y
	return(list(fit = fit, se = se))
}

# Function to do cubic smoothing spline fit to y ~ x
# with constraint of monotonic increasing for x>b.
# Based on code provided by Simon Wood
# Last updated: 1 February 2014

smooth.monotonic <- function(x, y, b, k = -1, w = NULL, newx = x ) {
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
