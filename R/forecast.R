# Function performs predictions of k and life expectancy based on leecarter results (in lcaout)

#' Forecasts using Lee-Carter method.
#'
#' The kt coefficients are forecast using a random walk with drift.
#' The forecast coefficients are then multiplied by bx to obtain a forecast
#' demographic rate curve.
#'
#' @param object Output from \code{\link{lee_carter}}.
#' @param h Number of years ahead to forecast.
#' @param se Method used for computation of standard error. Possibilities: \dQuote{innovdrift} (innovations and drift) and \dQuote{innovonly} (innovations only).
#' @param jumpchoice Method used for computation of jumpchoice. Possibilities: \dQuote{actual} (use actual rates from final year) and \dQuote{fit} (use fitted rates).
#' @param level Confidence level for prediction intervals.
#' @param ... Other arguments.
#'
#' @return Object of class \code{fm_forecast} with the following components:
#' \item{age}{Ages from \code{object}.}
#' \item{year}{Years from \code{object}.}
#' \item{rate}{List of matrices containing forecasts, lower bound and upper bound of prediction intervals.
#'   Point forecast matrix takes the same name as the series that has been forecast.}
#' \item{fitted}{Matrix of one-step forecasts for historical data}
#' Other components included are
#' \item{e0}{Forecasts of life expectancies (including lower and upper bounds)}
#' \item{kt.f}{Forecasts of coefficients from the model.}
#' \item{type}{Data type.}
#' \item{model}{Details about the fitted model}
#'
#' @author Rob J Hyndman
#' @examples
#' france.lca <- lee_carter(aus_mortality, adjust="e0")
#' france.fcast <- forecast(france.lca, 50)
#' plot(france.fcast)
#' plot(france.fcast,'c')
#' @keywords models
##' @export
forecast.lca_model <- function(object, h=50, se=c("innovdrift","innovonly"),
                               jumpchoice=c("fit","actual"), level=80, ...) {
  se <- match.arg(se)
  jumpchoice <- match.arg(jumpchoice)

  # Section 1 Read in data from object
  jumpyear <- max(object$year)
  nyears <- length(object$year)
  nages <- length(object$age)

  # Find jumprates
  if(jumpchoice=="actual")
    jumprates <- object[[4]][,nyears]
  else if(jumpchoice=="fit")
    jumprates <- exp(object$ax + object$bx*object$kt[nyears])
  else
    stop(paste("Unknown jump choice:",jumpchoice))
  object$kt <- object$kt - object$kt[nyears]

  # Time series estimation of kt as Random walk with drift
  fit <- forecast::rwf(object$kt, drift=TRUE)
  kt.drift <- fit$model$par$drift
  sec <- fit$model$par$drift.se
  see <- sqrt(fit$model$sigma2)

  # Project kt
  x <- 1:h
  zval <- stats::qnorm(0.5 + 0.005*level)
  kt.forecast <- object$kt[nyears] + (x * kt.drift)

  # Calculate standard errors of forecast kt
  if (se=="innovdrift")
    kt.stderr <- sqrt(x*(see^2) + (x*sec)^2)
  else if(se=="innovonly")
    kt.stderr <- sqrt(x*(see^2))
  kt.lo.forecast <- kt.forecast - (zval*kt.stderr)
  kt.hi.forecast <- kt.forecast + (zval*kt.stderr)
  kt.f <- data.frame(kt.forecast,kt.lo.forecast,kt.hi.forecast)
  names(kt.f) <- c("kt forecast","kt lower","kt upper")
  deltat <- object$year[2] - object$year[1]
  kt.f <- ts(kt.f,start=object$year[nyears]+deltat,deltat=deltat)

  # Calculate expected life and mx forecasts
  e0.forecast <- rep(0,h)
  mx.forecast <- matrix(0,nrow=nages,ncol=h)
  colnames(mx.forecast) <- seq(h)
  rownames(mx.forecast) <- object$age
  mx.lo.forecast <- mx.hi.forecast <- mx.forecast
  logjumprates <- log(jumprates)
  series <- names(object)[4]
  agegroup <- object$age[4]-object$age[3]
  for (cnt in 1:h)
  {
    mx.forecast[,cnt] <- fitmx(kt.f[cnt,1], logjumprates, object$bx)
    mx.lo.forecast[,cnt] <- fitmx(kt.f[cnt,2], logjumprates, object$bx)
    mx.hi.forecast[,cnt] <- fitmx(kt.f[cnt,3], logjumprates, object$bx)
    e0.forecast[cnt] <- get.e0(mx.forecast[,cnt],agegroup,series,startage=min(object$age))
  }
  kt.f <- data.frame(kt.forecast,kt.lo.forecast,kt.hi.forecast)
  names(kt.f) <- c("kt forecast","kt lower","kt upper")
  kt.f <- ts(kt.f,start=object$year[nyears]+deltat,deltat=deltat)

  output = list(label=object$label,age=object$age,year=object$year[nyears] + x*deltat,
                rate=list(forecast=mx.forecast,lower=mx.lo.forecast,upper=mx.hi.forecast),
                fitted=object$fitted,
                e0=ts(e0.forecast,start=object$year[nyears]+deltat,deltat=deltat),
                kt.f=structure(list(mean=kt.f[,1],lower=kt.f[,2],upper=kt.f[,3],level=level,x=object$kt,
                                    method="Random walk with drift"),class="forecast"),
                type = object$type,lambda=0)
  names(output$rate)[1] = names(object)[4]
  output$model <- object
  output$model$jumpchoice <- jumpchoice
  output$model$jumprates <- jumprates
  output$call <- match.call()
  output$name <- names(object)[4]
  return(structure(output,class=c("fmforecast","demogdata")))
}
