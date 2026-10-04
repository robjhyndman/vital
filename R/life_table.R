#' Compute period life tables from age-specific mortality rates
#'
#' All available years and ages are included in the tables.
#' $qx = mx/(1 + ((1-ax) * mx))$ as per Chiang (1984).
#' Ages can be single years, abridged (0, 1, 5, 10, ...), or 5-year groups
#' starting at age 5 or above.
#' Missing mortality rates are interpolated (on the log scale) from
#' neighbouring ages, with a warning.
#'
#' @param .data A `vital` including an age variable and a variable containing mortality rates.
#' @param mortality Variable in `.data` containing Mortality rates (mx). If omitted, the variable with name  `mx`, `Mortality` or `Rate` will be used (not case sensitive).
#'
#' @author Rob J Hyndman
#' @return A vital object containing the index, keys, and the new life table
#' variables `mx`, `qx`, `lx`, `dx`, `Lx`, `Tx`, `ex`, `rx` (survivorship
#' ratios), `nx` (widths of the age groups) and `ax` (average years lived in
#' each age group by those dying in it).
#' @rdname life_table
#'
#' @references Chiang CL. (1984) *The life table and its applications*. Robert E Krieger Publishing Company: Malabar.
#' @references Keyfitz, N, and Caswell, H. (2005) *Applied mathematical demography*, Springer-Verlag: New York.
#' @references Preston, S.H., Heuveline, P., and Guillot, M. (2001) *Demography: measuring and modeling population processes*. Blackwell
#'
#' @examples
#' # Compute Norwegian life table for females in 2003
#' norway_mortality |>
#'   dplyr::filter(Sex == "Female", Year == 2003) |>
#'   life_table()
#' @export

life_table <- function(.data, mortality) {
  # Mortality variable
  mortality_quo <- enquo(mortality)
  if (!quo_is_missing(mortality_quo)) {
    mortality <- as_name(mortality_quo)
  } else {
    mortality <- find_measure(.data, c("mx", "mortality", "rate"))
  }
  if (is.na(mortality) | !(mortality %in% colnames(.data))) {
    vvar <- vital_var_list(.data)
    if (!is.null(vvar$deaths) & !is.null(vvar$population)) {
      # Compute Mx from deaths and population
      .data$Mx <- .data[[vvar$deaths]] / .data[[vvar$population]]
      mortality <- "Mx"
    } else {
      stop("Mortality variable not found in data")
    }
  }
  if (anyNA(.data[[mortality]])) {
    warning(
      "Missing mortality rates have been interpolated from neighbouring ages",
      call. = FALSE
    )
  }
  # Index variable
  index <- tsibble::index_var(.data)
  # Keys including age
  keys <- tsibble::key_vars(.data)

  age <- age_var(.data)
  if (is.null(age)) {
    stop("No age variable found")
  }
  sex <- sex_var(.data)
  if (is.null(sex)) {
    sex <- "None"
  }

  # Drop Age as a key and nest results
  keys_noage <- non_age_keys(.data)
  # Age keys to keep with each life table (e.g. AgeGroup as well as age)
  age_keys <- c(age, setdiff(keys, c(age, keys_noage)))
  .data <- tidyr::nest(.data, .by = tidyselect::all_of(c(index, keys_noage)))

  # Create life table for each sub-tibble and row-bind them.
  out <- purrr::map2(
    .data[["data"]],
    if (sex == "None") "None" else .data[[sex]],
    lt,
    age = age,
    mortality = mortality,
    keep = age_keys
  )
  .data$lt <- out
  .data$data <- NULL
  tsibble::as_tibble(.data) |>
    tidyr::unnest(cols = lt) |>
    tsibble::as_tsibble(index = index, key = tidyselect::all_of(keys)) |>
    as_vital(.age = age, .sex = sex, reorder = TRUE)
}

# This is a revised version of the demography::lt function.

# keep contains the columns of dt to return alongside the life table
lt <- function(dt, sex, age, mortality, keep = age) {
  # Order by age
  dt <- dt[order(dt[[age]]), ]

  # Grab information from tibble
  mx <- dt[[mortality]]
  sex <- tolower(sex[1])
  ages <- sort(round(unique(dt[[age]])))
  startage <- ages[1]
  widths <- diff(ages)
  # Abridged ages are 0, 1, 5, 10, ...
  abridged <- length(ages) > 2 &&
    identical(ages[1:3], c(0, 1, 5)) &&
    all(widths[-(1:2)] == 5)

  # Check we can proceed
  if (startage < 0L) {
    stop("startage must be non-negative")
  } else if (all(widths == 1)) {
    agegroup <- 1L
  } else if (abridged || all(widths == 5)) {
    agegroup <- 5L
  } else {
    stop("Only 1-year and 5-year agegroups handled")
  }
  if (agegroup == 5L && !abridged && startage < 5L) {
    stop(
      "5-year age groups starting below age 5 must have separate groups for ages 0 and 1-4"
    )
  }

  # Set a0
  if (startage == 0L) {
    a0 <- dplyr::case_when(
      sex == "female" ~ 0.35 + (mx[1] < 0.107) * (-0.297 + 2.8 * mx[1]),
      sex == "male" ~ 0.33 + (mx[1] < 0.107) * (-0.285 + 2.684 * mx[1]),
      TRUE ~ 0.34 + (mx[1] < 0.107) * (-0.291 + 2.742 * mx[1])
    )
  } else {
    a0 <- 0.5
  }

  # Compute width of each age group
  nn <- NROW(dt)
  nx <- c(widths, Inf)

  # Interpolate missing rates
  mx <- fill_mx(mx, dt[[age]])

  # Set remaining ax values
  if (agegroup == 1L) {
    if (nn > 1) {
      ax <- c(a0, rep(0.5, nn - 2L), Inf)
    } else {
      ax <- Inf
    }
  } else if (abridged) {
    a1 <- dplyr::case_when(
      sex == "female" ~ 1.361 + (mx[1] < 0.107) * (0.161 - 1.518 * mx[1]),
      sex == "male" ~ 1.352 + (mx[1] < 0.107) * (0.299 - 2.816 * mx[1]),
      TRUE ~ 1.3565 + (mx[1] < 0.107) * (0.230 - 2.167 * mx[1])
    )
    ax <- c(a0, a1, rep(2.6, nn - 3L), Inf)
  } else {
    # agegroup==5 and startage >= 5
    ax <- c(rep(2.6, nn - 1), Inf)
  }
  # Find qx
  qx <- nx * mx / (1 + (nx - ax) * mx)
  qx[nn] <- 1
  # Find lx and dx
  if (nn > 1) {
    lx <- pmax(0, c(1, cumprod(1 - qx[1:(nn - 1)])))
    dx <- -diff(c(lx, 0))
  } else {
    lx <- dx <- 1
  }
  # Now Lx, Tx and ex
  Lx <- nx * lx - dx * (nx - ax)
  Lx[nn] <- if_else(mx[nn] == 0, 0, lx[nn] / mx[nn])
  Lx[is.na(Lx)] <- 0
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx / lx
  # Finally compute rx
  if (abridged) {
    rx <- c(
      0,
      (Lx[1] + Lx[2]) / 5 * lx[1],
      Lx[3] / (Lx[1] + Lx[2]),
      Lx[4:(nn - 1)] / Lx[3:(nn - 2)],
      Tx[nn] / Tx[nn - 1]
    )
  } else if (nn > 2) {
    rx <- c(Lx[1] / lx[1], Lx[2:(nn - 1)] / Lx[1:(nn - 2)], Tx[nn] / Tx[nn - 1])
  } else if (nn == 2) {
    rx <- c(Lx[1] / lx[1], Tx[nn] / Tx[nn - 1])
  } else {
    rx <- c(Lx[1] / lx[1])
  }
  # Return the results in a tibble
  result <- tibble::tibble(
    mx = mx,
    qx = qx,
    lx = lx,
    dx = dx,
    Lx = Lx,
    Tx = Tx,
    ex = ex,
    rx = rx,
    nx = nx,
    ax = ax
  ) |>
    dplyr::bind_cols(dt[keep])

  return(result)
}

# Fill missing mortality rates by linear interpolation of log rates between
# neighbouring ages with positive rates, using the nearest such rate beyond
# the youngest or oldest of them
fill_mx <- function(mx, age) {
  miss <- is.na(mx)
  ok <- !miss & mx > 0
  if (!any(miss) || !any(ok)) {
    return(mx)
  }
  if (sum(ok) == 1L) {
    mx[miss] <- mx[ok]
  } else {
    mx[miss] <- exp(
      stats::approx(age[ok], log(mx[ok]), xout = age[miss], rule = 2)$y
    )
  }
  mx
}
