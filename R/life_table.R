#' Compute period lifetables from mortality rates
#'
#' All available years and ages are included in the tables.
#' qx = mx/(1 + ((1-ax) * mx)) as per Chiang (1984).
#' Warning: the code has only been tested for data based on single-year age groups.
#'
#' @param data. A tsibble containing mortality rates, or deaths and exposures. It must contain an age variable and a variable containing mortality rates.
#' @param age. Variable in `data` containing start year of age intervals.
#' @param mortality. Variable in `data` containing Mortality rates (mx).
#'
#' @return A tsibble object.
#' @rdname life_table
#'
#' @references Chiang CL. (1984) \emph{The life table and its applications}. Robert E Krieger Publishing Company: Malabar.
#' @references Keyfitz, N, and Caswell, H. (2005) \emph{Applied mathematical demography}, Springer-Verlag: New York.
#' @references Preston, S.H., Heuveline, P., and Guillot, M. (2001) \emph{Demography: measuring and modeling population processes}. Blackwell
#'
#' @examples
#' # Compute French life tables
#' demography::fr.mort %>%
#'   as_tsibble() %>%
#'   life_table(age=Age, mortality=Mortality, group=Group)
#' @export

life_table <- function(.data, age, mortality, group) {
  # Index names
  index <- attributes(.data)$index
  # How many groups?
  groups <- unique(dplyr::pull(.data, {{group}}))
  years <- unique(dplyr::pull(.data, index))
  # Split into a list of tibbles with consistent names
  lst_data <- list()
  k <- 0
  for(i in seq_along(groups)) {
    for(j in seq_along(years)) {
      k <- k+1
      df <- dplyr::rename(.data,
                          x = {{age}},
                          mx = {{mortality}},
                          group = {{group}},
                          year = attributes(.data)$index)
      df <- dplyr::filter(df,
                         group == groups[i],
                         year == years[j]) %>%
            dplyr::arrange(x)
      lst_data[[k]] <- as_tibble(df)
    }
  }
  # Create life table for each sub-tibble and row-bind them.
  purrr::map_dfr(lst_data, function(dt) { lt(dt) }) %>%
    as_tsibble(index=Year, key=c(Age, Group)) %>%
    dplyr::select(Year, Age, Group, dplyr::everything()) %>%
    arrange(Group, Year, Age)
}

lt <- function(dt) {
  # Grab information from tibble
  mx <- dt$mx
  startage <- round(min(dt$x))
  agegroup <- round(dt$x[2]-dt$x[1])
  sex <- tolower(dt$group[1])

  if (agegroup == 5L & startage > 0L & startage < 5L)
    stop("0 < startage < 5 not supported for 5-year age groups")
  else if(startage < 0L)
    stop("startage must be non-negative")
  else if(agegroup != 1L & agegroup != 5L)
    stop("Only 1-year and 5-year agegroups handled")

  # Set a0
  if (startage == 0L) {
    a0 <- case_when(
        sex == "female" ~ 0.35 + (mx[1]<0.107)*(-0.297 + 2.8 * mx[1]),
        sex == "male"   ~ 0.33 + (mx[1]<0.107)*(-0.285 + 2.684 * mx[1]),
        TRUE            ~ 0.34 + (mx[1]<0.107)*(-0.291 + 2.742 * mx[1])
      )
  } else {
    a0 <- 0.5
  }

  # Compute width of each age group
  nn <- NROW(dt)
  nx <- c(1L, rep(agegroup, nn-2), Inf)
  if(agegroup == 5L)
    nx[2] <- 4L

  # Set remaining ax values
  if (agegroup == 1L) {
    if (nn > 1) {
      ax <- c(a0, rep(0.5, nn - 2L), Inf)
    } else {
      ax <- Inf
    }
  }
  else if (agegroup == 5L & startage == 0) {
    a1 <- case_when(
      sex == "female" ~ 1.361 + (mx[1]<0.107)*(0.161 - 1.518 * mx[1]),
      sex == "male"   ~ 1.352 + (mx[1]<0.107)*(0.299 - 2.816 * mx[1]),
      TRUE            ~ 1.3565+ (mx[1]<0.107)*(0.230 - 2.167 * mx[1])
    )
    ax <- c(a0, a1, rep(2.6, nn - 3L), Inf)
  } else { # agegroup==5 and startage > 0
    ax <- c(rep(2.6, nn - 1), Inf)
    nx[1L] <- agegroup
  }
  # Find qx
  qx <- nx * mx / (1 + (nx - ax) * mx)
  qx[nn] <- 1
  # Find lx and dx
  if (nn > 1) {
    lx <- c(1, cumprod(1 - qx[1:(nn - 1)]))
    dx <- -diff(c(lx, 0))
  }
  else {
    lx <- dx <- 1
  }
  # Now Lx, Tx and ex
  Lx <- nx * lx - dx * (nx - ax)
  Lx[nn] <- lx[nn] / mx[nn]
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx / lx
  # Finally compute rx
  if (nn > 2) {
    rx <- c(Lx[1] / lx[1], Lx[2:(nn - 1)] / Lx[1:(nn - 2)], Tx[nn] / Tx[nn - 1])
  } else if (nn == 2) {
    rx <- c(Lx[1] / lx[1], Tx[nn] / Tx[nn - 1])
  } else {
    rx <- c(Lx[1] / lx[1])
  }
  if (agegroup == 5L) {
    rx <- c(
      0, (Lx[1] + Lx[2]) / 5 * lx[1], Lx[3] / (Lx[1] + Lx[2]),
      Lx[4:(nn - 1)] / Lx[3:(nn - 2)], Tx[nn] / Tx[nn - 1]
    )
  }
  # Return the results in a tibble
  result <- tibble::tibble(
      ax = ax, mx = mx, qx = qx, lx = lx,
      dx = dx, Lx = Lx, Tx = Tx, ex = ex, rx = rx, nx = nx
    ) %>%
    mutate(Age = dt$x, Year = dt$year[1], Group=dt$group[1])

  return(result)
}
