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
  purrr::map_dfr(lst_data, function(dt) {
    lt(dt$mx, startage=min(dt$x), agegroup = dt$x[2]-dt$x[1],
       sex=dt$group[1]) %>%
    tibble::rownames_to_column("Age") %>%
    mutate(Year = dt$year[1], Group=dt$group[1])
  }) %>%
    as_tsibble(index=Year, key=c(Age, Group)) %>%
    dplyr::select(Year, Age, Group, dplyr::everything()) %>%
    mutate(Age = as.numeric(Age)) %>%
    arrange(Group, Year, Age)
}

lt <- function(mx, startage = 0, agegroup = 5, sex) {
  # Omit missing ages
  if (is.na(mx[1])) {
    mx[1] <- 0
  }
  firstmiss <- (1:length(mx))[is.na(mx)][1]
  if (!is.na(firstmiss)) {
    mx <- mx[1:(firstmiss - 1)]
  }
  nn <- length(mx)
  if (nn < 1) {
    stop("Not enough data to proceed")
  }

  # Compute width of each age group
  if (agegroup == 1) {
    nx <- c(rep(1, nn - 1), Inf)
  } else if (agegroup == 5) { # First age group 0, then 1-4, then 5-year groups.
    nx <- c(1, 4, rep(5, nn - 3), Inf)
  } else {
    stop("agegroup must be either 1 or 5")
  }

  if (agegroup == 5 & startage > 0 & startage < 5) {
    stop("0 < startage < 5 not supported for 5-year age groups")
  }

  if (startage == 0) # for single year data and the first age (0) in 5-year data
  {
    if (sex == "female") {
      if (mx[1] < 0.107) {
        a0 <- 0.053 + 2.8 * mx[1]
      } else {
        a0 <- 0.35
      }
    }
    else if (sex == "male") {
      if (mx[1] < 0.107) {
        a0 <- 0.045 + 2.684 * mx[1]
      } else {
        a0 <- 0.33
      }
    }
    else # if(sex == "total")
    {
      if (mx[1] < 0.107) {
        a0 <- 0.049 + 2.742 * mx[1]
      } else {
        a0 <- 0.34
      }
    }
  }
  else if (startage > 0) {
    a0 <- 0.5
  } else {
    stop("startage must be non-negative")
  }
  if (agegroup == 1) {
    if (nn > 1) {
      ax <- c(a0, rep(0.5, nn - 2), Inf)
    } else {
      ax <- Inf
    }
  }
  else if (agegroup == 5 & startage == 0) {
    if (sex == "female") {
      if (mx[1] < 0.107) {
        a1 <- 1.522 - 1.518 * mx[1]
      } else {
        a1 <- 1.361
      }
    }
    else if (sex == "male") {
      if (mx[1] < 0.107) {
        a1 <- 1.651 - 2.816 * mx[1]
      } else {
        a1 <- 1.352
      }
    }
    else # sex == "total"
    {
      if (mx[1] < 0.107) {
        a1 <- 1.5865 - 2.167 * mx[1]
      } else {
        a1 <- 1.3565
      }
    }
    ax <- c(a0, a1, rep(2.6, nn - 3), Inf)
    ### ax=2.5 known to be too low esp at low levels of mortality
  }
  else # agegroup==5 and startage > 0
  {
    ax <- c(rep(2.6, nn - 1), Inf)
    nx <- c(rep(5, nn))
  }
  qx <- nx * mx / (1 + (nx - ax) * mx)
  # age <- startage + cumsum(nx) - 1
  # if (max(age) >= 75) {
  #    idx <- (age >= 75)
  #   ax[idx] <- (1/mx + nx - nx/(1 - exp(-nx * mx)))[idx]
  #  qx[idx] <- 1 - exp(-nx * mx)[idx]
  #    }
  # qx[qx > 1] <- 1  ################  NOT NEEDED IN THEORY

  # plot(qx)  #### TO CHECK RESULT RE QX>1

  qx[nn] <- 1
  if (nn > 1) {
    lx <- c(1, cumprod(1 - qx[1:(nn - 1)]))
    dx <- -diff(c(lx, 0))
  }
  else {
    lx <- dx <- 1
  }
  Lx <- nx * lx - dx * (nx - ax)
  Lx[nn] <- lx[nn] / mx[nn]
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx / lx
  if (nn > 2) {
    rx <- c(Lx[1] / lx[1], Lx[2:(nn - 1)] / Lx[1:(nn - 2)], Tx[nn] / Tx[nn - 1])
  } else if (nn == 2) {
    rx <- c(Lx[1] / lx[1], Tx[nn] / Tx[nn - 1])
  } else {
    rx <- c(Lx[1] / lx[1])
  }
  if (agegroup == 5) {
    rx <- c(
      0, (Lx[1] + Lx[2]) / 5 * lx[1], Lx[3] / (Lx[1] + Lx[2]),
      Lx[4:(nn - 1)] / Lx[3:(nn - 2)], Tx[nn] / Tx[nn - 1]
    )
  }
  result <- data.frame(
    ax = ax, mx = mx, qx = qx, lx = lx,
    dx = dx, Lx = Lx, Tx = Tx, ex = ex, rx = rx, nx = nx
  )
  return(result)
}
