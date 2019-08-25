#' Compute period lifetables from mortality rates
#'
#' All available years and ages are included in the tables.
#' qx = mx/(1 + ((1-ax) * mx)) as per Chiang (1984).
#' Warning: the code has only been tested for data based on single-year age groups.
#'
#' @param .data A tsibble including an age variable and a variable containing mortality rates.
#' @param age Variable in `.data` containing start year of age intervals. If omitted, the variable with name `Age` will be used (not case sensitive).
#' @param sex Optional variable in `.data` containing sex information. If omitted, the variable with name `Sex` or `Group` will be used (not case sensitive).
#' @param mortality Variable in `.data` containing Mortality rates (mx). If omitted, the variable with name  `Mortality` or `Rate` will be used (not case sensitive).
#'
#' @return A tsibble object.
#' @rdname life_table
#'
#' @references Chiang CL. (1984) \emph{The life table and its applications}. Robert E Krieger Publishing Company: Malabar.
#' @references Keyfitz, N, and Caswell, H. (2005) \emph{Applied mathematical demography}, Springer-Verlag: New York.
#' @references Preston, S.H., Heuveline, P., and Guillot, M. (2001) \emph{Demography: measuring and modeling population processes}. Blackwell
#'
#' @examples
#' # Compute Australia life table for females in 2003
#' library(dplyr)
#' aus_mortality %>%
#'   filter(Code=="AUS", Sex=="female", Year==2003) %>%
#'   life_table()
#' @export

life_table <- function(.data, age, sex, mortality) {
  # Index variable
  index <- tsibble::index_var(.data)
  # Keys including age
  keys <- tsibble::key_vars(.data)

  # Find age and mortality columns
  if(!missing(age))
    age <- {{age}}
  else
    age <- keys[tolower(keys) == "age"]
  if(!missing(sex))
    sex <- {{sex}}
  else {
    sex <- keys[tolower(keys) == "sex"]
    if(length(sex)==0L) {
      sex <- keys[tolower(keys) == "group"]
      if(length(sex)==0L)
      sex = "None"
    }
  }
  if(!missing(mortality))
    mortality <- {{mortality}}
  else {
    measures <- tsibble::measured_vars(.data)
    mortality <- measures[startsWith(tolower(measures),"mortality")][1]
    if(length(mortality)==0L) {
      mortality <- measures[startsWith(tolower(measures),"rate")][1]
      if(length(mortality)==0L)
        stop("Mortality rates column not found")
    }
  }

  # Drop Age as a key and nest results
  keys_noage <- keys[keys != age]
  .data <- tidyr::nest(.data, -index, -!!keys_noage, .key="lst_data")

  # Create life table for each sub-tibble and row-bind them.
  if(sex=="None")
    out <- purrr::map2(.data[['lst_data']], "None", lt, age=age, mortality=mortality)
  else
    out <- purrr::map2(.data[['lst_data']], .data[[sex]], lt, age=age, mortality=mortality)
  .data$lt <- out
  .data$lst_data <- NULL
  out <- tibble::as_tibble(.data) %>% tidyr::unnest() %>% tsibble::as_tsibble(index=index, key=keys)
  # Sort and rearrange results
  arrange(out, !!!rlang::syms(c(keys_noage, index, age))) %>%
    select(!!index, !!age, !!keys_noage, tidyselect::everything())
}

# This is a revised version of the demography::lt function.

lt <- function(dt, sex, age, mortality) {
  # Order by age
  dt <- dt[order(dt[[age]]),]

  # Grab information from tibble
  mx <- dt[[mortality]]
  sex <- sex[1]
  ifelse(sex=="None", "total", tolower(dt[[sex]][1]))
  ages <- sort(round(unique(dt[[age]])))
  startage <- ages[1]
  agegroup <- ages[2]-ages[1]

  # Check we can proceed
  if (agegroup == 5L & startage > 0L & startage < 5L) {
    stop("0 < startage < 5 not supported for 5-year age groups")
  } else if (startage < 0L) {
    stop("startage must be non-negative")
  } else if (agegroup != 1L & agegroup != 5L) {
    print(dt)
    stop("Only 1-year and 5-year agegroups handled")
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
  nx <- c(1L, rep(agegroup, nn - 2), Inf)
  if (agegroup == 5L) {
    nx[2] <- 4L
  }

  # Set remaining ax values
  if (agegroup == 1L) {
    if (nn > 1) {
      ax <- c(a0, rep(0.5, nn - 2L), Inf)
    } else {
      ax <- Inf
    }
  }
  else if (agegroup == 5L & startage == 0) {
    a1 <- dplyr::case_when(
      sex == "female" ~ 1.361 + (mx[1] < 0.107) * (0.161 - 1.518 * mx[1]),
      sex == "male" ~ 1.352 + (mx[1] < 0.107) * (0.299 - 2.816 * mx[1]),
      TRUE ~ 1.3565 + (mx[1] < 0.107) * (0.230 - 2.167 * mx[1])
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
  # if (nn > 2) {
  #   rx <- c(Lx[1] / lx[1], Lx[2:(nn - 1)] / Lx[1:(nn - 2)], Tx[nn] / Tx[nn - 1])
  # } else if (nn == 2) {
  #   rx <- c(Lx[1] / lx[1], Tx[nn] / Tx[nn - 1])
  # } else {
  #   rx <- c(Lx[1] / lx[1])
  # }
  # if (agegroup == 5L) {
  #   rx <- c(
  #     0, (Lx[1] + Lx[2]) / 5 * lx[1], Lx[3] / (Lx[1] + Lx[2]),
  #     Lx[4:(nn - 1)] / Lx[3:(nn - 2)], Tx[nn] / Tx[nn - 1]
  #   )
  # }
  # Return the results in a tibble
  result <- tibble::tibble(mx = mx, qx = qx, lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = ex) %>%
    # Omitting  rx = rx, nx = nx, ax = ax, Do we need them?
    mutate(Age = dt[[age]])

  return(result)
}
