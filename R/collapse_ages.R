# Function to collapse upper ages into a single age group

#' Collapse upper ages into a single age group. Counts are summed while
#' rates are recomputed where possible.
#' @details If the object includes deaths,
#' population and mortality rates, then deaths and population are summed and
#' mortality rates are recomputed as deaths/population. But if the object contains
#' mortality rates but not deaths and population, then the last rate remains
#' unchanged (and a warning is generated).
#'
#' @param .data A vital object including an age variable
#' @param max_age Maximum age to include in the collapsed age group.
#'
#' @return A vital object with the same variables as `.data`, but with the upper
#' ages collapsed into a single age group.
#' @author Rob J Hyndman
#' @examples
#' norway_mortality |>
#'   dplyr::filter(Sex == "Female") |>
#'   collapse_ages(max_age = 85)
#' @export

collapse_ages <- function(.data, max_age = 100) {
  if (!inherits(.data, "vital")) {
    stop(".data needs to be a vital object")
  }
  colnames <- colnames(.data)
  attr_data <- vital_var_list(.data)
  # Index variable
  index <- tsibble::index_var(.data)
  # Keys including age
  keys <- tsibble::key_vars(.data)
  age <- attr_data$age
  if (!(max_age %in% .data[[age]])) {
    stop("max_age must be one of the ages in .data")
  }
  keys_noage <- non_age_keys(.data)

  # Identify other columns
  pop <- attr_data$population
  deaths <- attr_data$deaths
  births <- attr_data$births
  sex <- attr_data$sex
  rates <- find_measures(.data, c("mx", "mortality", "fx", "fertility", "rate"))

  # Compute death and birth counts if they are missing
  for (i in rates) {
    if (!is.null(pop)) {
      if (tolower(i) %in% c("mx", "mortality") && is.null(deaths)) {
        .data[[".Deaths"]] <- .data[[i]] * .data[[pop]]
        deaths <- ".Deaths"
      } else if (tolower(i) %in% c("fx", "fertility") && is.null(births)) {
        .data[[".Births"]] <- .data[[i]] * .data[[pop]]
        births <- ".Births"
      }
    }
  }

  # Collapse data by summing for max_age and above, using the ages of each
  # group as some groups may not have all ages
  tbl <- .data |>
    as_tibble() |>
    dplyr::arrange(dplyr::across(all_of(c(index, keys_noage, age))))
  collapsed <- tbl |>
    group_by(across(all_of(c(index, keys_noage)))) |>
    dplyr::reframe(dplyr::across(
      everything(),
      function(x) {
        collapse_age_vector(
          x,
          tbl[[age]][dplyr::cur_group_rows()],
          max_age,
          # Rates are recomputed below
          dplyr::cur_column() %in% c(setdiff(keys, keys_noage), rates)
        )
      }
    )) |>
    as_tsibble(index = index, key = all_of(c(keys_noage, age)))
  upper_ages <- collapsed[[age]] == max_age

  # Recompute rates where possible
  for (i in rates) {
    if (tolower(i) %in% c("mx", "mortality")) {
      counts <- deaths
    } else if (tolower(i) %in% c("fx", "fertility")) {
      counts <- births
    } else {
      counts <- NULL
    }
    if (!is.null(pop) & !is.null(counts)) {
      collapsed[[i]][upper_ages] <- collapsed[[counts]][upper_ages] /
        collapsed[[pop]][upper_ages]
    } else {
      # Rates were truncated, so keep the value at max_age
      warning("Cannot recompute rates for ", i, ". Using upper age value.")
    }
  }

  # Return result
  return(as_vital(
    collapsed,
    .age = age,
    .sex = sex,
    .deaths = deaths,
    .births = births,
    .population = pop,
    reorder = TRUE
  )[, colnames])
}

collapse_age_vector <- function(x, ages, max_age, truncate = FALSE) {
  if (is.numeric(x)) {
    # Truncate age and rate variables, and sum others
    if (truncate) {
      out <- x[ages <= max_age]
    } else {
      # Sum upper group
      out <- c(x[ages < max_age], sum(x[ages >= max_age]))
    }
  } else if (is.character(x)) {
    # Probably AgeGroup. Add + to upper group
    out <- x[ages <= max_age]
    if (!endsWith(out[length(out)], "+")) {
      out[length(out)] <- paste0(out[length(out)], "+")
    }
  } else if (is.logical(x)) {
    # Perhaps OpenInterval variable
    out <- c(x[ages < max_age], any(x[ages >= max_age]))
  } else {
    # No idea what this is, but just truncate
    out <- x[ages <= max_age]
  }
  return(out)
}
