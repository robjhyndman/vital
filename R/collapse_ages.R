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
#' aus_mortality |>
#'  dplyr::filter(State == "Victoria", Sex == "female") |>
#'  collapse_ages(max_age = 85)
#' @export

collapse_ages <- function(.data, max_age = 100) {
  if(!inherits(.data, "vital")) {
    stop(".data needs to be a vital object")
  }
  colnames <- colnames(.data)
  attr_data <- attributes(.data)
  # Index variable
  index <- tsibble::index_var(.data)
  # Keys including age
  keys <- tsibble::key_vars(.data)
  age <- attr_data$agevar
  keys_noage <- keys[!(keys %in% c(age, "Age", "AgeGroup", "Age_Group"))]

  # Identify other columns
  pop <- attr_data$populationvar
  deaths <- attr_data$deathsvar
  births <- attr_data$birthsvar
  sex <- attr_data$sexvar
  rates <- find_measures(.data, c("mx", "mortality", "fx", "fertility", "rate"))

  # Store values for max age in case they are needed
  max_age_values <- .data[.data[[age]] == max_age,]

  # Compute death and birth counts if they are missing
  for(i in rates) {
    if(!is.null(pop)) {
      if(tolower(i) %in% c("mx", "mortality") && is.null(deaths)) {
        .data[[".Deaths"]] <- .data[[i]] * .data[[pop]]
        deaths <- ".Deaths"
      } else if(tolower(i) %in% c("fx", "fertility") && is.null(births)) {
        .data[[".Births"]] <- .data[[i]] * .data[[pop]]
        births <- ".Births"
      }
    }
  }

  # Collapse data by summing for max_age and above
  ages <- sort(unique(.data[[age]]))
  collapsed <- .data |>
    as_tibble() |>
    dplyr::group_by_at(c(index, keys_noage)) |>
    dplyr::reframe(dplyr::across(everything(),
      function(x) { collapse_age_vector(x, ages, max_age) } )) |>
    as_tsibble(index = index, key = all_of(c(keys_noage, age)))
  upper_ages <- collapsed[[age]] == max_age

  # Recompute rates where possible
  for(i in rates) {
    if(tolower(i) %in% c("mx", "mortality")) {
      counts <- deaths
    } else if(tolower(i) %in% c("fx", "fertility")) {
      counts <- births
    } else
      counts <- NULL
    if(!is.null(pop) & !is.null(counts)) {
        collapsed[[i]][upper_ages] <- collapsed[[counts]][upper_ages] / collapsed[[pop]][upper_ages]
    } else {
      warning("Cannot recompute rates for ", i, ". Using upper age value.")
      tmp <- max_age_values |>
        select(index, keys_noage, age, i)
      colnames(tmp)[colnames(tmp) == i] <- ".new_rate"
      collapsed <- collapsed |>
        left_join(tmp, by = c(index, keys_noage, age))
      collapsed[[i]] <- if_else(upper_ages, collapsed[[".new_rate"]], collapsed[[i]])
      collapsed[[".new_rate"]] <- NULL
    }
  }

  # Return result
  return(as_vital(collapsed, .age = age, .sex = sex,
    .deaths = deaths, .births = births, .population = pop,
    reorder = TRUE)[,colnames]
  )
}

collapse_age_vector <- function(x, ages, max_age) {
  if(is.numeric(x)) {
    # is it an age variable? Keep it as is
    if(is.constant(diff(x))) {
      out <- x[ages <= max_age]
    } else {
      # Sum upper group
      out <- c(x[ages < max_age], sum(x[ages >= max_age]))
    }
  } else if(is.character(x)) {
    # Probably AgeGroup. Add + to upper group
    out <- x[ages <= max_age]
    out[length(out)] <- paste0(out[length(out)],"+")
  } else if(is.logical(x)) {
    # Perhaps OpenInterval variable
    out <- c(x[ages < max_age], any(x[ages > max_age]))
  } else {
    # No idea what this is, but just truncate
    out <- x[ages <= max_age]
  }
  return(out)
}

is.constant <- function(x) {
  x <- as.numeric(x)
  y <- rep(x[1], length(x))
  return(isTRUE(all.equal(x, y)))
}
