# Function to collapse upper ages into a single age group

#' Collapse upper ages into a single age group. Counts are summed while
#' rates are recomputed.
#'
#' @param .data A tsibble including an age variable and a variable containing mortality rates.
#' @param max_age Maximum age to include in the collapsed age group.
#'
#' @return A tsibble with the same variables as `.data` but with the upper ages collapsed into a single age group.
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

  # Collapse data by summing above max_age
  ages <- sort(unique(.data[[age]]))
  collapsed <- .data |>
    as_tibble() |>
    dplyr::group_by_at(dplyr::vars(c(index, keys_noage))) |>
    dplyr::reframe(dplyr::across(dplyr::where(is.numeric),
      function(x) { collapse_age_vector(x, ages, max_age) } )) |>
    as_tsibble(index = index, key = keys)

  # Recompute rates
  for(i in rates) {
    if(tolower(i) %in% c("mx", "mortality")) {
      if(length(deaths) == 0L | length(pop) == 0L) {
        warning(paste(i, "not recomputed"))
      } else {
        collapsed[[i]] <- collapsed[[deaths]] / collapsed[[pop]]
      }
    } else if(tolower(i) %in% c("fx", "fertility")) {
      if(length(births) == 0L | length(pop) == 0L) {
        warning(paste(i, "not recomputed"))
      } else {
        collapsed[[i]] <- collapsed[[births]] / collapsed[[pop]]
      }
    }
  }

  # Return result
  return(as_vital(collapsed, age = age, sex = sex,
    deaths = deaths, births = births, population = pop)[,colnames]
  )
}

collapse_age_vector <- function(x, ages, max_age) {
  # is it an age variable?
  if(is.constant(diff(x))) {
    x[ages <= max_age]
  } else {
    c(x[ages < max_age], sum(x[ages >= max_age]))
  }
}

is.constant <- function(x) {
  x <- as.numeric(x)
  y <- rep(x[1], length(x))
  return(isTRUE(all.equal(x, y)))
}
