#' Compute life expectancy from age-specific mortality rates
#'
#' Returns remaining life expectancy at a given age (0 by default).
#'
#' @param .data A vital object including an age variable and a variable containing mortality rates.
#' @param from_age Age at which life expectancy to be calculated. Either a scalar or a vector of ages.
#' @param mortality Variable in `.data` containing Mortality rates (mx). If omitted, the variable with name  `mx`, `Mortality` or `Rate` will be used (not case sensitive).
#'
#' @return A `vital` object with life expectancy in column `ex`.
#' @rdname life_expectancy
#' @seealso [life_table()]
#'
#' @references Chiang CL. (1984) *The life table and its applications*.
#' Robert E Krieger Publishing Company: Malabar.
#' @references Keyfitz, N, and Caswell, H. (2005) *Applied Mathematical Demography*,
#' Springer-Verlag: New York.
#' @references Preston, S.H., Heuveline, P., and Guillot, M. (2001)
#' *Demography: measuring and modeling population processes*. Blackwell
#'
#' @examples
#' # Compute Norwegian life expectancy for females over time
#' norway_mortality |>
#'   dplyr::filter(Sex == "Female") |>
#'   life_expectancy()
#' @author Rob J Hyndman
#' @export

life_expectancy <- function(.data, from_age = 0, mortality) {
  lt_out <- life_table(.data = .data, mortality = {{ mortality }})
  age <- age_var(lt_out)
  missing_ages <- setdiff(from_age, lt_out[[age]])
  if (length(missing_ages) > 0L) {
    warning(
      "Ages not in the data are ignored: ",
      paste(missing_ages, collapse = ", "),
      call. = FALSE
    )
  }
  lt_out |>
    # Keep only relevant ages
    dplyr::filter(.data[[age]] %in% from_age) |>
    # Keep only ex column plus index and keys
    dplyr::select(all_of(c(tsibble::index_var(lt_out), tsibble::key_vars(lt_out), "ex")))
}

utils::globalVariables(c("Lx", "Tx", "Age"))
