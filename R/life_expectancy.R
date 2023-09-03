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
#' # Compute Victorian life expectancy for females over time
#' aus_mortality |>
#'   dplyr::filter(Code == "VIC", Sex == "female") |>
#'   life_expectancy()
#' @author Rob J Hyndman
#' @export

life_expectancy <- function(.data, from_age = 0, mortality) {
  life_table(.data = .data, mortality = mortality) |>
    # Keep only relevant ages
    dplyr::filter(Age %in% from_age) |>
    # Keep only ex column plus index and keys
    dplyr::select(-mx, -qx, -lx, -dx, -Lx, -Tx)
}

utils::globalVariables(c("mx", "qx", "lx", "dx", "Lx", "Tx", "Age"))
