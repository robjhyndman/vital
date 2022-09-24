#' Compute life expectancy from mortality rates
#'
#' Returns remaining life expectancy at a given age (0 by default).
#'
#' @param .data A tsibble including an age variable and a variable containing mortality rates.
#' @param from_age Age at which life expectancy to be calculated. Either a scalar or a vector of ages.
#' @param age Variable in `.data` containing start year of age intervals. If omitted, the variable with name `Age` or `Age_group` will be used (not case sensitive).
#' @param sex Optional variable in `.data` containing sex information. If omitted, the variable with name `Sex` or `Group` will be used (not case sensitive).
#' @param mortality Variable in `.data` containing Mortality rates (mx). If omitted, the variable with name  `mx`, `Mortality` or `Rate` will be used (not case sensitive).
#'
#' @return A tsibble object with life expectancy in column `ex`.
#' @rdname life_expectancy
#' @seealso life_table
#'
#' @references Chiang CL. (1984) \emph{The life table and its applications}. Robert E Krieger Publishing Company: Malabar.
#' @references Keyfitz, N, and Caswell, H. (2005) \emph{Applied mathematical demography}, Springer-Verlag: New York.
#' @references Preston, S.H., Heuveline, P., and Guillot, M. (2001) \emph{Demography: measuring and modeling population processes}. Blackwell
#'
#' @examples
#' # Compute Australia life expectancy for females over time
#' library(dplyr)
#' aus_mortality |>
#'   filter(Code=="AUS", Sex=="female") |>
#'   life_expectancy()
#' @export

life_expectancy <- function(.data, from_age=0, age, sex, mortality) {
  life_table(.data=.data, age=age, sex=sex, mortality = mortality) |>
    # Keep only relevant ages
    filter(Age %in% from_age) |>
    # Keep only ex column plus index and keys
    dplyr::select(-mx,-qx,-lx,-dx,-Lx,-Tx)
}
