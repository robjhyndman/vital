
#' @keywords internal
#' @import fabletools
#' @importFrom crayon bold red green blue col_align col_nchar make_style style
#' @importFrom dplyr mutate rename select arrange if_else full_join %>% transmute
#' @importFrom dplyr ungroup group_by summarise left_join across
#' @importFrom ggplot2 ggplot aes geom_line autoplot xlab
#' @importFrom grDevices rainbow
#' @importFrom purrr map map2 map_dfr map_chr map_lgl transpose possibly compose
#' @importFrom purrr map_dbl map_int
#' @importFrom rlang %||% warn is_quosure quo_is_missing get_expr as_name flatten
#' @importFrom rlang enquo quo quo_is_null is_empty inform as_quosures empty_env
#' @importFrom rlang is_quosures eval_tidy := sym abort syms is_null list2 is_call
#' @importFrom rlang is_syntactic_literal is_symbol new_function set_names missing_arg
#' @importFrom rlang call2 get_env new_formula expr_name new_environment enexpr
#' @importFrom rlang quo_get_expr quo_is_call caller_env as_string
#' @importFrom rlang rep_along new_quosure expr call_name as_label
#' @importFrom stats na.omit ts residuals fitted
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyselect eval_select everything all_of
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tsibble as_tsibble group_by_key measured_vars n_keys tsibble
#' @importFrom tsibble key_data is_ordered is_regular index index_var key_vars
#' @importFrom tsibble is_tsibble build_tsibble index2
#' @aliases NULL vital-package
#' @examples
#' # create a vital with only age as a key
#' vital(
#'   year = rep(2010:2015, 100),
#'   age = rep(0:99, each = 6),
#'   mx = runif(600, 0, 1),
#'   index = year,
#'   key = age,
#'   .age = age
#' )
"_PACKAGE"

#' @export
ggplot2::autoplot

#' @export
generics::forecast
