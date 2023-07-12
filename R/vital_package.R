
#' @keywords internal
#' @import fabletools
#' @importFrom crayon bold red green blue col_align col_nchar make_style style
#' @importFrom dplyr mutate rename select arrange if_else full_join %>% transmute
#' @importFrom ggplot2 ggplot aes geom_line autoplot xlab
#' @importFrom grDevices rainbow
#' @importFrom purrr map map2 map_dfr map_chr map_lgl transpose possibly compose
#' @importFrom rlang %||% warn is_quosure quo_is_missing get_expr
#' @importFrom rlang enquo quo quo_is_null is_empty inform as_quosures empty_env
#' @importFrom rlang is_quosures eval_tidy := sym abort syms is_null list2 is_call
#' @importFrom rlang is_syntactic_literal is_symbol new_function set_names missing_arg
#' @importFrom rlang rep_along new_quosure
#' @importFrom stats na.omit ts residuals
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyselect eval_select everything
#' @importFrom tsibble as_tsibble group_by_key measured_vars n_keys
#' @importFrom tsibble key_data is_ordered is_regular
"_PACKAGE"

#' @export
ggplot2::autoplot

#' @export
generics::forecast
