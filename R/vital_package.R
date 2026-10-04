#' @keywords internal
#' @import fabletools
#' @importFrom dplyr mutate rename select arrange if_else %>% transmute
#' @importFrom dplyr ungroup group_by summarise left_join across pull
#' @importFrom dplyr group_by_drop_default
#' @importFrom ggplot2 autoplot
#' @importFrom grDevices rainbow
#' @importFrom purrr map map2 map_chr map_lgl possibly compose
#' @importFrom purrr map_dbl map_int
#' @importFrom rlang %||% %@% warn is_quosure quo_is_missing get_expr as_name flatten
#' @importFrom rlang enquo quo quo_is_null is_empty inform exec
#' @importFrom rlang is_quosures eval_tidy := sym abort syms is_null list2 is_call
#' @importFrom rlang is_syntactic_literal is_symbol new_function set_names missing_arg
#' @importFrom rlang call2 get_env new_formula expr_name new_environment enexpr set_env
#' @importFrom rlang quo_get_expr quo_is_call caller_env as_string quo_name
#' @importFrom rlang rep_along new_quosure expr call_name as_label call_args
#' @importFrom rlang exprs expr_text
#' @importFrom stats na.omit residuals fitted var time predict sd
#' @importFrom stats as.formula
#' @importFrom tibble as_tibble tibble tbl_sum
#' @importFrom tidyselect eval_select everything all_of
#' @importFrom tidyr pivot_longer
#' @importFrom tsibble as_tsibble measured_vars n_keys tsibble
#' @importFrom tsibble key_data is_ordered is_regular index index_var key_vars
#' @importFrom tsibble is_tsibble build_tsibble index2 is_grouped_ts
#' @importFrom utils head tail
#' @aliases NULL vital-package
"_PACKAGE"

#' @export
ggplot2::autoplot

#' @export
fabletools::forecast

#' @export
fabletools::generate

#' @export
fabletools::model

#' @export
fabletools::report

#' @export
fabletools::interpolate

#' @export
fabletools::tidy

#' @export
fabletools::augment

#' @export
fabletools::estimate

#' @export
fabletools::glance
