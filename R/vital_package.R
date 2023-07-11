
#' @keywords internal
#' @importFrom crayon bold red green blue col_align col_nchar make_style style
#' @importFrom dplyr mutate rename select arrange if_else full_join %>% transmute
#' @importFrom fabletools interpolate tidy new_model_class new_model_definition
#' @importFrom fabletools model model_sum refit report forecast generate glance
#' @importFrom fabletools null_model
#' @importFrom ggplot2 ggplot aes geom_line autoplot xlab
#' @importFrom grDevices rainbow
#' @importFrom purrr map map2 map_dfr transpose
#' @importFrom rlang %||% warn
#' @importFrom rlang enquo quo quo_is_null is_empty inform as_quosures empty_env
#' @importFrom rlang is_quosures eval_tidy := sym abort syms is_null
#' @importFrom stats na.omit ts residuals
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyselect everything
#' @importFrom tsibble as_tsibble group_by_key measured_vars n_keys
#' @importFrom tsibble key_data is_ordered is_regular
"_PACKAGE"

#' @export
ggplot2::autoplot

#' @export
generics::forecast
