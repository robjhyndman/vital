#' @keywords internal
#' @importFrom dplyr mutate rename select arrange if_else full_join
#' @importFrom ggplot2 ggplot aes geom_line autoplot xlab
#' @importFrom purrr map2 map_dfr
#' @importFrom stats na.omit ts
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather spread
#' @importFrom tidyselect everything
#' @importFrom tsibble as_tsibble
#' @importFrom grDevices rainbow
#' @importFrom generics forecast
#' @importFrom crayon bold red green blue col_align col_nchar make_style style
#' @importFrom rlang quo_is_null is_empty inform as_quosures empty_env is_quosures eval_tidy
"_PACKAGE"

#' @export
tsibble::as_tsibble

#' @export
ggplot2::autoplot

#' @export
generics::forecast
