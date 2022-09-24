#' @keywords internal
#' @importFrom dplyr mutate rename select arrange
#' @importFrom purrr map2 map_dfr
#' @importFrom rlang syms
#' @importFrom stats na.omit
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather spread
#' @importFrom tidyselect everything
#' @importFrom tsibble as_tsibble
#' @importFrom curl new_handle handle_setopt
"_PACKAGE"

.hmd <- new.env(parent = emptyenv())

#' @export
tsibble::as_tsibble
