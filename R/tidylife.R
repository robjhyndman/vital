#' @keywords internal
#' @importFrom dplyr mutate rename select arrange
#' @importFrom ggplot2 ggplot aes geom_line autoplot xlab
#' @importFrom purrr map2 map_dfr
#' @importFrom stats na.omit
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather spread
#' @importFrom tidyselect everything
#' @importFrom tsibble as_tsibble
#' @importFrom curl new_handle handle_setopt
#' @importFrom grDevices rainbow
#' @importFrom patchwork plot_spacer plot_annotation wrap_plots
#' @import rlang vctrs
"_PACKAGE"

.hmd <- new.env(parent = emptyenv())

#' @export
tsibble::as_tsibble

#' @export
ggplot2::autoplot
