#' @export
augment.mdl_vtl_df <- function(x, ...) {
  mbl_vars <- mable_vars(x)
  kv <- key_vars(x)
  agevar <- attributes(x[[mbl_vars[1]]][[1]]$data)$agevar
  index <- index_var(x[[mbl_vars[1]]][[1]]$data)
  x <- mutate(
    as_tibble(x),
    dplyr::across(all_of(mbl_vars), function(x) lapply(x, augment, ...))
  )
  x <- pivot_longer(x, all_of(mbl_vars), names_to = ".model", values_to = ".aug")
  unnest_tsbl(x, ".aug", parent_key = c(kv, ".model")) |>
    as_tsibble(index = index, key = c(agevar, kv, ".model")) |>
    as_vital(.age = agevar, reorder = TRUE)
}

#' @export
augment.mdl_vtl_ts <- function(x, ...) {
  response(x) |>
    mutate(
      .fitted = x$fit$fitted[[".fitted"]],
      .resid = x$fit$fitted[[".resid"]],
      .innov = x$fit$fitted[[".innov"]]
    )
}

#' @export
fitted.mdl_vtl_df <- function(object, ...) {
  augment(object) |>
    transmute(.fitted)
}

#' @export
residuals.mdl_vtl_df <- function(object, type = c("innovation", "response"), ...) {
  type <- match.arg(type)
  if(type == "innovation") {
    augment(object) |> transmute(.innov)
  } else {
    augment(object) |> transmute(.resid)
  }
}
