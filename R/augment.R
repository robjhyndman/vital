#' @export
augment.mdl_vtl_df <- function(x, ...) {
  mbl_vars <- mable_vars(x)
  kv <- key_vars(x)
  agevar <- age_var(x[[mbl_vars[1]]][[1]]$data)
  index <- index(x[[mbl_vars[1]]][[1]]$data)
  x <- mutate(
    as_tibble(x),
    dplyr::across(all_of(mbl_vars), function(x) lapply(x, augment, ...))
  )
  x <- pivot_longer(
    x,
    all_of(mbl_vars),
    names_to = ".model",
    values_to = ".aug"
  )
  unnest_tsbl(x, ".aug", parent_key = c(kv, ".model")) |>
    as_tsibble(index = index, key = all_of(c(agevar, kv, ".model"))) |>
    as_vital(.age = agevar, reorder = TRUE)
}

#' @export
augment.mdl_vtl_ts <- function(x, ...) {
  x_response <- response(x)
  out <- x_response |>
    left_join(
      x$fit$fitted,
      by = c(age_var(x_response), index_var(x_response))
    )
  # Back transform fitted value
  fits <- as.list(out)[".fitted"]
  bt <- map(x$transformation, invert_transformation)
  fits <- map2(bt, fits, function(bt, fit) bt(fit))
  out[[".resid"]] <- out[[".response"]] - fits[[1]]
  out[[".fitted"]] <- fits[[1]]
  out
}

#' @export
fitted.mdl_vtl_df <- function(object, ...) {
  augment(object) |>
    transmute(.fitted)
}

#' @export
residuals.mdl_vtl_df <- function(
  object,
  type = c("innovation", "response"),
  ...
) {
  type <- match.arg(type)
  if (type == "innovation") {
    augment(object) |> transmute(.innov)
  } else {
    augment(object) |> transmute(.resid)
  }
}


#' @export
response.mdl_vtl_ts <- function(object, ...) {
  mv <- measured_vars(object$data)
  vvar <- vital_var_list(object$data)
  protected <- c(vvar$age, vvar$population, vvar$deaths, vvar$births)
  mv <- mv[!(mv %in% protected)]
  resp <- as.list(object$data)[mv]
  bt <- map(object$transformation, invert_transformation)
  resp <- map2(bt, resp, function(bt, fit) bt(fit))
  out <- object$data[c(index_var(object$data), vvar$age)]
  out[
    if (length(resp) == 1) {
      ".response"
    } else {
      mv
    }
  ] <- resp
  # Fix key
  as_vital(
    out,
    key = vvar$age,
    index = index_var(object$data),
    .age = vvar$age
  )
}
