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
      .resid = x$fit$fitted[[".resid"]]
    )
}


#' @export
fitted.mdl_vtl_ts <- function(object, h = 1, ...) {
  bt <- map(object$transformation, invert_transformation)
  fits <- if (h == 1) {
    fitted(object$fit, ...)
  } else {
    hfitted(object, h = h, ...)
  }
  if (h == 1) {
    fits <- as.matrix(fits)
    fits <- map2(bt, split(fits, col(fits)), function(bt,
                                                      fit) {
      bt(fit)
    })
  }
  nm <- if (length(fits) == 1) {
    ".fitted"
  } else {
    map_chr(object$response, expr_name)
  }
  agevar <- attributes(object$data)$agevar
  index <- index_var(object$data)
  out <- object$data[c(index, agevar)]
  out[nm] <- fits
  as_tsibble(out, index = index, key = agevar) |> as_vital(age = agevar)
}

#' @export
fitted.mdl_vtl_df <- function(object, ...) {
  mbl_vars <- mable_vars(object)
  kv <- key_vars(object)
  agevar <- attributes(object[[mbl_vars[1]]][[1]]$data)$agevar
  index <- index_var(object[[mbl_vars[1]]][[1]]$data)
  object <- mutate(as_tibble(object), dplyr::across(
    all_of(mbl_vars),
    function(x) lapply(x, fitted, ...)
  ))
  object <- pivot_longer(object, mbl_vars,
                         names_to = ".model",
                         values_to = ".fitted"
  )

  unnest_tbl(object, ".fitted") |>
    as_tsibble(index = index, key = c(agevar, kv, ".model")) |>
    as_vital(.age = agevar, reorder = TRUE)
}


#' @export
residuals.mdl_vtl_ts <- function(object, type = "innovation", ...) {
  if (type == "response") {
    .resid <- response(object)
    .fits <- fitted(object)
    .resid <- as.matrix(.resid[measured_vars(.resid)]) -
      as.matrix(.fits[measured_vars(.fits)])
  } else {
    .resid <- residuals(object$fit, type = type, ...)
    if (is.null(.resid)) {
      if (type == "innovation") {
        .resid <- response(object)
        .resid <- map2(object$transformation, .resid[measured_vars(.resid)], calc)
        .fits <- fitted(object)
        .fits <- map2(object$transformation, .fits[measured_vars(.fits)],calc)
        .resid <- do.call(cbind, .resid) - do.call(cbind,as.matrix(.fits))
      } else {
        warn(sprintf(
          "Residuals of type `%s` are not supported for %s models.\nDefaulting to `type=\"response\"`",
          type, model_sum(object)
        ))
        return(residuals(object, type = "response", ...))
      }
    }
  }
  .resid <- as.matrix(.resid)
  .resid <- split(.resid, col(.resid))
  nm <- if (length(.resid) == 1) {
    ".resid"
  } else {
    map_chr(object$response, expr_name)
  }
  agevar <- attributes(object$data)$agevar
  index <- index_var(object$data)
  out <- object$data[c(index, agevar)]
  out[nm] <- .resid
  as_tsibble(out, index = index, key = agevar) |> as_vital(age = agevar)
}

#' @export
residuals.mdl_vtl_df <- function(object, ...) {
  mbl_vars <- mable_vars(object)
  kv <- key_vars(object)
  agevar <- attributes(object[[mbl_vars[1]]][[1]]$data)$agevar
  index <- index_var(object[[mbl_vars[1]]][[1]]$data)
  object <- mutate(as_tibble(object), dplyr::across(
    all_of(mbl_vars),
    function(x) lapply(x, residuals, ...)
  ))
  object <- pivot_longer(object, mbl_vars,
                         names_to = ".model",
                         values_to = ".resid"
  )
  unnest_tbl(object, ".resid") |>
    as_tsibble(index = index, key = c(agevar, kv, ".model")) |>
    as_vital(.age = agevar, reorder = TRUE)
}
