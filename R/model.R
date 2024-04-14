#' Estimate models for vital data
#'
#' Trains specified model definition(s) on a dataset. This function will
#' estimate the a set of model definitions (passed via `...`) to each series
#' within `.data` (as identified by the key structure). The result will be a
#' mable (a model table), which neatly stores the estimated models in a tabular
#' structure. Rows of the data identify different series within the data, and
#' each model column contains all models from that model definition. Each cell
#' in the mable identifies a single model.
#'
#' @param .data A vital object including an age variable.
#' @param ... Definitions for the models to be used. All models must share the
#' same response variable.
#'
#' @rdname model
#' @author Rob J Hyndman and Mitchell O'Hara-Wild
#'
#' @param .safely If a model encounters an error, rather than aborting the process
#' a [NULL model][null_model()] will be returned instead. This allows for an error
#' to occur when computing many models, without losing the results of the successful models.
#'
#' @section Parallel:
#'
#' It is possible to estimate models in parallel using the
#' [future](https://cran.r-project.org/package=future) package. By specifying a
#' [`future::plan()`] before estimating the models, they will be computed
#' according to that plan.
#'
#' @section Progress:
#'
#' Progress on model estimation can be obtained by wrapping the code with
#' `progressr::with_progress()`. Further customisation on how progress is
#' reported can be controlled using the `progressr` package.
#' @return A mable containing the fitted models.
#' @examples
#' aus_mortality |>
#'   dplyr::filter(State == "Victoria", Sex == "female") |>
#'   model(
#'     naive = FNAIVE(Mortality),
#'     mean = FMEAN(Mortality)
#'   )
#' @export
model.vital <- function(.data, ..., .safely = TRUE) {
  nm <- purrr::map(rlang::enexprs(...), rlang::expr_text)
  models <- rlang::dots_list(...)

  if (length(models) == 0) {
    abort("At least one model must be specified.")
  }
  if (!all(is_mdl <- purrr::map_lgl(models, inherits, "mdl_defn"))) {
    abort(sprintf("Model definition(s) incorrectly created: %s
Check that specified model(s) are model definitions.", nm[which(!is_mdl)[1]]))
  }

  # Keys including age
  keys <- tsibble::key_vars(.data)
  agevar <- attributes(.data)$agevar
  sexvar <- attributes(.data)$sexvar
  # Drop Age as a key
  kv <- keys[!(keys %in% c(agevar, "Age", "AgeGroup"))]
  # Make sure Sex is first key (so it can be identified inside estimate_progress)
  if(!is.null(sexvar)) {
    if(!(sexvar %in% kv)) {
      stop("Sex should be one of the keys")
    }
    kv <- c(sexvar, kv[kv != sexvar])
  }
  n_ages <- length(unique(.data[[agevar]]))
  num_key <- n_keys(.data) / n_ages
  num_mdl <- length(models)
  num_est <- num_mdl * num_key
  p <- progressr::progressor(num_est)

  .data <- nest_keys(.data, "lst_data")

  if (.safely) {
    estimate <- function(dt, mdl, sex) {
      out <- purrr::safely(estimate.vital)(dt, mdl, sex)
      if (is.null(out$result)) {
        f <- quo(!!mdl$formula)
        f <- rlang::set_env(f, mdl$env)
        out$result <- estimate.vital(dt, null_model(!!f))
      }
      out
    }
  }

  estimate_progress <- function(dt, keys, mdl) {
    if(!is.null(sexvar)) {
      sex <- keys[1]
    } else {
      sex <- NULL
    }
    if(!is.null(mdl$extra$coherent)) {
      if("geometric_mean" %in% keys & mdl$extra$coherent) {
        # No need to make the model stationary
        mdl$extra$coherent <- FALSE
      } else {
        # Make model stationary
        mdl$extra$coherent <- TRUE
      }
    }
    out <- estimate(dt, mdl, sex)
    p()
    out
  }

  if (is_attached("package:future")) {
    require_package("future.apply")
    stop("Not implemented")
    eval_models <- function(models, lst_data, keyvars) {
      out <- future.apply::future_mapply(
        rep(lst_data, length(models)),
        rep(sex, length(models)),
        rep(models, each = length(lst_data)),
        FUN = estimate_progress,
        SIMPLIFY = FALSE,
        future.globals = FALSE
      )
      unname(split(out, rep(seq_len(num_mdl), each = num_key)))
    }
  } else {
    eval_models <- function(models, lst_data, keyvars) {
      vars <- colnames(keyvars)
      keyvars <- keyvars |> t() |> as.data.frame() |> as_tibble()
      purrr::map(models, function(model) {
        purrr::map2(lst_data, keyvars, estimate_progress, model)
      })
    }
  }
  fits <- eval_models(models, .data[["lst_data"]], .data[,kv])
  names(fits) <- ifelse(nchar(names(models)), names(models), nm)

  # Report errors if estimated safely
  if (.safely) {
    fits <- purrr::imap(fits, function(x, nm) {
      err <- purrr::map_lgl(x, function(x) !is.null(x[["error"]]))
      if ((tot_err <- sum(err)) > 0) {
        err_msg <- table(purrr::map_chr(x[err], function(x) x[["error"]][["message"]]))
        rlang::warn(
          sprintf(
            "%i error%s encountered for %s\n%s\n",
            tot_err,
            if (tot_err > 1) sprintf("s (%i unique)", length(err_msg)) else "",
            nm,
            paste0("[", err_msg, "] ", names(err_msg), collapse = "\n")
          )
        )
      }
      purrr::map(x, function(x) x[["result"]])
    })
  }

  fits <- purrr::map(fits, list_of_models)

  final <- .data %>%
    transmute(
      !!!syms(kv),
      !!!fits
    ) %>%
    fabletools::as_mable(key = !!kv, model = names(fits))
  class(final) <- c("mdl_vtl_df", class(final))
  return(final)
}

require_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    abort(sprintf("The `%s` package must be installed to use this functionality. It can be installed with install.packages(\"%s\")", pkg, pkg))
  }
}

nest_keys <- function(.data, nm = "data") {
  # Keys including age
  keys <- tsibble::key_vars(.data)
  agevar <- attributes(.data)$agevar
  # Drop Age as a key
  keys_noage <- keys[!(keys %in% c(agevar, "Age", "AgeGroup"))]

  out <- key_data(.data) |>
    tidyr::unnest(.rows) |>
    select(all_of(c(keys_noage, ".rows"))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys_noage))) |>
    tidyr::nest() |>
    mutate(data = list(unlist(data))) |>
    ungroup() |>
    unclass()

  row_indices <- out[[length(out)]]
  out[[length(out)]] <- NULL
  col_nest <- -match(keys_noage, colnames(.data))
  if (is_empty(col_nest)) {
    col_nest <- NULL
  }
  idx <- tsibble::index_var(.data)
  idx2 <- tsibble::index2_var(.data)
  ordered <- is_ordered(.data)
  regular <- is_regular(.data)
  attr_data <- attributes(.data)
  out[[nm]] <- purrr::map(row_indices, function(x, i, j) {
    out <- if (is.null(j)) x[i, ] else x[i, j]
    tsibble::build_tsibble_meta(
      out,
      key_data = tibble::as_tibble(list(.rows = list(seq_along(i)))),
      index = idx, index2 = idx2, ordered = ordered,
      interval = if (length(i) > 1 && regular) tsibble::interval_pull(out[[idx]]) else tsibble::interval(.data)
    ) |>
      as_vital(.age = attr_data$agevar, .sex = attr_data$sexvar,
               .births = attr_data$birthsvar, .deaths = attr_data$deathsvar,
               .population = attr_data$populationvar)
  }, x = tibble::as_tibble(.data), j = col_nest)
  tibble::as_tibble(out)
}

list_of_models <- function(x = list()) {
  vctrs::new_vctr(x, class = "lst_mdl")
}

#' @export
estimate.vital <- function(.data, .model, sex, ...) {
  if (!inherits(.model, "mdl_defn")) {
    abort("Model definition incorrectly created. Check that specified model(s) are model definitions.")
  }
  .model$stage <- "estimate"
  .model$add_data(.data)
  validate_formula(.model, .data)
  parsed <- parse_model(.model)
  .dt_attr <- attributes(.data)
  agevar <- .dt_attr$agevar
  popvar <- .dt_attr$populationvar
  deathsvar <- .dt_attr$deathsvar
  birthsvar <- .dt_attr$birthsvar
  age <- .data[[agevar]]
  if(!is.null(popvar))
    pop <- .data[[popvar]]
  if(!is.null(deathsvar))
    deaths <- .data[[deathsvar]]
  if(!is.null(birthsvar))
    births <- .data[[birthsvar]]
  resp <- map(parsed$expressions, eval_tidy,
              data = .data,
              env = .model$specials
  )
  .data <- unclass(.data)[index_var(.data)]
  .data[map_chr(parsed$expressions, rlang::expr_name)] <- resp
  .data[[agevar]] <- age
  if(!is.null(popvar))
    .data[[popvar]] <- pop
  if(!is.null(deathsvar))
    .data[[deathsvar]] <- deaths
  if(!is.null(birthsvar))
    .data[[birthsvar]] <- births
  attributes(.data) <- c(attributes(.data), .dt_attr[setdiff(
    names(.dt_attr),
    names(attributes(.data))
  )])
  fit <- eval_tidy(
    expr(.model$train(.data = .data, sex=sex, specials = parsed$specials, !!!.model$extra))
  )
  .model$remove_data()
  .model$stage <- NULL
  new_model(fit, .model, .data, parsed$response, parsed$transformation)
}

# Same as fabletools function but with different class
new_model <- function(fit = NULL, model, data, response, transformation) {
  structure(list(
    fit = fit, model = model, data = data, response = response,
    transformation = transformation
  ), class = c("mdl_vtl_ts", "mdl_ts"))
}

globalVariables(c(".rows", "data", "calc", "sex"))
