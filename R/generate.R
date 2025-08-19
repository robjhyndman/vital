#' Generate responses from a mable
#'
#' Use a fitted model to simulate future data with similar
#' behaviour to the response.
#'
#' Innovations are sampled by the model's assumed error distribution.
#' If `bootstrap` is `TRUE`, innovations will be sampled from the model's residuals.
#'
#' @param x A mable.
#' @param new_data Future data needed for generation (should include the time index and exogenous regressors)
#' @param h The simulation horizon (can be used instead of `new_data` for regular time series with no exogenous regressors).
#' @param bootstrap If `TRUE`, then forecast distributions are computed using simulation with resampled errors.
#' @param times The number of replications.
#' @param ... Additional arguments
#' @author Rob J Hyndman and Mitchell O'Hara-Wild
#' @return A vital object with simulated values.
#' @rdname generate
#' @examples
#' norway_mortality |>
#'   model(lc = LC(Mortality)) |>
#'   generate(times = 3, bootstrap = TRUE)
#'
#' @export
generate.mdl_vtl_df <- function(
  x,
  new_data = NULL,
  h = NULL,
  bootstrap = FALSE,
  times = 1,
  ...
) {
  mdls <- mable_vars(x)
  if (!is.null(new_data)) {
    x <- bind_new_data(x, new_data)
  }
  kv <- c(key_vars(x), ".model")
  x <- tidyr::pivot_longer(
    as_tibble(x),
    all_of(mdls),
    names_to = ".model",
    values_to = ".sim"
  )

  # Evaluate simulations
  x[[".sim"]] <- map2(
    x[[".sim"]],
    x[["new_data"]] %||% rep_len(list(NULL), NROW(x)),
    generate,
    h = h,
    bootstrap = bootstrap,
    times = times,
    ...
  )
  x[["new_data"]] <- NULL
  agevar <- age_var(x$.sim[[1]])
  index <- index_var(x$.sim[[1]])

  unnest_tsbl(x, ".sim", parent_key = kv) |>
    as_tsibble(index = index, key = all_of(c(agevar, kv, ".rep"))) |>
    as_vital(.age = agevar, reorder = TRUE)
}

#' @export
generate.mdl_vtl_ts <- function(
  x,
  new_data = NULL,
  h = NULL,
  bootstrap = FALSE,
  times = 1,
  ...
) {
  if (is.null(new_data)) {
    new_data <- make_future_data(x$data, h)
  }
  if (is.null(new_data[[".rep"]])) {
    kv <- c(".rep", key_vars(new_data))
    idx <- index_var(new_data)
    intvl <- tsibble::interval(new_data)
    agevar <- age_var(new_data)
    new_data <- vctrs::vec_rbind(
      !!!set_names(rep(list(as_tibble(new_data)), times), seq_len(times)),
      .names_to = ".rep"
    )
    new_data <- build_tsibble(
      new_data,
      index = !!idx,
      key = !!kv,
      interval = intvl
    ) |>
      as_vital(.age = agevar)
  }
  # Compute specials with new_data
  x$model$stage <- "generate"
  x$model$add_data(new_data)
  specials <- tryCatch(
    parse_model_rhs(x$model),
    error = function(e) {
      abort(sprintf(
        "%s\n Unable to compute required variables from provided `new_data`.
Does your model require extra variables to produce simulations?",
        e$message
      ))
    },
    interrupt = function(e) {
      stop("Terminated by user", call. = FALSE)
    }
  )

  x$model$remove_data()
  x$model$stage <- NULL
  if (length(x$response) > 1) {
    abort("Generating paths from multivariate models is not yet supported.")
  }
  .sim <- generate(
    x[["fit"]],
    new_data = new_data,
    specials = specials,
    bootstrap = bootstrap,
    times = times,
    ...
  )[[".sim"]]

  # Back-transform forecast distributions
  bt <- map(x$transformation, function(x) {
    bt <- invert_transformation(x)
    env <- new_environment(new_data, get_env(bt))
    set_env(bt, env)
  })

  new_data[[".sim"]] <- bt[[1]](.sim)
  new_data
}

globalVariables(".sim")
