# Non-exported functions borrowed from fabletools

is.formula <- function(x) {
  inherits(x, "formula")
}

traverse <- function(
  x,
  .f = list,
  .g = identity,
  .h = identity,
  base = function(.x) is_syntactic_literal(.x) || is_symbol(.x)
) {
  if (base(x)) {
    return(.h(x))
  }
  .f(
    lapply(.g(x), traverse, .f = .f, .g = .g, .h = .h, base = base),
    .h(x)
  )
}

traverse_call <- function(
  x,
  .f = function(.x, .y) {
    map(.x, quo_get_expr) %>%
      as.call() %>%
      new_quosure(env = get_env(.x[[1]]))
  },
  .g = function(.x) {
    .x %>%
      get_expr() %>%
      as.list() %>%
      map(new_quosure, env = get_env(.x))
  },
  .h = identity,
  base = function(.x) !quo_is_call(.x)
) {
  x <- enquo(x)
  traverse(x, .f = .f, .g = .g, .h = .h, base = base)
}

names_no_null <- function(x) {
  names(x) %||% rep_along(x, "")
}

guess_response <- function(.data) {
  all_vars <- custom_error(
    measured_vars,
    "This model function does not support automatic selection of response variables. Please specify this in the model formula."
  )(.data)
  if (length(all_vars) != 1) {
    abort(
      "Could not automatically determine the response variable, please provide the response variable in the model specification"
    )
  }
  out <- sym(all_vars[[1]])
  inform(sprintf(
    "Model not specified, defaulting to automatic modelling of the `%s` variable. Override this using the model formula.",
    expr_name(out)
  ))
  out
}

custom_error <- function(.f, error) {
  force(error)
  function(...) {
    res <- capture_error(.f(...))
    if (!is.null(res$error)) {
      abort(error)
    }
    res$result
  }
}

merge_named_list <- function(...) {
  flat <- flatten(list(...))
  nm <- names_no_null(flat)
  map(split(flat, nm), function(x) flatten(unname(x)))
}

capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
  tryCatch(
    list(result = code, error = NULL),
    error = function(e) {
      if (!quiet) {
        message("Error: ", e$message)
      }
      list(result = otherwise, error = e)
    },
    interrupt = function(e) {
      stop("Terminated by user", call. = FALSE)
    }
  )
}

unnest_tbl <- function(.data, tbl_col, .sep = NULL) {
  row_indices <- rep.int(
    seq_len(NROW(.data)),
    map_int(
      .data[[tbl_col[[1]]]],
      NROW
    )
  )
  nested_cols <- map(tbl_col, function(x) {
    lst_col <- .data[[x]]
    if (is.data.frame(lst_col[[1]])) {
      lst_col <- map(lst_col, as_tibble)
      vctrs::vec_rbind(!!!lst_col)
    } else {
      unlist(lst_col)
    }
  })
  if (!is.null(.sep)) {
    nested_cols <- map2(nested_cols, tbl_col, function(x, nm) {
      set_names(x, paste(nm, colnames(x), sep = .sep))
    })
  }
  is_df <- map_lgl(nested_cols, is.data.frame)
  vctrs::vec_cbind(
    .data[
      row_indices,
      setdiff(
        names(.data),
        tbl_col
      ),
      drop = FALSE
    ],
    !!!set_names(
      nested_cols[!is_df],
      tbl_col[!is_df]
    ),
    !!!nested_cols[is_df]
  )
}

unnest_tsbl <- function(.data, tsbl_col, parent_key = NULL, interval = NULL) {
  tsbl <- .data[[tsbl_col]][[1L]]
  if (!is_tsibble(tsbl)) {
    abort("Unnested column is not a tsibble object.")
  }
  idx <- index(tsbl)
  idx_chr <- as_string(idx)
  key <- c(parent_key, key_vars(tsbl))
  .data <- unnest_tbl(.data, tsbl_col)
  build_tsibble(
    .data,
    key = !!key,
    index = !!idx,
    index2 = !!index2(tsbl),
    ordered = is_ordered(tsbl),
    interval = interval %||% tsibble::interval(tsbl),
    validate = FALSE
  )
}

bind_new_data <- function(object, new_data) {
  if (inherits(new_data, "list")) {
    scenario_nm <- attr(new_data, "names_to") %||% ".scenario"
    new_data <- vctrs::vec_rbind(
      !!!map(
        new_data,
        compose(
          as_tibble,
          bind_new_data
        ),
        object = object
      ),
      .names_to = scenario_nm
    )
    return(build_mable(
      new_data,
      key = c(scenario_nm, key_vars(object)),
      model = mable_vars(object)
    ))
  }
  if (!is.data.frame(new_data)) {
    abort(sprintf(
      "`new_data` requires a data frame. Perhaps you intended to specify the forecast horizon? If so, use `h = %s`.",
      deparse(new_data)
    ))
  }
  keys <- key_vars(new_data)
  agevar <- age_var(new_data)
  keys_noage <- keys[keys != agevar]
  if (!identical(key_vars(object), keys_noage)) {
    abort("Provided data contains a different key structure to the models.")
  }
  new_data <- nest_keys(new_data, "new_data")
  if (length(key_vars(object)) > 0) {
    attr_object <- attributes(object)
    object <- left_join(
      as_tibble(object),
      as_tibble(new_data),
      by = key_vars(object)
    )
    attributes(object) <- attr_object
    colnames(object)[NCOL(object)] <- "new_data"
    no_new_data <- map_lgl(object[["new_data"]], is_null)
    if (any(no_new_data)) {
      object[["new_data"]][no_new_data] <- rep(
        list(new_data[["new_data"]][[1]][0, ]),
        sum(no_new_data)
      )
    }
  } else {
    object[["new_data"]] <- new_data[["new_data"]]
  }
  object
}
build_mable <- function(x, key = NULL, key_data = NULL, model = NULL) {
  model <- names(tidyselect::eval_select(enquo(model), data = x))
  if (
    length(
      resp_var <- unique(map(x[model], function(mdl) response_vars(mdl[[1]])))
    ) >
      1
  ) {
    abort("A mable can only contain models with the same response variable(s).")
  }
  if (length(resp_var) == 0) {
    abort("A mable must contain at least one model.")
  }
  if (!is_null(key_data)) {
    assert_key_data(key_data)
    key <- utils::head(names(key_data), -1L)
  } else {
    key <- names(tidyselect::eval_select(enquo(key), data = x))
    key_data <- group_data(group_by(x, !!!syms(key)))
  }
  if (any(lengths(key_data[[length(key_data)]]) > 1)) {
    abort(
      "The result is not a valid mable. The key variables must uniquely identify each row."
    )
  }
  build_mable_meta(x, key_data, model, response = resp_var[[1]])
}

build_mable_meta <- function(x, key_data, model, response) {
  tsibble::new_tsibble(
    x,
    key = key_data,
    model = model,
    response = response,
    nrow = NROW(x),
    class = "mdl_df",
    subclass = "mdl_df"
  )
}
assert_key_data <- function(x) {
  nc <- NCOL(x)
  if (
    is_false(
      is.data.frame(x) &&
        nc > 0 &&
        is.list(x[[nc]]) &&
        names(x)[[nc]] == ".rows"
    )
  ) {
    abort(
      "The `key` attribute must be a data frame with its last column called `.rows`."
    )
  }
}

build_fable <- function(x, response, distribution) {
  response <- eval_tidy(enquo(response))
  distribution <- names(x)[tidyselect::eval_select(
    enquo(distribution),
    x
  )]
  if (is_grouped_ts(x)) {
    fbl <- structure(
      x,
      class = c(
        "grouped_fbl",
        "grouped_ts",
        "grouped_df",
        "fbl_ts",
        "tbl_ts",
        "tbl_df",
        "tbl",
        "data.frame"
      ),
      response = response,
      dist = distribution,
      model_cn = ".model"
    )
  } else {
    fbl <- tsibble::new_tsibble(
      x,
      response = response,
      dist = distribution,
      model_cn = ".model",
      class = "fbl_ts"
    )
  }
  if (is.null(dimnames(fbl[[distribution]]))) {
    warn(
      "The dimnames of the fable's distribution are missing and have been set to match the response variables."
    )
    dimnames(fbl[[distribution]]) <- response
  }
  if (!identical(response, dimnames(fbl[[distribution]]))) {
    dimnames(fbl[[distribution]]) <- response
  }
  validate_fable(fbl)
  fbl
}
validate_fable <- function(fbl) {
  stopifnot(inherits(fbl, "fbl_ts"))
  chr_dist <- distribution_var(fbl)
  if (!(chr_dist %in% names(fbl))) {
    abort(sprintf(
      "Could not find distribution variable `%s` in the fable. A fable must contain a distribution, if you want to remove it convert to a tsibble with `as_tsibble()`.",
      chr_dist
    ))
  }
  vctrs::vec_assert(
    fbl[[chr_dist]],
    distributional::new_dist(dimnames = response_vars(fbl))
  )
}

dist_types <- function(dist) {
  map_chr(vctrs::vec_data(dist), function(x) class(x)[1])
}
