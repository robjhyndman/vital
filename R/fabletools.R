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
  if (!is.data.frame(new_data)) {
    if (!(is.numeric(new_data) && length(new_data) == 1L)) {
      abort("`new_data` requires a data frame.")
    }
    abort(sprintf(
      "`new_data` requires a data frame. Perhaps you intended to specify the forecast horizon? If so, use `h = %s`.",
      deparse(new_data)
    ))
  }
  if (!tsibble::is_tsibble(new_data)) {
    abort("`new_data` must be a vital object or tsibble.")
  }
  # Take the vital variables of a tsibble (e.g. from tsibble::new_data()) from
  # the data used to train the models
  if (is.null(age_var(new_data))) {
    vvar <- mable_vital_vars(object)
    if (!(vvar$age %in% names(new_data))) {
      abort(sprintf("`new_data` must contain the age variable `%s`.", vvar$age))
    }
    new_data <- restore_vital(new_data, vvar[unlist(vvar) %in% names(new_data)])
  }
  if (!setequal(key_vars(object), non_age_keys(new_data))) {
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
dist_types <- function(dist) {
  map_chr(vctrs::vec_data(dist), function(x) class(x)[1])
}
