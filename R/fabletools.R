# Non-exported functions borrowed from fabletools

is.formula <- function(x) {
  inherits(x, "formula")
}

traverse <- function (x, .f = list, .g = identity, .h = identity,
  base = function(.x)  is_syntactic_literal(.x) || is_symbol(.x))  {
    if (base(x))
        return(.h(x))
    .f(lapply(.g(x), traverse, .f = .f, .g = .g, .h = .h, base = base),
        .h(x))
}

traverse_call <- function (x, .f = function(.x, .y) map(.x, quo_get_expr) %>%
    as.call %>% new_quosure(env = get_env(.x[[1]])), .g = function(.x) .x %>%
    get_expr %>% as.list %>% map(new_quosure, env = get_env(.x)),
    .h = identity, base = function(.x) !quo_is_call(.x)) {
    x <- enquo(x)
    traverse(x, .f = .f, .g = .g, .h = .h, base = base)
}

names_no_null <- function (x) {
    names(x) %||% rep_along(x, "")
}

guess_response <- function (.data) {
    all_vars <- custom_error(measured_vars, "This model function does not support automatic selection of response variables. Please specify this in the model formula.")(.data)
    if (length(all_vars) != 1) {
        abort("Could not automatically determine the response variable, please provide the response variable in the model specification")
    }
    out <- sym(all_vars[[1]])
    inform(sprintf("Model not specified, defaulting to automatic modelling of the `%s` variable. Override this using the model formula.",
        expr_name(out)))
    out
}

custom_error <- function (.f, error) {
    force(error)
    function(...) {
        res <- capture_error(.f(...))
        if (!is.null(res$error)) {
            abort(error)
        }
        res$result
    }
}

merge_named_list <- function (...) {
    flat <- flatten(list(...))
    nm <- names_no_null(flat)
    map(split(flat, nm), function(x) flatten(unname(x)))
}

capture_error <- function (code, otherwise = NULL, quiet = TRUE) {
    tryCatch(list(result = code, error = NULL), error = function(e) {
        if (!quiet)
            message("Error: ", e$message)
        list(result = otherwise, error = e)
    }, interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
    })
}

unnest_tbl <- function (.data, tbl_col, .sep = NULL) {
    row_indices <- rep.int(seq_len(NROW(.data)), map_int(.data[[tbl_col[[1]]]],
        NROW))
    nested_cols <- map(tbl_col, function(x) {
        lst_col <- .data[[x]]
        if (is.data.frame(lst_col[[1]])) {
            lst_col <- map(lst_col, as_tibble)
            vctrs::vec_rbind(!!!lst_col)
        }
        else {
            unlist(lst_col)
        }
    })
    if (!is.null(.sep)) {
        nested_cols <- map2(nested_cols, tbl_col, function(x,
            nm) set_names(x, paste(nm, colnames(x), sep = .sep)))
    }
    is_df <- map_lgl(nested_cols, is.data.frame)
    vctrs::vec_cbind(.data[row_indices, setdiff(names(.data),
        tbl_col), drop = FALSE], !!!set_names(nested_cols[!is_df],
        tbl_col[!is_df]), !!!nested_cols[is_df])
}

unnest_tsbl <- function (.data, tsbl_col, parent_key = NULL, interval = NULL)
{
    tsbl <- .data[[tsbl_col]][[1L]]
    if (!is_tsibble(tsbl)) {
        abort("Unnested column is not a tsibble object.")
    }
    idx <- index(tsbl)
    idx_chr <- as_string(idx)
    key <- c(parent_key, key_vars(tsbl))
    .data <- unnest_tbl(.data, tsbl_col)
    build_tsibble(.data, key = !!key, index = !!idx, index2 = !!index2(tsbl),
        ordered = is_ordered(tsbl), interval = interval %||% interval(tsbl),
        validate = FALSE)
}
