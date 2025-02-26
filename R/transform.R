# Lookup table for function inverses
inverse_table <- function() {
  table <- new.env(parent = emptyenv())
  list(
    add = function(ns, fn, inv) {
      table[[ns]] <- as.list(table[[ns]])
      table[[ns]][[fn]] <- inv
    },
    get = function(ns, fn) {
      ns_name <- environmentName(ns)
      if (nchar(ns_name) == 0) {
        ns_name <- "base"
      }
      ret <- table[[ns_name]][[fn]]
      if (is.null(ret)) {
        t_fn <- get(fn, envir = ns)
        if (inherits(t_fn, "transformation")) {
          ret <- function(operation, target, result) {
            args <- call_args(operation)
            target_pos <- match(list(target), args)
            call2(
              expr(invert_transformation(!!t_fn)),
              !!!replace(args, target_pos, list(result))
            )
          }
        } else {
          abort(sprintf(
            "No supported inverse for the `%s` transformation.",
            fn
          ))
        }
      }
      ret
    }
  )
}

undo_transformation <- function(operation, target, result) {
  fn <- call_name(operation)
  env <- get_env(operation, caller_env())
  ns <- eval_tidy(expr(environment(get(!!fn))), env = env)
  inverse_table$get(ns, fn)(operation, get_expr(target), result)
}

inverse_table <- inverse_table()

map(
  c("log", "logb"),
  function(.x) {
    inverse_table$add(
      "base",
      .x,
      function(operation, target, result) {
        args <- call_args(operation)
        target_pos <- match(list(target), args)
        if (length(args) == 1) {
          expr(exp(!!result))
        } else {
          expr((!!args[[2]])^!!result)
        }
      }
    )
  }
)

inverse_table$add(
  "base",
  "log10",
  function(operation, target, result) {
    args <- call_args(operation)
    target_pos <- match(list(target), args)
    expr(10^!!result)
  }
)

inverse_table$add(
  "base",
  "log2",
  function(operation, target, result) {
    args <- call_args(operation)
    target_pos <- match(list(target), args)
    expr(2^!!result)
  }
)

inverse_table$add(
  "base",
  "log1p",
  function(operation, target, result) {
    args <- call_args(operation)
    target_pos <- match(list(target), args)
    expr(expm1(!!result))
  }
)

inverse_table$add(
  "base",
  "expm1",
  function(operation, target, result) {
    args <- call_args(operation)
    target_pos <- match(list(target), args)
    expr(log1p(!!result))
  }
)

inverse_table$add(
  "base",
  "exp",
  function(operation, target, result) {
    args <- call_args(operation)
    target_pos <- match(list(target), args)
    expr(log(!!!replace(args, target_pos, list(result))))
  }
)

inverse_table$add(
  "fabletools",
  "box_cox",
  function(operation, target, result) {
    args <- call_args(operation)
    target_pos <- match(list(target), args)
    expr(inv_box_cox(!!!replace(args, target_pos, list(result))))
  }
)

inverse_table$add(
  "fabletools",
  "inv_box_cox",
  function(operation, target, result) {
    args <- call_args(operation)
    target_pos <- match(list(target), args)
    expr(box_cox(!!!replace(args, target_pos, list(result))))
  }
)

inverse_table$add(
  "base",
  "sqrt",
  function(operation, target, result) {
    expr((!!result)^2)
  }
)

inverse_table$add(
  "base",
  "^",
  function(operation, target, result) {
    args <- call_args(operation)
    target_pos <- match(list(target), args)
    if (target_pos == 1) {
      if (args[[2]] == 0) {
        abort(sprintf("Cannot invert %s.", expr_text(operation)))
      }
      expr((!!result)^(1 / !!args[[2]]))
    } else {
      expr(log(!!result) / log(!!args[[1]]))
    }
  }
)

inverse_table$add(
  "base",
  "+",
  function(operation, target, result) {
    args <- call_args(operation)
    target_pos <- match(list(target), args)
    if (length(args) == 1) {
      result
    } else {
      expr(!!result - !!args[[-target_pos]])
    }
  }
)

inverse_table$add(
  "base",
  "-",
  function(operation, target, result) {
    args <- call_args(operation)
    target_pos <- match(list(target), args)
    if (length(args) == 1) {
      expr(-!!result)
    } else {
      if (target_pos == 1) {
        expr(!!result + !!args[[2]])
      } else {
        expr(!!args[[1]] - !!result)
      }
    }
  }
)

inverse_table$add(
  "base",
  "/",
  function(operation, target, result) {
    args <- call_args(operation)
    target_pos <- match(list(target), args)
    if (target_pos == 1) {
      expr(!!args[[2]] * !!result)
    } else {
      expr(!!args[[1]] / !!result)
    }
  }
)

inverse_table$add(
  "base",
  "*",
  function(operation, target, result) {
    args <- call_args(operation)
    target_pos <- match(list(target), args)
    expr(!!result / !!args[[-target_pos]])
  }
)

inverse_table$add(
  "base",
  "(",
  function(operation, target, result) {
    call2("(", !!!exprs(!!result))
  }
)
