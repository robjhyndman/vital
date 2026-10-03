test_that("model() gives the same fits when future is attached", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  dat <- norway_mortality |>
    dplyr::filter(Year > 2000, Age < 90) |>
    make_pr(Mortality)
  fit_models <- function() {
    msgs <- character()
    fit <- withCallingHandlers(
      dat |>
        model(
          lc = LC(log(Mortality)),
          fdm = FDM(log(Mortality), coherent = TRUE),
          bad = LC(Mortality)
        ),
      warning = function(w) {
        msgs <<- c(msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    # Only the bad model should fail, so report any other model errors
    errors <- grep("encountered for", msgs, value = TRUE)
    expect_true(
      all(grepl("encountered for bad", errors)),
      info = paste(errors, collapse = "\n")
    )
    fit
  }
  base <- fit_models()

  attached <- !rlang::is_attached("package:future")
  suppressPackageStartupMessages(library(future))
  old_plan <- future::plan(future::sequential)
  tryCatch(
    {
      expect_warning(
        dat |> model(bad = LC(Mortality)),
        "Lee-Carter models require a log transformation"
      )
      called <- FALSE
      with_mocked_bindings(
        dat |> model(lc = LC(log(Mortality))),
        future_mapply = function(..., future.globals, future.seed) {
          called <<- TRUE
          mapply(...)
        },
        .package = "future.apply"
      )
      expect_true(called)
      seq_fit <- fit_models()
      expect_identical(colnames(seq_fit), colnames(base))
      expect_identical(seq_fit$Sex, base$Sex)
      expect_identical(
        forecast(seq_fit |> dplyr::select(-bad), h = 2)$.mean,
        forecast(base |> dplyr::select(-bad), h = 2)$.mean
      )

      skip_on_cran()
      future::plan(future::multisession, workers = 2)
      ms_fit <- fit_models()
      expect_identical(
        forecast(ms_fit |> dplyr::select(-bad), h = 2)$.mean,
        forecast(base |> dplyr::select(-bad), h = 2)$.mean
      )
    },
    finally = {
      future::plan(old_plan)
      if (attached) {
        detach("package:future")
      }
    }
  )
})

test_that("model() estimates without .safely", {
  dat <- norway_mortality |>
    dplyr::filter(Year > 2000, Sex == "Female")
  safe <- dat |> model(mean = FMEAN(Mortality))
  unsafe <- dat |> model(mean = FMEAN(Mortality), .safely = FALSE)
  expect_equal(
    safe$mean[[1]]$fit$model,
    unsafe$mean[[1]]$fit$model
  )
  expect_error(
    dat |> model(bad = LC(Mortality), .safely = FALSE),
    "Lee-Carter models require a log transformation"
  )
})

test_that("model() reports the underlying cause of chained errors", {
  dat <- norway_mortality |>
    dplyr::filter(Year > 2000, Sex == "Female")
  local_mocked_bindings(
    train_fmean = function(...) {
      purrr::map(1, function(i) stop("the underlying cause"))
    }
  )
  expect_warning(
    dat |> model(mean = FMEAN(Mortality)),
    "the underlying cause"
  )
})
