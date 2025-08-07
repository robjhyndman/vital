#' Future population simulation
#'
#' Simulate future age-specific population given a starting population and
#' models for fertility, mortality, and migration. If any model is NULL, it is
#' assumed there are no future births, deaths or net migrants, respectively.
#' This is an experimental function and has not been thoroughly tested.
#'
#' @param starting_population A `vital` object with the age-sex-specific starting population.
#' @param mortality_model A `mable` object containing an age-sex-specific model for mortality rates,
#' trained on data up to the year of the starting population. If NULL, there are zero future deaths.
#' @param fertility_model A `mable` object containing an age-specific model for fertility rates,
#' trained on data up to the year of the starting population. If NULL, there are zero future births.
#' @param migration_model A `mable` object containing an age-sex-specific model for net migration numbers,
#' trained on data up to the year of the starting population. If NULL, there are zero future net migrants.
#' @param h The forecast horizon equal to the number of years to simulate into the future.
#' @param n_reps The number of replicates to simulate.
#' @param female A character string giving the name used for females in the sex
#' variable of the `starting_population`. This is needed when computing births
#' from the fertility rates. If missing, the function will try to identify the
#' most likely value automatically.
#' @return A `vital` object containing the simulated future population.
#' @export
generate_population <- function(
  starting_population,
  mortality_model = NULL,
  fertility_model = NULL,
  migration_model = NULL,
  h = 10,
  n_reps = 1000,
  female = NULL
) {
  # Check inputs
  if (!inherits(starting_population, "vital")) {
    stop("starting_population must be a vital object")
  }
  if (!is.null(mortality_model)) {
    if (!inherits(mortality_model, "mdl_vtl_df")) {
      stop("mortality_model must be a mable object")
    }
    if (NCOL(mortality_model) > 2) {
      stop("mortality_model must contain only one model")
    }
  }
  if (!is.null(fertility_model)) {
    if (!inherits(fertility_model, "mdl_vtl_df")) {
      stop("fertility_model must be a mable object")
    }
    if (NCOL(fertility_model) > 2) {
      stop("fertility_model must contain only one model")
    }
  }
  if (!is.null(migration_model)) {
    if (!inherits(migration_model, "mdl_vtl_df")) {
      stop("migration_model must be a mable object")
    }
    if (NCOL(migration_model) > 2) {
      stop("mortality_model must contain only one model")
    }
  }
  if (!is.numeric(h) || h <= 0) {
    stop("h must be a positive numeric value")
  }
  if (!is.numeric(n_reps) || n_reps <= 0) {
    stop("n_reps must be a positive numeric value")
  }
  indexvar <- index_var(starting_population)
  vvars <- vital_var_list(starting_population)
  if (is.null(female)) {
    sexes <- unique(starting_population[[vvars$sex]])
    if (length(sexes) != 2) {
      stop("More than 2 sexes found")
    }
    # Try female
    female <- sexes[grepl("^[Ff]", sexes)]
    if (length(female) == 0L) {
      # Try women
      female <- sexes[grepl("^[Ww]", sexes)]
    }
    if (length(female) == 0L) {
      female <- sexes[1]
      warning(paste("Setting female to ", female))
    }
    male <- sexes[sexes != female]
  }
  # Prepare the starting population
  pop <- starting_population[
    starting_population[[indexvar]] == max(starting_population[[indexvar]]),
  ]
  pop[[indexvar]] <- pop[[indexvar]] + 1
  pop$Prev_Pop <- round(pop[[vvars$population]])
  pop <- pop[, c(indexvar, vvars$age, vvars$sex, "Prev_Pop")]

  # Simulate from mortality model
  if (!is.null(mortality_model)) {
    future_mortality <- mortality_model |>
      generate(h = h + 2, times = n_reps)
    future_mortality$mx <- future_mortality$.sim
    future_mortality <- future_mortality |> dplyr::select(-.sim, -.model)
    if ("geometric_mean" %in% future_mortality[[vvars$sex]]) {
      future_mortality <- undo_pr(future_mortality, "mx", key = vvars$sex)
    }
  } else {
    # 0 deaths
    future_mortality <- tidyr::expand_grid(
      year = max(pop[[indexvar]]) + seq(h + 2) - 1,
      age = unique(pop[[vvars$age]]),
      sex = unique(pop[[vvars$sex]]),
      .rep = seq(n_reps)
    )
    future_mortality$mx <- 0
    colnames(future_mortality) <- c(
      indexvar,
      vvars$age,
      vvars$sex,
      ".rep",
      "mx"
    )
  }
  # Simulate from fertility model
  if (!is.null(fertility_model)) {
    future_fertility <- fertility_model |>
      generate(h = h + 2, times = n_reps)
    future_fertility$fx <- pmax(future_fertility$.sim, 0) # Ensure no negative fertility rates
    future_fertility[[vvars$sex]] <- female
    future_fertility <- future_fertility |> dplyr::select(-.sim, -.model)
  } else {
    # 0 births
    future_fertility <- tidyr::expand_grid(
      year = max(pop[[indexvar]]) + seq(h + 2) - 1,
      age = unique(pop[[vvars$age]]),
      .rep = seq(n_reps)
    )
    future_fertility$fx <- 0
    future_fertility[[vvars$sex]] <- female
    colnames(future_fertility) <- c(
      indexvar,
      vvars$age,
      vvars$sex,
      ".rep",
      "fx"
    )
  }
  # Simulate from migration model
  if (!is.null(migration_model)) {
    future_migration <- migration_model |>
      generate(h = h + 2, times = n_reps)
    future_migration$Nx <- future_migration$.sim
    future_migration <- future_migration |> dplyr::select(-.sim, -.model)
    if ("mean" %in% future_mortality[[vvars$sex]]) {
      future_migration <- undo_sd(future_migration, "Nx", key = vvars$sex)
    }
  } else {
    # 0 net migrants
    future_migration <- tidyr::expand_grid(
      year = max(pop[[indexvar]]) + seq(h + 2) - 1,
      age = unique(pop[[vvars$age]]),
      sex = unique(pop[[vvars$sex]]),
      .rep = seq(n_reps)
    )
    future_migration$Nx <- 0
    colnames(future_migration) <- c(
      indexvar,
      vvars$age,
      vvars$sex,
      ".rep",
      "Nx"
    )
  }
  # Combine into one tibble
  future <- tibble::as_tibble(pop) |>
    dplyr::right_join(
      as_tibble(future_mortality),
      by = c(indexvar, vvars$age, vvars$sex)
    ) |>
    dplyr::left_join(
      as_tibble(future_fertility),
      by = c(indexvar, vvars$age, vvars$sex, ".rep")
    ) |>
    dplyr::left_join(
      future_migration,
      by = c(indexvar, vvars$age, vvars$sex, ".rep")
    )
  future$fx[is.na(future$fx)] <- 0
  future$Nx[is.na(future$Nx)] <- 0
  future$Population <- NA_integer_
  future <- future |>
    dplyr::arrange(
      future[[indexvar]],
      future[[vvars$age]],
      future[[vvars$sex]],
      future[[".rep"]]
    )
  # Remove extra years
  first_year <- max(pop[[indexvar]])
  last_year <- first_year + h - 1
  future <- future[future[[indexvar]] <= last_year, ]

  # Split into years
  future <- split(future, future[[indexvar]])

  # Advance the population by one year and combine upper ages. Assume zero births
  advance <- function(age, x) {
    # Check age is ordered
    if (max(diff(age)) != 1 & min(diff(age)) < 0) {
      stop("Age variable must be ordered and consecutive")
    }
    min_age <- age == min(age)
    max_age <- age == max(age)
    max_age_1 <- age == max(age) - 1
    c(rep(0, sum(min_age)), x[!max_age & !max_age_1], x[max_age_1] + x[max_age])
  }
  for (y in seq(h)) {
    yr <- future[[y]][[indexvar]][1]
    n <- NROW(future[[y]])
    # Add half migrants to current population
    future[[y]]$Rx <- pmax(0, future[[y]]$Prev_Pop + 0.5 * future[[y]]$Nx)
    # Survivorship ratios
    nsr <- future[[y]] |>
      as_tsibble(index = indexvar, key = c(vvars$age, vvars$sex, ".rep")) |>
      as_vital(.sex = "Sex", .age = "Age", .population = "Rx") |>
      life_table()
    nsr$nsr <- 1 - nsr$rx
    nsr <- nsr[, c(indexvar, vvars$age, vvars$sex, ".rep", "nsr")]
    # Deaths
    future[[y]] <- future[[y]] |>
      left_join(nsr, by = c(indexvar, vvars$age, vvars$sex, ".rep"))
    future[[y]]$cohD <- pmax(0, future[[y]]$nsr * future[[y]]$Rx)
    future[[y]]$Rx2 <- pmax(
      0,
      advance(future[[y]][[vvars$age]], future[[y]]$Rx - future[[y]]$cohD)
    )
    future[[y]]$Ex <- 0.5 * (future[[y]]$Rx + future[[y]]$Rx2)
    future[[y]]$Dx <- stats::rpois(n, future[[y]]$Ex * future[[y]]$mx)
    future[[y]]$cohD <- 0.5 *
      (future[[y]]$Dx + advance(future[[y]][[vvars$age]], future[[y]]$Dx))
    future[[y]]$Rx2 <- pmax(
      0,
      advance(future[[y]][[vvars$age]], future[[y]]$Rx - future[[y]]$cohD)
    )

    births <- future[[y]][, c(
      indexvar,
      vvars$age,
      vvars$sex,
      ".rep",
      "fx",
      "Rx",
      "Rx2"
    )]
    births$Births <- stats::rpois(n, births$fx * (births$Rx + births$Rx2) / 2)
    births <- births |>
      group_by(.rep) |>
      summarise(
        Births = sum(Births, na.rm = TRUE),
        .groups = "drop"
      )
    births[[male]] <- stats::rbinom(
      n_reps,
      round(births$Births),
      prob = (1.05 / 2.05)
    )
    births[[female]] <- births$Births - births[[male]]
    births$Births <- NULL
    births <- births |>
      tidyr::pivot_longer(
        all_of(c(male, female)),
        names_to = vvars$sex,
        values_to = vvars$population
      )
    births[[vvars$age]] <- 0
    births[[indexvar]] <- yr

    # Infant mortality
    births <- births |>
      dplyr::left_join(
        future[[y]][, c(
          indexvar,
          vvars$age,
          vvars$sex,
          ".rep",
          "Nx",
          "mx",
          "nsr"
        )],
        by = c(indexvar, vvars$age, vvars$sex, ".rep")
      )
    births$RxB <- births$Population + 0.5 * births$Nx
    births$cohD <- pmax(0, births$nsr * births$RxB)
    births$Rx20 <- births$RxB - births$cohD
    births$Ex0 <- 0.5 * (births$RxB + births$Rx20)
    births$Dx <- stats::rpois(NROW(births), births$Ex0 * births$mx)
    births$f0 <- births$cohD / (births$Ex0 * births$mx)
    births$cohDB <- births$f0 * births$Dx
    births$Dx0 <- pmax(0, births$Dx - births$cohDB)
    age0 <- births[, c(
      indexvar,
      vvars$age,
      vvars$sex,
      ".rep",
      "RxB",
      "cohDB",
      "f0",
      "Dx0"
    )] |>
      left_join(
        future[[y]][future[[y]][[vvars$age]] == 0, ],
        by = c(indexvar, vvars$age, vvars$sex, ".rep")
      )
    age0$cohD <- (1 - age0$f0) * age0$Dx0 + 0.5 * age0$Dx
    age0$Rx2 <- age0$RxB - age0$cohDB
    age0 <- age0[, colnames(future[[y]])]
    future[[y]] <- dplyr::bind_rows(
      age0,
      future[[y]][future[[y]][[vvars$age]] > 0, ]
    )
    future[[y]][[vvars$population]] <- pmax(
      0,
      round(future[[y]]$Rx2 + 0.5 * future[[y]]$Nx)
    )
    future[[y]] <- future[[y]] |>
      dplyr::arrange(
        future[[y]][[indexvar]],
        future[[y]][[vvars$age]],
        future[[y]][[vvars$sex]],
        future[[y]][[".rep"]]
      )
    if (y < h) {
      future[[y + 1]]$Prev_Pop <- future[[y]][[vvars$population]]
    }
    future[[y]] <- future[[y]][, c(
      indexvar,
      vvars$age,
      vvars$sex,
      ".rep",
      vvars$population
    )]
  }
  future <- dplyr::bind_rows(future) |>
    as_vital(
      index = Year,
      key = c(vvars$age, vvars$sex, ".rep"),
      .sex = vvars$sex,
      .age = vvars$age
    )
}

utils::globalVariables(c("fx", "Nx", "Prev_Pop"))
