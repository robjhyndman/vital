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
      generate(h = h + 2, times = n_reps) |>
      dplyr::rename(mx = .sim) |>
      dplyr::select(-.model)
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
    ) |>
      mutate(mx = 0)
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
      generate(h = h + 2, times = n_reps) |>
      dplyr::rename(fx = .sim) |>
      dplyr::select(-.model) |>
      dplyr::mutate(fx = pmax(fx, 0)) # Ensure no negative fertility rates
  } else {
    # 0 births
    future_fertility <- tidyr::expand_grid(
      year = max(pop[[indexvar]]) + seq(h + 2) - 1,
      age = unique(pop[[vvars$age]]),
      .rep = seq(n_reps)
    ) |>
      mutate(fx = 0)
    colnames(future_fertility) <- c(indexvar, vvars$age, ".rep", "fx")
  }
  # Simulate from migration model
  if (!is.null(migration_model)) {
    future_migration <- migration_model |>
      generate(h = h + 2, times = n_reps) |>
      dplyr::rename(Nx = .sim) |>
      dplyr::select(-.model)
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
    ) |>
      mutate(Nx = 0)
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
      as_tibble(future_fertility) |> mutate(Sex = female),
      by = c(indexvar, vvars$age, vvars$sex, ".rep")
    ) |>
    dplyr::left_join(
      future_migration,
      by = c(indexvar, vvars$age, vvars$sex, ".rep")
    ) |>
    dplyr::mutate(
      fx = tidyr::replace_na(fx, 0),
      Nx = tidyr::replace_na(Nx, 0),
      Population = NA_integer_
    )
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
  for (y in seq(h)) {
    yr <- future[[y]][[indexvar]][1]
    future[[y]][[vvars$population]] <- pmax(
      0,
      round(future[[y]]$Prev_Pop * (1 - future[[y]]$mx) + future[[y]]$Nx)
    )
    future[[y]][[vvars$age]] <- future[[y]][[vvars$age]] + 1
    births <- future[[y]] |>
      group_by(.rep) |>
      summarise(
        Births = sum(fx * Prev_Pop, na.rm = TRUE),
        .groups = "drop"
      )
    births[[male]] <- rbinom(n_reps, round(births$Births), prob = (1.05 / 2.05))
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
    future[[y]] <- future[[y]] |>
      bind_rows(births)
    future[[y]] <- future[[y]] |>
      dplyr::arrange(
        future[[y]][[indexvar]],
        future[[y]][[vvars$age]],
        future[[y]][[vvars$sex]],
        future[[y]][[".rep"]]
      )
    max_age <- max(future[[y]][[vvars$age]])
    last_group <- future[[y]][[vvars$age]] == max_age
    second_last_group <- future[[y]][[vvars$age]] == max_age - 1
    future[[y]][[vvars$population]][second_last_group] <-
      future[[y]][[vvars$population]][second_last_group] +
      future[[y]][[vvars$population]][last_group]
    future[[y]] <- future[[y]][
      future[[y]][[vvars$age]] < max(future[[y]][[vvars$age]]),
    ]
    if (y < h) {
      future[[y + 1]]$Prev_Pop <- future[[y]][[vvars$population]]
    }
    future[[y]] <- future[[y]] |>
      select(indexvar, vvars$age, vvars$sex, ".rep", vvars$population)
  }
  future <- bind_rows(future) |>
    as_vital(
      index = Year,
      key = c(vvars$age, vvars$sex, ".rep"),
      .sex = vvars$sex,
      .age = vvars$age
    )
}
