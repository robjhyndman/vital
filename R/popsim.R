#' Future population simulation
#'
#' Simulate future age-specific population given a starting population and
#' models for fertility, mortality, and migration. If any model is NULL, it is
#' assumed there are no future births, deaths or net migrants, respectively.
#' This is an experimental function and has not been thoroughly tested.
#' The simulation follows `demography::pop.sim()`, as described in Hyndman and Booth (2008).
#'
#' @references
#' Hyndman and Booth (2008) Stochastic population forecasts using functional data
#' models for mortality, fertility and migration. *International Journal of Forecasting*, 24(3), 323-342.
#'
#' @param starting_population A `vital` object with the age-sex-specific starting population.
#' @param mortality_model A `mable` object containing an age-sex-specific model for mortality rates,
#' trained on data up to the year of the starting population. If NULL, there are zero future deaths.
#' @param fertility_model A `mable` object containing an age-specific model for fertility rates,
#' trained on data up to the year of the starting population. If NULL, there are zero future births.
#' @param migration_model A `mable` object containing an age-sex-specific model for net migration numbers,
#' trained on data up to the year of the starting population. Net migrants are indexed by age at the
#' end of the year, as returned by [net_migration()]. If NULL, there are zero future net migrants.
#' @param h The forecast horizon equal to the number of years to simulate into the future.
#' @param n_reps The number of replicates to simulate.
#' @param female A character string giving the name used for females in the sex
#' variable of the `starting_population`. This is needed when computing births
#' from the fertility rates. If missing, the function will try to identify the
#' most likely value automatically.
#' @return A `vital` object containing the simulated future population.
#' @examples
#' # Norwegian data, with ages above 100 combined
#' nor <- norway_mortality |>
#'   dplyr::filter(Sex != "Total") |>
#'   collapse_ages(max_age = 100)
#' # Models trained on data up to the year of the starting population
#' mortality <- nor |>
#'   dplyr::filter(Year > 2010) |>
#'   model(fmean = FMEAN(log(Mortality)))
#' fertility <- norway_fertility |>
#'   dplyr::filter(Year > 2010) |>
#'   model(fmean = FMEAN(Fertility))
#' migration <- net_migration(nor, norway_births) |>
#'   dplyr::filter(Year > 2010) |>
#'   model(fmean = FMEAN(NetMigration))
#' # Simulate 5 future populations for 3 years, starting from the final year of data
#' generate_population(
#'   starting_population = nor |> dplyr::filter(Year == max(Year)),
#'   mortality_model = mortality,
#'   fertility_model = fertility,
#'   migration_model = migration,
#'   h = 3,
#'   n_reps = 5
#' )
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
    if (length(mable_vars(mortality_model)) > 1) {
      stop("mortality_model must contain only one model")
    }
  }
  if (!is.null(fertility_model)) {
    if (!inherits(fertility_model, "mdl_vtl_df")) {
      stop("fertility_model must be a mable object")
    }
    if (length(mable_vars(fertility_model)) > 1) {
      stop("fertility_model must contain only one model")
    }
  }
  if (!is.null(migration_model)) {
    if (!inherits(migration_model, "mdl_vtl_df")) {
      stop("migration_model must be a mable object")
    }
    if (length(mable_vars(migration_model)) > 1) {
      stop("migration_model must contain only one model")
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
  # Populations are advanced one year of age per year
  ages <- sort(unique(starting_population[[vvars$age]]))
  if (length(ages) < 3 || any(diff(ages) != 1)) {
    stop("starting_population must have at least three consecutive single-year ages")
  }
  sexes <- unique(starting_population[[vvars$sex]])
  if (length(sexes) != 2) {
    stop("starting_population must contain exactly 2 sexes")
  }
  if (is.null(female)) {
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
  } else if (!(female %in% sexes)) {
    stop("female must be one of the values of the sex variable")
  }
  male <- sexes[sexes != female]
  # Prepare the starting population
  pop <- starting_population[
    starting_population[[indexvar]] == max(starting_population[[indexvar]]),
  ]
  pop[[indexvar]] <- pop[[indexvar]] + 1
  pop$Prev_Pop <- round(pop[[vvars$population]])
  pop <- pop[, c(indexvar, vvars$age, vvars$sex, "Prev_Pop")]
  # Years to simulate
  first_year <- max(pop[[indexvar]])
  last_year <- first_year + h - 1

  # Simulate from mortality model
  if (!is.null(mortality_model)) {
    future_mortality <- mortality_model |>
      generate(
        h = model_horizon(mortality_model, first_year - 1, last_year),
        times = n_reps
      )
    future_mortality$mx <- pmax(future_mortality$.sim, 0) # Ensure no negative mortality rates
    future_mortality <- future_mortality |> dplyr::select(-.sim, -.model)
    if ("geometric_mean" %in% future_mortality[[vvars$sex]]) {
      future_mortality <- undo_pr(
        future_mortality,
        "mx",
        key = all_of(vvars$sex)
      )
    }
  } else {
    # 0 deaths
    future_mortality <- tidyr::expand_grid(
      year = first_year + seq(h) - 1,
      age = unique(pop[[vvars$age]]),
      sex = unique(pop[[vvars$sex]]),
      .rep = as.character(seq(n_reps))
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
      generate(
        h = model_horizon(fertility_model, first_year - 1, last_year),
        times = n_reps
      )
    future_fertility$fx <- pmax(future_fertility$.sim, 0) # Ensure no negative fertility rates
    future_fertility[[vvars$sex]] <- female
    future_fertility <- future_fertility |> dplyr::select(-.sim, -.model)
  } else {
    # 0 births
    future_fertility <- tidyr::expand_grid(
      year = first_year + seq(h) - 1,
      age = unique(pop[[vvars$age]]),
      sex = female,
      .rep = as.character(seq(n_reps))
    )
    future_fertility$fx <- 0
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
      generate(
        h = model_horizon(migration_model, first_year - 1, last_year),
        times = n_reps
      )
    future_migration$Nx <- future_migration$.sim
    future_migration <- future_migration |> dplyr::select(-.sim, -.model)
    if ("mean" %in% future_migration[[vvars$sex]]) {
      future_migration <- undo_sd(
        future_migration,
        "Nx",
        key = all_of(vvars$sex)
      )
    }
  } else {
    # 0 net migrants
    future_migration <- tidyr::expand_grid(
      year = first_year + seq(h) - 1,
      age = unique(pop[[vvars$age]]),
      sex = unique(pop[[vvars$sex]]),
      .rep = as.character(seq(n_reps))
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
  future[[vvars$population]] <- NA_real_
  future <- future |>
    dplyr::arrange(
      future[[indexvar]],
      future[[vvars$age]],
      future[[vvars$sex]],
      future[[".rep"]]
    )
  # Remove years before the starting population
  future <- future[future[[indexvar]] >= first_year, ]

  # Split into years
  future <- split(future, future[[indexvar]])

  # The simulation follows demography::pop.sim(). Rows within each year are
  # ordered by age, then sex and replicate, so each age is a block of rows.
  # Advance the population by one year and combine upper ages. Assume zero births
  advance <- function(age, x) {
    min_age <- age == min(age)
    max_age <- age == max(age)
    max_age_1 <- age == max(age) - 1
    c(rep(0, sum(min_age)), x[!max_age & !max_age_1], x[max_age_1] + x[max_age])
  }
  # Value at the next age; the oldest age keeps its own value
  next_age <- function(age, x) {
    c(x[age != min(age)], x[age == max(age)])
  }
  for (y in seq(h)) {
    fy <- future[[y]]
    n <- NROW(fy)
    age <- fy[[vvars$age]]
    age0 <- age == min(age)
    oldest <- age == max(age)
    oldest_1 <- age == max(age) - 1
    # Net migrants are indexed by age at the end of the year. Add half of them
    # to the cohort at the start of the year, splitting those in the open age
    # group equally between its two cohorts
    mig <- next_age(age, fy$Nx)
    mig[oldest_1 | oldest] <- rep(0.5 * fy$Nx[oldest], 2)
    fy$Rx <- pmax(0, fy$Prev_Pop + 0.5 * mig)
    # Survivorship ratios from the life table of each sex and replicate
    rx <- single_year_rx(
      fy$mx,
      age,
      paste(fy[[vvars$sex]], fy$.rep),
      fy[[vvars$sex]]
    )
    nsr <- 1 - rx
    # No survivors where the life table has run out of lives
    nsr[!is.finite(nsr)] <- 1
    fy$nsr <- pmin(pmax(nsr, 0), 1)
    # Deaths, with exposures from the cohorts' expected survivors, using the
    # survivorship ratio for each cohort's age at the end of the year
    cohD <- pmax(0, next_age(age, fy$nsr) * fy$Rx)
    Rx2 <- pmax(0, advance(age, fy$Rx - cohD))
    fy$Dx <- stats::rpois(n, 0.5 * (fy$Rx + Rx2) * fy$mx)
    # Each cohort has half the deaths at its age at the start of the year and
    # half at its age at the end, except that the open age group's deaths all
    # come from its cohorts
    fy$cohD <- 0.5 * (fy$Dx + next_age(age, fy$Dx))
    fy$cohD[oldest_1] <- 0.5 * fy$Dx[oldest_1] + fy$Dx[oldest]
    fy$cohD[oldest] <- 0
    fy$Rx2 <- pmax(0, advance(age, fy$Rx - fy$cohD))

    births <- fy[, c(indexvar, vvars$age, vvars$sex, ".rep", "fx", "Rx", "Rx2")]
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
        values_to = "B"
      )

    # Infant mortality, for the rows of the youngest age
    infant <- fy[age0, c(vvars$sex, ".rep", "Nx", "mx", "nsr", "Rx")] |>
      left_join(births, by = c(vvars$sex, ".rep"))
    RxB <- pmax(0, infant$B + 0.5 * infant$Nx)
    cohDB <- infant$nsr * RxB
    # Deaths at age 0, from the cohort aged 0 at the start of the year and the
    # birth cohort
    risk0 <- 0.5 * (infant$Rx + RxB - cohDB) * infant$mx
    Dx0 <- stats::rpois(length(risk0), risk0)
    # Proportion of infant deaths in the birth cohort (none if no risk)
    f0 <- if_else(risk0 > 0, pmin(1, cohDB / risk0), 0)
    fy$cohD[age0] <- (1 - f0) * Dx0 + 0.5 * fy$Dx[age == min(age) + 1]
    fy$Rx2 <- pmax(0, advance(age, fy$Rx - fy$cohD))
    fy$Rx2[age0] <- pmax(0, RxB - f0 * Dx0)
    fy[[vvars$population]] <- pmax(0, round(fy$Rx2 + 0.5 * fy$Nx))
    future[[y]] <- fy
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
  future <- dplyr::bind_rows(future)
  # Restore the types of the index and age variables
  for (v in c(indexvar, vvars$age)) {
    future[[v]] <- vctrs::vec_cast(future[[v]], starting_population[[v]])
  }
  future |>
    as_vital(
      index = !!sym(indexvar),
      key = all_of(c(vvars$age, vvars$sex, ".rep")),
      .sex = vvars$sex,
      .age = vvars$age,
      .population = vvars$population
    )
}

# Survivorship ratios rx of single-year life tables, as computed by lt(), for
# all groups at once. Returns one value for each element of mx, where age gives
# the (consecutive) single-year ages, group identifies each life table, and sex
# gives the sex of each element.
single_year_rx <- function(mx, age, group, sex) {
  ages <- sort(unique(age))
  groups <- unique(group)
  nn <- length(ages)
  pos <- cbind(match(group, groups), match(age, ages))
  m <- matrix(NA_real_, length(groups), nn)
  m[pos] <- mx
  for (i in which(apply(is.na(m), 1, any))) {
    m[i, ] <- fill_mx(m[i, ], ages)
  }
  sex <- tolower(sex[match(groups, group)])
  # Average years lived in each age by those dying
  ax <- matrix(0.5, length(groups), nn)
  if (ages[1] == 0) {
    ax[, 1] <- dplyr::case_when(
      sex == "female" ~ 0.35 + (m[, 1] < 0.107) * (-0.297 + 2.8 * m[, 1]),
      sex == "male" ~ 0.33 + (m[, 1] < 0.107) * (-0.285 + 2.684 * m[, 1]),
      TRUE ~ 0.34 + (m[, 1] < 0.107) * (-0.291 + 2.742 * m[, 1])
    )
  }
  qx <- m / (1 + (1 - ax) * m)
  qx[qx > 1] <- 1
  lx <- matrix(1, length(groups), nn)
  for (j in seq_len(nn - 1)) {
    lx[, j + 1] <- lx[, j] * (1 - qx[, j])
  }
  dx <- lx - cbind(lx[, -1, drop = FALSE], 0)
  Lx <- lx - dx * (1 - ax)
  Lx[, nn] <- ifelse(m[, nn] == 0, 0, lx[, nn] / m[, nn])
  Lx[is.na(Lx)] <- 0
  Tx <- Lx[, nn:1, drop = FALSE]
  for (j in seq_len(nn - 1)) {
    Tx[, j + 1] <- Tx[, j + 1] + Tx[, j]
  }
  Tx <- Tx[, nn:1, drop = FALSE]
  rx <- cbind(
    Lx[, 1] / lx[, 1],
    Lx[, 2:(nn - 1), drop = FALSE] / Lx[, 1:(nn - 2), drop = FALSE],
    Tx[, nn] / Tx[, nn - 1]
  )
  rx[pos]
}

# Horizon needed for a model to be simulated up to last_year from the end of
# its data, which must not be later than the starting population (start_year)
model_horizon <- function(model, start_year, last_year) {
  fit_data <- model[[mable_vars(model)]][[1]]$data
  data_end <- max(fit_data[[index_var(fit_data)]])
  if (data_end > start_year) {
    stop(
      "Models must be trained on data up to the year of the starting population (",
      start_year,
      ")"
    )
  }
  last_year - data_end
}
