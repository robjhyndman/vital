#' Set the username and password to access the former site of the
#' Human Mortality Database
#'
#' @description
#' To download data from the HMD using this package, you need to register at
#' the "former" HMD website (https://former.mortality.org),
#' Usernames and passwords for the new HMD site (https://mortality.org) will not
#' work. This is a temporary patch until an API is released for the new site.
#'
#' @details You can either pass the username and password as arguments to the function,
#' or you can store your username and password in the .Renviron file
#' containing
#'
#' HMD_USERNAME=<YOUR USERNAME>
#'
#' HMD_PASSWORD=<YOUR PASSWORD>
#'
#' @author Emi Tanaka
#'
#' @param username The username (typically your email) that you signed up for HMD.
#' @param password The password used to access HMD.
#'
#' @export
hmd_session <- function(username = Sys.getenv("HMD_USERNAME"),
                        password = Sys.getenv("HMD_PASSWORD")) {
  .hmd$username <- username
  .hmd$password <- password
  invisible(list(username = username, password = password))
}

hmd_handle <- function(username, password) {
  h <- curl::new_handle()
  curl::handle_setopt(
    handle = h,
    httpauth = 1,
    userpwd = paste0(username, ":", password)
  )
  h
}

#' Fetch the data from the Human Mortality Database
#'
#' The `hmd_data()` fetches data from multiple statistics and merge them together
#' in the output. All other functions grab a particular (set of) statistics from
#' the Human Mortality Database (mortality.org). You need to register an account at
#' mortality.org to get the data and call the `hmd_session()` first before
#' using these functions.
#'
#' @param country The country code to fetch the data for. The input may be
#'   a named or unnamed character vector. If the vector is named, then the
#'   name is used as the label.
#' @param stats The statistics to extract. The available statistics are:
#'   "birth", "death", "life_expectancy", "exposure_to_risk", "population",
#'   "life_tables", and "death_rate".
#' @param sex_format A logical value to indicate whether the sex should be
#'   returned as "long" or "wide" format. The default is "wide".
#' @param range_year,range_age A single integer indicating the period or
#'   cohort year/age range. Generally, only 1, 5 or 10 are available.
#' @return A humble object.
#' @seealso hmd_session
##' @export
hmd_data <- function(country,
                     stats = "death_rate",
                     sex_format = c("wide", "long"),
                     year_range = 1,
                     age_range = 1) {
  stats <- match.arg(stats,
    c(
      "death_rate", "birth", "death", "life_expectancy",
      "exposure_to_risk", "population", "life_tables"
    ),
    several.ok = TRUE
  )
  sex_format <- match.arg(sex_format)

  res <- map(stats, function(astat) {
    switch(astat,
      "birth" = hmd_birth(country, sex_format),
      "death" = hmd_death(country, sex_format, age_range, year_range),
      "life_expectancy" = hmd_life_expectancy(country, sex_format, year_range),
      "exposure_to_risk" = hmd_exposure_to_risk(country, sex_format, age_range, year_range),
      "population" = hmd_population(country, sex_format, age_range),
      "life_tables" = hmd_life_table(country, sex_format, age_range, year_range),
      "death_rate" = hmd_death_rate(country, sex_format, age_range, year_range)
    )
  })
  new_humble(Reduce(merge, res))
}

#' @rdname hmd_data
##' @export
hmd_life_expectancy <- function(country, sex_format = c("wide", "long"), year_range = 1) {
  sex_format <- match.arg(sex_format)
  country_labels <- names(country) %||% country
  period <- ifelse(year_range == 1, "", paste0("_1x", year_range))
  filename <- paste0("E0per", period, ".txt")
  out <- map(1:length(country), function(i) {
    read_hmd_file(
      country[i],
      filename,
      country_labels[i]
    )
  })
  res <- do.call("rbind", out)
  year_name <- ifelse(year_range > 1, paste0("year_range_", year_range), "year")
  colnames(res) <- c(year_name, "lifeexp_female", "lifeexp_male", "lifeexp_total", "country")
  if (sex_format == "wide") {
    new_humble(res, intervals = year_name)
  } else {
    new_humble(rbind_sex(res, "lifeexp"), intervals = year_name)
  }
}

rbind_sex <- function(data, var) {
  nms <- colnames(data)
  out <- data[setdiff(nms, paste0(var, c("_female", "_male", "_total")))]
  out <- rbind(out, out, out)
  for (avar in var) {
    fvec <- data[[paste0(avar, "_female")]]
    mvec <- data[[paste0(avar, "_male")]]
    tvec <- data[[paste0(avar, "_total")]]
    out[[avar]] <- c(fvec, mvec, tvec)
  }
  out$sex <- rep(c("female", "male", "total"), c(length(fvec), length(mvec), length(tvec)))
  out
}

#' @rdname hmd_data
##' @export
hmd_birth <- function(country, sex_format = c("wide", "long")) {
  sex_format <- match.arg(sex_format)
  country_labels <- names(country) %||% country
  out <- map(1:length(country), function(i) {
    read_hmd_file(
      country[i],
      "Births.txt",
      country_labels[i]
    )
  })
  res <- do.call("rbind", out)
  colnames(res) <- c("year", "birth_female", "birth_male", "birth_total", "country")
  if (sex_format == "wide") {
    new_humble(res)
  } else {
    new_humble(rbind_sex(res, "birth"))
  }
}

#' @rdname hmd_data
##' @export
hmd_life_table <- function(country, sex_format = c("wide", "long"), age_range = 1, year_range = 1) {
  sex_format <- match.arg(sex_format)
  period <- paste0(age_range, "x", year_range)
  country_labels <- names(country) %||% country
  year_name <- ifelse(year_range > 1, paste0("year_range_", year_range), "year")
  age_name <- ifelse(age_range > 1, paste0("age_range_", age_range), "age")

  # total
  out <- map(1:length(country), function(i) {
    read_hmd_file(
      country[i],
      paste0("bltper_", period, ".txt"),
      country_labels[i]
    )
  })
  total <- do.call("rbind", out)
  idx <- 3:(ncol(total) - 1)

  cnames <- colnames(total)[idx]
  colnames(total)[1:2] <- c(year_name, age_name)
  colnames(total)[idx] <- paste0(cnames, "_total")
  # female
  out <- map(1:length(country), function(i) {
    read_hmd_file(
      country[i],
      paste0("mltper_", period, ".txt"),
      country_labels[i]
    )
  })
  male <- do.call("rbind", out)
  colnames(male)[1:2] <- c(year_name, age_name)
  colnames(male)[idx] <- paste0(colnames(male)[idx], "_male")
  # female
  out <- map(1:length(country), function(i) {
    read_hmd_file(
      country[i],
      paste0("fltper_", period, ".txt"),
      country_labels[i]
    )
  })
  female <- do.call("rbind", out)
  colnames(female)[1:2] <- c(year_name, age_name)
  colnames(female)[idx] <- paste0(colnames(female)[idx], "_female")

  res <- merge(merge(female, male), total)
  if (sex_format == "wide") {
    new_humble(res, intervals = c(year_name, age_name))
  } else {
    new_humble(rbind_sex(res, cnames), intervals = c(year_name, age_name))
  }
}

#' @rdname hmd_data
##' @export
hmd_population <- function(country, sex_format = c("wide", "long"), age_range = 1) {
  sex_format <- match.arg(sex_format)
  country_labels <- names(country) %||% country
  appendix <- ifelse(age_range == 1, "", age_range)
  out <- map(1:length(country), function(i) {
    read_hmd_file(
      country[i],
      paste0("Population", appendix),
      country_labels[i]
    )
  })
  res <- do.call("rbind", out)
  age_name <- ifelse(age_range > 1, paste0("age_range_", age_range), "age")
  colnames(res) <- c(
    "year", age_name,
    "pop_female", "pop_male", "pop_total", "country"
  )
  if (sex_format == "wide") {
    new_humble(res, intervals = age_name)
  } else {
    new_humble(rbind_sex(res, "pop"), intervals = age_name)
  }
}

hmd_death <- function(country, sex_format = c("wide", "long"), age_range = 1, year_range = 1) {
  sex_format <- match.arg(sex_format)
  period <- paste0(age_range, "x", year_range)
  country_labels <- names(country) %||% country
  out <- map(1:length(country), function(i) {
    read_hmd_file(
      country[i],
      paste0("Deaths_", period, ".txt"),
      country_labels[i]
    )
  })
  res <- do.call("rbind", out)
  year_name <- ifelse(year_range > 1, paste0("year_range_", year_range), "year")
  age_name <- ifelse(age_range > 1, paste0("age_range_", age_range), "age")
  colnames(res) <- c(
    year_name, age_name,
    "death_female", "death_male", "death_total", "country"
  )
  if (sex_format == "wide") {
    new_humble(res, intervals = c(year_name, age_name))
  } else {
    new_humble(rbind_sex(res, "death"), intervals = c(year_name, age_name))
  }
}

hmd_exposure_to_risk <- function(country, sex_format = c("wide", "long"), age_range = 1, year_range = 1) {
  sex_format <- match.arg(sex_format)
  period <- paste0(age_range, "x", year_range)
  country_labels <- names(country) %||% country
  out <- map(1:length(country), function(i) {
    read_hmd_file(
      country[i],
      paste0("Exposures_", period, ".txt"),
      country_labels[i]
    )
  })
  res <- do.call("rbind", out)
  year_name <- ifelse(year_range > 1, paste0("year_range_", year_range), "year")
  age_name <- ifelse(age_range > 1, paste0("age_range_", age_range), "age")
  colnames(res) <- c(
    year_name, age_name,
    "exprisk_female", "exprisk_male", "exprisk_total", "country"
  )
  if (sex_format == "wide") {
    new_humble(res, intervals = c(year_name, age_name))
  } else {
    new_humble(rbind_sex(res, "exprisk"), intervals = c(year_name, age_name))
  }
}

hmd_death_rate <- function(country, sex_format = c("wide", "long"), age_range = 1, year_range = 1) {
  sex_format <- match.arg(sex_format)
  period <- paste0(age_range, "x", year_range)
  country_labels <- names(country) %||% country
  out <- map(1:length(country), function(i) {
    read_hmd_file(
      country[i],
      paste0("Mx_", period, ".txt"),
      country_labels[i]
    )
  })
  res <- do.call("rbind", out)
  year_name <- ifelse(year_range > 1, paste0("year_range_", year_range), "year")
  age_name <- ifelse(age_range > 1, paste0("age_range_", age_range), "age")
  colnames(res) <- c(
    year_name, age_name,
    "deathrate_female", "deathrate_male", "deathrate_total", "country"
  )
  if (sex_format == "wide") {
    new_humble(res, intervals = c(year_name, age_name))
  } else {
    new_humble(rbind_sex(res, "deathrate"), intervals = c(year_name, age_name))
  }
}

read_hmd_file <- function(country, filename, label = country) {
  check_hmd_session()

  url <- url_mortality(country, filename)
  if (is.null(.hmd$username) | is.null(.hmd$password)) {
    abort("Use `hmd_session()` to set the username and password")
  }
  con <- curl::curl(url, handle = hmd_handle(.hmd$username, .hmd$password))
  open(con)
  data <- utils::read.table(con, skip = 2, header = TRUE, na.strings = ".")
  close(con)
  if ("Age" %in% colnames(data) && !any(grepl("-", data$Age))) {
    data$Age[data$Age == "110+"] <- "110"
    data$Age <- as.integer(data$Age)
  }
  data$country <- label
  data
}

url_mortality <- function(country, filename) {
  paste0("https://former.mortality.org/hmd/", country, "/STATS/", filename)
}

check_hmd_session <- function() {
  if (is.null(.hmd$username) | is.null(.hmd$username)) {
    hmd_session()
    if (is.null(.hmd$username) | is.null(.hmd$username)) {
      abort("You need to supply your HMD username or password with `hmd_session()`")
    }
  }
}
