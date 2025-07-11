# Check reading in HMD files

test_that("read_hmd_files", {
  # Incorrect username/password
  expect_error(read_hmd("AUS", "fred@gmail.com", "password"))
  # Read all files
  z <- read_hmd_files(c(
    "Deaths_1x1.txt",
    "Exposures_1x1.txt",
    "Population.txt",
    "Mx_1x1.txt"
  ))
  expect_identical(dim(z), c(33300L, 8L))
  expect_identical(
    colnames(z),
    c(
      "Year",
      "Age",
      "OpenInterval",
      "Sex",
      "Deaths",
      "Exposures",
      "Population",
      "Mortality"
    )
  )
  expect_true(tsibble::is_tsibble(z))
  # Read subset of files
  z <- read_hmd_files(c("Exposures_1x1.txt", "Population.txt", "Mx_1x1.txt"))
  expect_identical(dim(z), c(33300L, 7L))
  # Read 1 file
  z <- read_hmd_files("Mx_1x1.txt")
  expect_identical(dim(z), c(33300L, 5L))
  # Read 0 files
  expect_error(read_hmd_files())
})
