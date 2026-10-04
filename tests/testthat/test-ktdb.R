# Check reading in ktdb files

test_that("read_ktdb_files", {
  # Read 2 files
  z <- read_ktdb_files("maustl.txt", "faustl.txt")
  expect_identical(dim(z), c(4794L, 7L))
  expect_identical(
    colnames(z),
    c("Year", "Age", "Triangle", "Cohort", "Population", "Deaths", "Sex")
  )
  expect_true(tsibble::is_tsibble(z))
  # Read 1 file
  z <- read_ktdb_files("maustl.txt")
  expect_identical(dim(z), c(2397L, 7L))
  expect_identical(
    colnames(z),
    c("Year", "Age", "Triangle", "Cohort", "Population", "Deaths", "Sex")
  )
  expect_true(tsibble::is_tsibble(z))
  # Read 0 files
  expect_error(read_ktdb_files(), "At least one of")
  # Test different Triangle
  z <- read_ktdb_files("maustl.txt", "faustl.txt", triangle = 2)
  expect_true(all(z$Triangle == 2))
})

test_that("read_ktdb passes triangle to read_ktdb_files", {
  local_mocked_bindings(read_ktdb_files = function(male, female, triangle) {
    triangle
  })
  expect_identical(read_ktdb(1, triangle = 2), 2)
})

test_that("read_ktdb finds the files for each country", {
  local_mocked_bindings(read_ktdb_files = function(male, female, triangle) {
    c(male, female)
  })
  expect_match(read_ktdb(10), "finland/mfinla.txt$", all = FALSE)
  expect_match(read_ktdb("Finland"), "finland/mfinla.txt$", all = FALSE)
  expect_match(read_ktdb(37), "ltu/mltu.txt$", all = FALSE)
  expect_error(read_ktdb(24), "Unknown country code")
  expect_error(read_ktdb("Belarus"), "No K-T data available")
  expect_error(read_ktdb("Narnia"), "Unknown country")
})
