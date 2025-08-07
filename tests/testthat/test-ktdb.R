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
  expect_error(read_ktdb_files())
  # Test different Triangle
  z <- read_ktdb_files("maustl.txt", "faustl.txt", triangle = 2)
  expect_true(all(z$Triangle == 2))
})
