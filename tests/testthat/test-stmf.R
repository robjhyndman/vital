# Check reading in stmf files

test_that("read_stmf_files", {
  # Read 1 file
  z <- read_stmf_files("AUSstmfout.csv")
  expect_identical(dim(z), c(8838L, 5L))
  expect_identical(
    colnames(z),
    c("YearWeek", "Sex", "Age_group", "Deaths", "Mortality")
  )
  expect_true(tsibble::is_tsibble(z))
  expect_true(inherits(z, "vital"))
  expect_identical(vital_vars(z), c(sex = "Sex", deaths = "Deaths"))
  # Read 0 files
  expect_error(read_stmf_files())
})
