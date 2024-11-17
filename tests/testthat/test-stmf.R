# Check reading in stmf files

test_that("read_stmf_file", {
  # Read 1 file
  z <- read_stmf_file("AUSstmfout.csv")
  expect_equal(dim(z), c(8838L, 5L))
  expect_equal(colnames(z), c("YearWeek", "Age_group", "Sex", "Deaths", "Mortality"))
  expect_true(tsibble::is_tsibble(z))
  # Read 0 files
  expect_error(read_stmf_file())
})

