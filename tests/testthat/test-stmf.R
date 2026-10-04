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

test_that("read_stmf finds the file for each country", {
  local_mocked_bindings(
    hmd_download = function(url, username, password) url,
    read_stmf_files = function(file) file
  )
  expect_match(read_stmf("Norway", "user", "pass"), "NORstmfout.csv$")
  expect_match(read_stmf("NOR", "user", "pass"), "NORstmfout.csv$")
  expect_error(read_stmf("Belarus", "user", "pass"), "No STMF data available")
  expect_error(read_stmf("Narnia", "user", "pass"), "Unknown country")
})

test_that("read_stmf reads the downloaded file", {
  local_mocked_bindings(
    hmd_download = function(url, username, password) "AUSstmfout.csv"
  )
  expect_identical(
    read_stmf("AUS", "user", "pass"),
    read_stmf_files("AUSstmfout.csv")
  )
})

test_that("read_stmf gives a clear error when the HMD login fails", {
  skip_on_cran()
  skip_if_offline()
  expect_error(
    read_stmf("NOR", "fred@example.com", "wrongpassword"),
    "Check your username and password"
  )
})
