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
  expect_error(read_stmf_files(), "argument \"file\" is missing")
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

test_that("hmd_download logs in and saves the file, or reports a failed login", {
  submitted <- NULL
  mock_rvest <- function(content_type, content) {
    local_mocked_bindings(
      session = function(url) list(url = url),
      html_form = function(x) {
        list(list(fields = list(`__RequestVerificationToken` = list(value = "token"))))
      },
      html_form_set = function(form, ...) {
        submitted <<- list(...)
        form
      },
      session_submit = function(x, form) x,
      session_jump_to = function(x, url) {
        list(response = list(headers = list(`content-type` = content_type), content = content))
      },
      .package = "rvest",
      .env = parent.frame()
    )
  }
  mock_rvest("text/csv", charToRaw("a,b\n1,2\n"))
  file <- hmd_download("https://example.com/x.csv", "user@example.com", "secret")
  expect_identical(readLines(file), c("a,b", "1,2"))
  expect_identical(submitted$Email, "user@example.com")
  expect_identical(submitted$Password, "secret")
  expect_identical(submitted$`__RequestVerificationToken`, "token")
  # A failed login returns the HTML login page
  mock_rvest("text/html; charset=utf-8", charToRaw("<html></html>"))
  expect_error(
    hmd_download("https://example.com/x.csv", "user@example.com", "wrong"),
    "Check your username and password"
  )
})
