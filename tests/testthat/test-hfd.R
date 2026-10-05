test_that("read_hfd_files reads fertility rates", {
  z <- read_hfd_files("NORasfrRR.txt")
  expect_s3_class(z, "vital")
  expect_identical(colnames(z), c("Year", "Age", "ASFR", "OpenInterval"))
  expect_identical(vital_vars(z), c(age = "Age"))
  expect_identical(range(z$Age), c(12L, 55L))
  expect_false(anyNA(z$ASFR))
})

test_that("read_hfd reads the requested variables", {
  requested <- list()
  local_mocked_bindings(
    readHFDweb = function(CNTRY, item, username, password, fixup) {
      requested[[length(requested) + 1]] <<- c(CNTRY, item)
      HMDHFDplus::readHFD(test_path(paste0("NOR", item, ".txt")), fixup = fixup)
    },
    .package = "HMDHFDplus"
  )
  z <- read_hfd("NOR", "user", "pass")
  expect_identical(requested, list(c("NOR", "asfrRR")))
  expect_identical(z, read_hfd_files("NORasfrRR.txt"))
})
