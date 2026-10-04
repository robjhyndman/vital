test_that("read_hfd_files reads fertility rates", {
  z <- read_hfd_files("NORasfrRR.txt")
  expect_s3_class(z, "vital")
  expect_identical(colnames(z), c("Year", "Age", "ASFR", "OpenInterval"))
  expect_identical(vital_vars(z), c(age = "Age"))
  expect_identical(range(z$Age), c(12L, 55L))
  expect_false(anyNA(z$ASFR))
})
