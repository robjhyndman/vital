# Check reading in HMD files

test_that("read deaths", {
  z <- read_hmd("bltper_1x1.txt")
  expect_equal(dim(z), c(720L,5L))
}
test_that("read deaths", {
  z <- read_hmd("Deaths_5x10.txt")
  expect_equal(dim(z), c(720L,5L))
}
test_that("read deaths", {
  z <- read_hmd("E0per.txt")
  expect_equal(dim(z), c(720L,5L))
}
test_that("read deaths", {
  z <- read_hmd("Exposures_1x10.txt")
  expect_equal(dim(z), c(720L,5L))
}
test_that("read deaths", {
  z <- read_hmd("fltper_1x1.txt")
  expect_equal(dim(z), c(720L,5L))
}
test_that("read deaths", {
  z <- read_hmd("Mx_1x1.txt")
  expect_equal(dim(z), c(720L,5L))
}
test_that("read deaths", {
  z <- read_hmd("Mx_5x10.txt")
  expect_equal(dim(z), c(720L,5L))
}
test_that("read deaths", {
  z <- read_hmd("Population.txt")
  expect_equal(dim(z), c(720L,5L))
}

