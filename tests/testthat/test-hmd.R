# Check reading in HMD files

test_that("read deaths", {
  z <- read_hmd("bltper_1x1.txt")
  expect_equal(dim(z), c(10656L, 11L))
  expect_true(abs(z$ex[10656] - 1.33) < 1e-5)
})
test_that("read deaths", {
  z <- read_hmd("Deaths_5x10.txt")
  expect_equal(dim(z), c(720L, 5L))
  expect_true(abs(z$Deaths[200] - 4177.11) < 1e-8)
})
test_that("read deaths", {
  z <- read_hmd("E0per.txt")
  expect_equal(dim(z), c(288L, 3L))
  expect_true(abs(z$Life_expectancy[200] - 62.91) < 1e-8)
})
test_that("read deaths", {
  z <- read_hmd("Exposures_1x10.txt")
  expect_equal(dim(z), c(3330L, 5L))
  expect_true(abs(z$Exposure[1000] - 1132518.4) < 1e-1)
})
test_that("read deaths", {
  z <- read_hmd("fltper_1x1.txt")
  expect_equal(dim(z), c(10656L, 11L))
  expect_true(abs(z$ex[1060] - 58.78) < 1e-5)
})
test_that("read deaths", {
  z <- read_hmd("Mx_1x1.txt")
  expect_equal(dim(z), c(31968L, 5L))
  expect_equal(
    as.character(z[20000, ]),
    c("1952", "69", "FALSE", "Male", "0.052076")
  )
})
test_that("read deaths", {
  z <- read_hmd("Mx_5x10.txt")
  expect_equal(dim(z), c(720L, 5L))
})
test_that("read deaths", {
  z <- read_hmd("Population.txt")
  expect_equal(dim(z), c(31968L, 5L))
  expect_equal(
    as.character(z[10000, ]),
    c("1936", "34", "FALSE", "Total", "98748.77")
  )
})
