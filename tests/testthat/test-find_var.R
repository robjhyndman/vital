test_that("find_key gives a readable error", {
  expect_error(
    find_key(norway_births, c("age", "age_group")),
    "No key variable found with name in: age, age_group$"
  )
})
