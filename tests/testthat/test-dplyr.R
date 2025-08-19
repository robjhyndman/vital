# Small data set for testing purposes
nor_mortality <- norway_mortality |>
  filter(
    Year <= 1903,
    Age > 80
  )

test_that("classes", {
  expect_s3_class(nor_mortality |> filter(Sex == "Male"), "vital")
  expect_s3_class(nor_mortality |> select(Population), "vital")
  expect_s3_class(nor_mortality |> slice(10), "vital")
  expect_s3_class(
    nor_mortality |> arrange(Age, Sex, Year),
    "vital"
  )
  expect_s3_class(nor_mortality |> mutate(mx = Deaths / Population), "vital")
  expect_s3_class(nor_mortality |> head(10), "vital")
  expect_s3_class(nor_mortality |> tail(100), "vital")
  expect_s3_class(
    bind_rows(
      nor_mortality |> filter(Year == 1901),
      nor_mortality |> filter(Year == 1902)
    ),
    "vital"
  )
  expect_s3_class(
    bind_cols(
      nor_mortality |> select(-Population),
      Exposure = nor_mortality$Population
    ),
    "vital"
  )
  expect_s3_class(nor_mortality |> transmute(mx = Deaths / Population), "vital")
  expect_s3_class(nor_mortality |> relocate(Year, Population, Age), "vital")
  expect_s3_class(nor_mortality |> summarise(Deaths = mean(Deaths)), "vital")
  expect_s3_class(
    nor_mortality |> group_by(Sex) |> summarise(Deaths = mean(Deaths)),
    "vital"
  )
  expect_s3_class(nor_mortality |> group_by(Sex) |> ungroup(), "vital")
  expect_s3_class(
    left_join(
      nor_mortality |> select(Population),
      nor_mortality |> select(Deaths)
    ),
    "vital"
  )
})
