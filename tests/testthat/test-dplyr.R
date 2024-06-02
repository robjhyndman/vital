# Small data set for testing purposes
aus_mortality <- aus_mortality  |>
  filter(Year <= 1903,
  Sex == "female",
  State %in% c("Victoria", "Tasmania"),
  Age > 80)

test_that("classes", {
  expect_s3_class(aus_mortality |> filter(State == "Victoria"), "vital")
  expect_s3_class(aus_mortality |> select(Exposure), "vital")
  expect_s3_class(aus_mortality |> slice(10), "vital")
  expect_s3_class(aus_mortality |> arrange(Age, Sex, State, Code, Year), "vital")
  expect_s3_class(aus_mortality |> mutate(mx = Exposure/Deaths), "vital")
  expect_s3_class(aus_mortality |> head(10), "vital")
  expect_s3_class(aus_mortality |> tail(100), "vital")
  expect_s3_class(bind_rows(aus_mortality |> filter(Year == 1901),
      aus_mortality |> filter(Year == 1902)), "vital")
  expect_s3_class(bind_cols(aus_mortality |> select(-Exposure),
      Exposure = aus_mortality$Exposure), "vital")
  expect_s3_class(aus_mortality |> transmute(mx = Exposure/Deaths), "vital")
  expect_s3_class(aus_mortality |> relocate(Year, Exposure, Age), "vital")
  expect_s3_class(aus_mortality |> summarise(Deaths = mean(Deaths)), "vital")
  expect_s3_class(aus_mortality |> group_by(Sex) |> summarise(Deaths = mean(Deaths)), "vital")
  expect_s3_class(aus_mortality |> group_by(Sex) |> ungroup(), "vital")
  expect_s3_class(left_join(aus_mortality |> select(Exposure),  aus_mortality |> select(Deaths)), "vital")
})
