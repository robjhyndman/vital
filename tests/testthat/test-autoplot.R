test_that("autoplot() plots a vital without an age variable against time", {
  expect_no_warning(p <- autoplot(norway_births, Births))
  built <- ggplot2::ggplot_build(p)
  expect_identical(p$labels$x, "Year [1Y]")
  # One line for each sex
  expect_identical(length(unique(built$data[[1]]$group)), 3L)
  expect_identical(NROW(built$data[[1]]), NROW(norway_births))
  # Series are identified by all keys, including age group labels
  stmf <- read_stmf_files("AUSstmfout.csv")
  p_stmf <- autoplot(stmf, Deaths)
  built <- ggplot2::ggplot_build(p_stmf)
  expect_identical(length(unique(built$data[[1]]$group)), 18L)
  expect_identical(p_stmf$labels$colour, "Sex/Age_group")
  # Several variables are plotted in separate rows of panels
  built <- ggplot2::ggplot_build(autoplot(stmf, ggplot2::vars(Deaths, Mortality)))
  expect_identical(NROW(built$layout$layout), 2L)
})
