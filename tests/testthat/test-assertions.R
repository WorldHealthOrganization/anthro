context("assertions")

test_that("the correct parameter name is used", {
  test_var <- 1
  expect_error(assert_character(test_var), "test_var")
  expect_error(assert_logical(test_var), "test_var")
  test_var <- "abcded"
  expect_error(assert_numeric(test_var), "test_var")
  test_var <- TRUE
  expect_error(assert_character_or_numeric(test_var), "test_var")
})
