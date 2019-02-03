context("utils")

test_that("standardize_oedema_var: all other values become 'n'", {
  expect_equal("n", standardize_oedema_var("test"))
  expect_equal("n", standardize_oedema_var(5))
  expect_equal("n", standardize_oedema_var(NA_character_))
})

test_that("standardize_oedema_var trims white spaces", {
  expect_equal("y", standardize_oedema_var(" y   "))
})

test_that("standardize_oedema_var replaces numerics by characters", {
  expect_equal("y", standardize_oedema_var(1))
  expect_equal("y", standardize_oedema_var("1"))
  expect_equal("n", standardize_oedema_var(2))
  expect_equal("n", standardize_oedema_var("2"))
})