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

test_that("rounding up works", {
  expect_equal(round_up(c(NA_real_, 730.49, 730.5)), c(NA_real_, 730, 731))
  expect_equal(round_up(NA_real_), NA_real_)
  expect_equal(round_up(73.5), 74)
  expect_equal(round_up(numeric()), numeric())
  expect_error(round_up("730"))
  expect_error(round_up(-1))
})

test_that("adjusting lenhei uses rounded age in days", {
  expect_equal(77.5, adjust_lenhei(730.5, "h", 77.5))
})
