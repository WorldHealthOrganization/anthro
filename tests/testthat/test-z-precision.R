test_that("z_precision accepts more decimal places and affects z-scores", {
  res2 <- anthro_zscores(sex = "f", age = 10, is_age_in_month = TRUE, weight = 10, z_precision = 2L)
  res4 <- anthro_zscores(sex = "f", age = 10, is_age_in_month = TRUE, weight = 10, z_precision = 4L)

  expect_true(!identical(res2$zwei, res4$zwei))
  expect_equal(res4$zwei, round(res4$zwei, 4))
})

test_that("z_precision = 0 returns integer z-scores (rounded to 0 digits)", {
  res0 <- anthro_zscores(sex = "f", age = 10, is_age_in_month = TRUE, weight = 10, z_precision = 0L)
  expect_equal(res0$zwei, round(res0$zwei, 0))
})

test_that("negative z_precision is rejected", {
  expect_error(anthro_zscores(sex = "f", age = 10, is_age_in_month = TRUE, weight = 10, z_precision = -1L))
})

test_that("non-integer numeric z_precision is accepted and coerced to integer", {
  res3 <- anthro_zscores(sex = "f", age = 10, is_age_in_month = TRUE, weight = 10, z_precision = 3)
  expect_equal(res3$zwei, round(res3$zwei, 3))
})
