context("zscores - arm circumference-for-age")

test_that("it returns NA if armc is <= 0", {
  observed <- anthro_zscore_arm_circumference_for_age(
    armc = c(-1, 0),
    age_in_days = c(100, 100),
    age_in_months = to_months(c(100, 100)),
    sex = c(2, 2)
  )
  expect_true(all(is.na(observed$zac)))
  expect_true(all(is.na(observed$fac)))
  expect_true(is.numeric(observed$zac))
})
