context("zscores - subscapular skinfold-for-age")

test_that("it returns NA if subskin is <= 0", {
  observed <- anthro_zscore_subscapular_skinfold_for_age(
    subskin = c(-1, 0),
    age_in_days = c(100, 100),
    age_in_months = to_months(c(100, 100)),
    sex = c(2, 2)
  )
  expect_true(all(is.na(observed$zss)))
  expect_true(all(is.na(observed$fss)))
  expect_true(is.numeric(observed$zss))
})
