context("zscores - head circumference-for-age")

test_that("it returns NA if headc is <= 0", {
  observed <- anthro_zscore_head_circumference_for_age(
    headc = c(-1, 0),
    age_in_days = c(100, 100),
    sex = c(2, 2)
  )
  expect_true(all(is.na(observed$zhc)))
  expect_true(all(is.na(observed$fhc)))
  expect_true(is.numeric(observed$zhc))
})
