context("zscores - weight for lenhei")

test_that("it returns NA if weight is <= 0", {
  observed <- anthro_zscore_weight_for_lenhei(
    weight = c(-1, 0),
    lenhei = 40,
    lenhei_unit = "l",
    age_in_days = 100,
    age_in_months = to_months(100),
    sex = 2,
    oedema = "n"
  )
  expect_true(all(is.na(observed$zwfl)))
  expect_true(all(is.na(observed$fwfl)))
  expect_true(is.numeric(observed$zwfl))
})

test_that("it returns NA if lenhei is <= 0", {
  observed <- anthro_zscore_weight_for_lenhei(
    weight = c(5, 5),
    lenhei = c(-1, 0),
    lenhei_unit = c("l", "l"),
    age_in_days = c(100, 100),
    age_in_months = to_months(c(100, 100)),
    sex = c(2, 2),
    oedema = c("n", "n")
  )
  expect_true(all(is.na(observed$zwfl)))
  expect_true(all(is.na(observed$fwfl)))
  expect_true(is.numeric(observed$zwfl))
})

test_that("it is only computed if age in month < 60", {
  observed <- anthro_zscore_weight_for_lenhei(
    weight = c(20, 20),
    lenhei = c(80, 80),
    lenhei_unit = c("l", "l"),
    age_in_days = c(age_to_days(60, TRUE), age_to_days(60 - 0.01, TRUE)),
    age_in_months = c(60, 60 - 0.01),
    sex = c(2, 2),
    oedema = c("n", "n")
  )
  expect_equal(is.na(observed$zwfl), c(TRUE, FALSE))
})
