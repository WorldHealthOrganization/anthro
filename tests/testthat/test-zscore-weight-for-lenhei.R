context("zscores - weight for lenhei")

test_that("it returns NA if weight is <= 0", {
  observed <- anthro_zscore_weight_for_lenhei(
    weight = c(-1, 0),
    lenhei = 40,
    lenhei_unit = "l",
    age_in_days = 100,
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
    sex = c(2, 2),
    oedema = c("n", "n")
  )
  expect_true(all(is.na(observed$zwfl)))
  expect_true(all(is.na(observed$fwfl)))
  expect_true(is.numeric(observed$zwfl))
})
