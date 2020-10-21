context("zscores - bmi for age")
describe("anthro_zscore_bmi_for_age", {
  it("fails if bmi is not numeric", {
    expect_error(anthro_zscore_bmi_for_age(
      "50", 44,
      to_months(44), 1, "n"
    ), "measure")
  })
  it("fails if age is not numeric", {
    expect_error(anthro_zscore_bmi_for_age(
      50, "44",
      to_months(44), 1, "n"
    ), "age")
  })
  it("fails if sex is not 1 or 2", {
    expect_error(anthro_zscore_bmi_for_age(
      50, 44,
      to_months(44), 3, "n"
    ), "sex")
    expect_error(anthro_zscore_bmi_for_age(
      50, 44,
      to_months(44), "F", "n"
    ), "sex")
  })
  it("sets result to NA if oedema is y", {
    expect_true(is.na(anthro_zscore_bmi_for_age(
      50, 44,
      to_months(44), 1, "y"
    )$zbmi))
  })
  it("has a default flag threshold of -6;5", {
    observed <- anthro_zscore_bmi_for_age(25, 1, to_months(1), 2, "n")
    expect_equal(observed$fbmi, 1L)
    observed <- anthro_zscore_bmi_for_age(10.37, 1, to_months(1), 2, "n",
      flag_threshold = c(-0.1, 0.1)
    )
    expect_equal(observed$fbmi, 1L)
  })
  it("does not compute zscores where no growthstandards are present", {
    observed <- anthro_zscore_bmi_for_age(
      c(50, 60),
      c(1, 1857),
      to_months(c(1, 1857)),
      c(1, 1),
      c("n", "n")
    )
    expect_true(is.data.frame(observed))
    expect_equal(is.na(observed$zbmi), c(FALSE, TRUE))
    expect_equal(is.na(observed$fbmi), c(FALSE, TRUE))
  })
  it("ignores zscores for age < 0 and age >= 1856", {
    observed <- anthro_zscore_bmi_for_age(50, 1857, to_months(1857), 1, "n")
    expect_true(is.na(observed$zbmi))
    expect_true(is.na(observed$fbmi))
    observed <- anthro_zscore_bmi_for_age(50, -1, to_months(1857), 1, "n")
    expect_true(is.na(observed$zbmi))
    expect_true(is.na(observed$fbmi))
  })
})

test_that("it returns NA if headc is <= 0", {
  observed <- anthro_zscore_bmi_for_age(
    bmi = c(-1, 0),
    age_in_days = c(100, 100),
    age_in_months = to_months(c(100, 100)),
    sex = c(1, 1),
    oedema = c("n", "n")
  )
  expect_true(all(is.na(observed$zbmi)))
  expect_true(all(is.na(observed$fbmi)))
  expect_true(is.numeric(observed$zbmi))
})
