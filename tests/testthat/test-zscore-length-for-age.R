context("zscores - length for age")
describe("anthro_zscore_length_for_age", {
  it("fails if lenhei is not numeric", {
    expect_error(anthro_zscore_length_for_age("50", 44, to_months(44), 1))
  })
  it("fails if age is not numeric", {
    expect_error(anthro_zscore_length_for_age(50, "44", to_months(44), 1))
  })
  it("fails if sex is not 1 or 2", {
    expect_error(anthro_zscore_length_for_age(50, 44, to_months(44), 3))
    expect_error(anthro_zscore_length_for_age(50, 44, to_months(44), "F"))
  })
  it("computes the z-score for length for age", {
    observed <- anthro_zscore_length_for_age(50, 44, to_months(44), 1)
    expected <- round(((50 / 56.4833)^1 - 1) / (0.03492 * 1), digits = 2L)
    expect_true(is.data.frame(observed))
    expect_equal(observed$zlen, expected, tolerance = 1e-8)
    expect_equal(observed$flen, 0L)
  })
  it("has a default flag threshold of 6", {
    observed <- anthro_zscore_length_for_age(50, 44, to_months(44), 1)
    expect_equal(observed$flen, 0L)
    observed <- anthro_zscore_length_for_age(50, 44, to_months(44), 1,
      flag_threshold = 2
    )
    expect_equal(observed$flen, 1L)
  })
  it("does not compute zscores where no growthstandards are present", {
    observed <- anthro_zscore_length_for_age(
      c(50, 60),
      c(1, 1857),
      to_months(c(1, 1857)),
      c(1, 1)
    )
    expect_true(is.data.frame(observed))
    expect_equal(is.na(observed$zlen), c(FALSE, TRUE))
    expect_equal(is.na(observed$flen), c(FALSE, TRUE))
  })
  it("ignores zscores for age < 0 and age >= 1856", {
    observed <- anthro_zscore_length_for_age(50, 1857, to_months(1857), 1)
    expect_true(is.na(observed$zlen))
    expect_true(is.na(observed$flen))
    observed <- anthro_zscore_length_for_age(50, -1, to_months(1857), 1)
    expect_true(is.na(observed$zlen))
    expect_true(is.na(observed$flen))
  })
  it("returns NA if weight is <= 0", {
    observed <- anthro_zscore_length_for_age(c(0, -1), 10, to_months(10), 1)
    expect_true(all(is.na(observed$zlen)))
    expect_true(all(is.na(observed$flen)))
    expect_true(is.numeric(observed$zlen))
  })
})
