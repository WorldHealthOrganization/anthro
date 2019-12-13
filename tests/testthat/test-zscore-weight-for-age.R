context("zscores - weight for age")
describe("anthro_zscore_weight_for_age", {
  it("fails if weight is not numeric", {
    expect_error(anthro_zscore_weight_for_age("50", 44, 1, "n"), "measure")
  })
  it("fails if age is not numeric", {
    expect_error(anthro_zscore_weight_for_age(50, "44", 1, "n"), "age")
  })
  it("fails if sex is not 1 or 2", {
    expect_error(anthro_zscore_weight_for_age(50, 44, 3, "n"), "sex")
    expect_error(anthro_zscore_weight_for_age(50, 44, "F", "n"), "sex")
  })
  it("sets result to NA if oedema is y", {
    expect_true(is.na(anthro_zscore_weight_for_age(50, 44, 1, "y")$zwei))
  })
  it("computes the z-score for length for age", {
    observed <- anthro_zscore_weight_for_age(17, 1522, 2, "n")
    expected <- 0.24
    expect_true(is.data.frame(observed))
    expect_equal(observed$zwei, expected, tolerance = 1e-8)
    expect_equal(observed$fwei, 0L)
  })
  it("has a default flag threshold of -6;5", {
    observed <- anthro_zscore_weight_for_age(10.37, 1, 2, "n")
    expect_equal(observed$fwei, 1L)
    observed <- anthro_zscore_weight_for_age(17, 1522, 2, "n",
      flag_threshold = c(-0.1, 0.1)
    )
    expect_equal(observed$fwei, 1L)
  })
  it("does not compute zscores where no growthstandards are present", {
    observed <- anthro_zscore_weight_for_age(
      c(50, 60),
      c(1, 1857),
      c(1, 1),
      c("n", "n")
    )
    expect_true(is.data.frame(observed))
    expect_equal(is.na(observed$zwei), c(FALSE, TRUE))
    expect_equal(is.na(observed$fwei), c(FALSE, TRUE))
  })
  it("ignores zscores for age < 0 and age >= 1856", {
    observed <- anthro_zscore_weight_for_age(50, 1857, 1, "n")
    expect_true(is.na(observed$zwei))
    expect_true(is.na(observed$fwei))
    observed <- anthro_zscore_weight_for_age(50, -1, 1, "n")
    expect_true(is.na(observed$zwei))
    expect_true(is.na(observed$fwei))
  })
  it("returns NA if weight is <= 0", {
    observed <- anthro_zscore_weight_for_age(
      c(0, -1), c(10, 10),
      c(1, 1), c("n", "n")
    )
    expect_true(all(is.na(observed$zwei)))
    expect_true(all(is.na(observed$fwei)))
    expect_true(is.numeric(observed$zwei))
  })
})
