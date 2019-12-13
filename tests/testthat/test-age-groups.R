context("age groups")

test_that("age groups are computed correctly", {
  range <- 0:65
  groups <- anthro_age_groups(range)

  expect_true(is.factor(groups))
  expected_groups <- c(
    "00-05 mo", "06-11 mo", "12-23 mo",
    "24-35 mo", "36-47 mo", "48-59 mo"
  )
  expect_equal(levels(groups), expected_groups)
  expect_equal(as.character(anthro_age_groups(59.9)), "48-59 mo")

  # age 60.5 is not in group 48-59 mo
  expect_true(is.na(anthro_age_groups(60.5)))
  expect_true(is.na(anthro_age_groups(60)))

  # age 61 is NA
  expect_true(is.na(anthro_age_groups(61)))
})
