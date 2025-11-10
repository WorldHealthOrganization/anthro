test_that("survey and approximation yield are equal within tolerance", {
  set.seed(1)
  n <- 200
  data <- data.frame(
    sex = sample(1:2, n, replace = TRUE),
    age = sample(0:50, n, replace = TRUE),
    weight = pmax(rnorm(n, 20, 10), 1),
    lenhei = pmax(rnorm(n, 80, 20), 30)
  )
  data$weight[43] <- NA_real_
  res_simple <- anthro_prevalence(
    data$sex,
    data$age,
    is_age_in_month = TRUE,
    weight = data$weight,
    lenhei = data$lenhei
  )
  opts <- options("anthro.internal.compute_method" = "survey")
  on.exit(options(opts))
  res_survey <- anthro_prevalence(
    data$sex,
    data$age,
    is_age_in_month = TRUE,
    weight = data$weight,
    lenhei = data$lenhei
  )
  expect_equal(colnames(res_simple), colnames(res_survey))
  expect_equal(nrow(res_simple), nrow(res_survey))
  for (col in colnames(res_simple)) {
    expect_equal(res_simple[[!!col]], res_survey[[!!col]], tolerance = 0.0001)
  }
})

test_that("zscore estimates should not overflow", {
  x <- rnorm(100000)
  expect_no_warning(
    zscore_estimate(x, length(x), data.frame())
  )
})
