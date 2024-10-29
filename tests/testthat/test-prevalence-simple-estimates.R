test_that("survey and approximation yield are equal within tolerance", {
  set.seed(1)
  data <- data.frame(
    sex = sample(1:2, 100, replace = TRUE),
    age = sample(0:50, 100, replace = TRUE),
    weight = pmax(rnorm(100, 20, 10), 1),
    lenhei = pmax(rnorm(100, 80, 20), 30)
  )
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

describe("sample size", {
  it("counts non na values", {
    res <- sample_size(c(1, 2, NA_real_))
    expect_s3_class(res, "data.frame")
    expect_equal(res$pop, 2)
    expect_equal(res$unwpop, 2)
    expect_false(is.integer(res$pop))
    expect_false(is.integer(res$unwpop))
  })
})
