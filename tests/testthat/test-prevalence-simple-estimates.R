test_that("survey and approximation results are equal within tolerance", {
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

test_that("survey and approximation with sw results are equal within tolerance", {
  set.seed(1)
  n <- 200
  data <- data.frame(
    sex = sample(1:2, n, replace = TRUE),
    age = sample(0:50, n, replace = TRUE),
    weight = pmax(rnorm(n, 20, 10), 1),
    lenhei = pmax(rnorm(n, 80, 20), 30)
  )
  data$sw <- runif(nrow(data)) / nrow(data)
  data$weight[43] <- NA_real_
  res_simple <- anthro_prevalence(
    data$sex,
    data$age,
    is_age_in_month = TRUE,
    weight = data$weight,
    lenhei = data$lenhei,
    sw = data$sw
  )
  opts <- options("anthro.internal.compute_method" = "survey")
  on.exit(options(opts))
  res_survey <- anthro_prevalence(
    data$sex,
    data$age,
    is_age_in_month = TRUE,
    weight = data$weight,
    lenhei = data$lenhei,
    sw = data$sw
  )
  expect_equal(colnames(res_simple), colnames(res_survey))
  expect_equal(nrow(res_simple), nrow(res_survey))
  for (col in colnames(res_simple)) {
    expect_equal(res_simple[[!!col]], res_survey[[!!col]], tolerance = 0.0001)
  }
})

test_that("sw > 0 is approximated correctly within tolerance", {
  res <- anthro_prevalence(
    sex = c(1, 2, 1),
    age = c(50, 50, NA_real_),
    is_age_in_month = TRUE,
    weight = 80,
    lenhei = 100,
    sw = 0.05
  )
  expect_true(all(!is.na(res$HAZ_pop)))
})

test_that("sw > 0 is approximated correctly within tolerance", {
  set.seed(1)
  data <- data.frame(
    sex = sample(1:2, 100, replace = TRUE),
    age = sample(0:50, 100, replace = TRUE),
    weight = pmax(rnorm(100, 20, 10), 1),
    lenhei = pmax(rnorm(100, 80, 20), 30)
  )
  data$sw <- runif(nrow(data)) / nrow(data)
  res <- anthro_prevalence(
    data$sex,
    data$age,
    is_age_in_month = TRUE,
    weight = data$weight,
    lenhei = data$lenhei,
    sw = data$sw
  )
  zscores <- anthro_zscores(
    data$sex,
    data$age,
    is_age_in_month = TRUE,
    weight = data$weight,
    lenhei = data$lenhei
  )
  zscores <- cbind(zscores, data)
  zscores$zlen <- ifelse(zscores$flen == 1, NA_real_, zscores$zlen)
  design <- survey::svydesign(
    ids = ~1,
    data = zscores,
    nest = TRUE,
    weights = ~sw
  )
  design_unweighted <- survey::svydesign(
    ids = ~1,
    data = zscores,
    nest = TRUE,
    weights = NULL
  )
  expected <- survey::svyby(
    ~zlen,
    ~sex,
    design,
    survey::svymean,
    na.rm = TRUE,
    na.rm.all = TRUE,
    drop.empty.groups = FALSE
  )
  expected_cis <- confint(
    expected,
    df = degf(design),
    level = 1 - prevalence_significance_level
  )
  observed <- res[c(8, 9), c("Group", "HA_r", "HA_se", "HA_ll", "HA_ul")]
  expect_equal(
    observed$HA_r,
    rev(expected$zlen)
  )
  expect_equal(
    observed$HA_se,
    rev(expected$se)
  )
  expect_equal(
    observed$HA_ll,
    rev(as.numeric(expected_cis[, 1]))
  )
  expect_equal(
    observed$HA_ul,
    rev(as.numeric(expected_cis[, 2]))
  )

  observed <- res[1, c("Group", "HA_r", "HA_se")]
  expected_zlen_all <- survey::svymean(
    ~zlen,
    design,
    na.rm = TRUE,
    na.rm.all = TRUE,
    drop.empty.groups = FALSE
  )
  expect_equal(
    as.numeric(observed[2:3]),
    c(as.numeric(expected_zlen_all), as.numeric(survey::SE(expected_zlen_all)))
  )
  expected_total_weighted <- survey::svytotal(
    ~ I(!is.na(zlen)),
    design,
    na.rm = TRUE,
    na.rm.all = TRUE,
    drop.empty.groups = FALSE
  )
  expected_total_unweighted <- survey::svytotal(
    ~ I(!is.na(zlen)),
    design_unweighted,
    na.rm = TRUE,
    na.rm.all = TRUE,
    drop.empty.groups = FALSE
  )
  observed <- res[1, 2:3, drop = TRUE]
  expect_equal(
    observed$HAZ_pop,
    as.numeric(expected_total_weighted[2])
  )
  expect_equal(
    observed$HAZ_unwpop,
    as.numeric(expected_total_unweighted[2])
  )
})

test_that("zscore estimates should not overflow", {
  x <- rnorm(100000)
  expect_no_warning(
    zscore_estimate(x, length(x), data.frame())
  )
  expect_no_warning(
    zscore_estimate_weighted(x, length(x), rep.int(1, length(x)), data.frame())
  )
})
