context("prevalence")

describe("anthro_prevalence()", {
  it("fails if any sampling weight is negative", {
    expect_error(
      anthro_prevalence(
        sex = 1,
        age = 20,
        weight = 20,
        lenhei = 20,
        sw = -1
      ),
      "weights"
    )
  })
  it("fails if no values left for analysis", {
    expect_error(
      expect_warning(anthro_prevalence(
        sex = c(1, 2, 2, 1),
        age = c(100, 100, 100, 100),
        oedema = c("n", "n", "n", "y"),
        is_age_in_month = TRUE,
        weight = c(18, 15, 10, 15),
        lenhei = c(100, 80, 100, 100)
      ), "rows", all = TRUE),
      "removed"
    )
  })
  it("orders the columns by indicator and cutoff", {
    res <- anthro_prevalence(
      sex = c(1, 2, 2, 1),
      age = c(1001, 1000, 1010, 1000),
      weight = c(18, 15, 10, 15),
      lenhei = c(100, 80, 100, 100)
    )
    col_names <- colnames(res)
    expect_equal(col_names[1L:3L], c("Group", "HAZ_pop", "HAZ_unwpop"))
    expect_equal(col_names[4L], c("HA_3_r"))
    expect_equal(col_names[8L], c("HA_2_r"))
    expect_equal(col_names[28L], c("HA_r"))
    expect_equal(col_names[33L], c("WAZ_pop"))
  })
  it("returns the group as a character vector", {
    res <- anthro_prevalence(
      sex = c(1, 2, 2, 1),
      age = c(1001, 1000, 1010, 1000),
      weight = c(18, 15, 10, 15),
      lenhei = c(100, 80, 100, 100)
    )
    expect_true(is.character(res$Group))
  })
  it("warns if wealthq is not NA,1,2,3,4,5 or Q1:Q5", {
    expect_warning(
      anthro_prevalence(
        sex = c(1, 2, 2, 1, 1),
        age = c(1001, 1000, 1010, 1000, 1000),
        weight = c(18, 15, 10, 15, 15),
        lenhei = c(100, 80, 100, 100, 100),
        wealthq = c("1", "1", "a", "a", "a")
      ),
      "wealthq",
      all = TRUE
    )
  })
  it("warns if wealthq is not NA,1,2,3,4,5 or Q1:Q5", {
    expect_warning(
      anthro_prevalence(
        sex = c(1, 2, 2, 1, 1),
        age = c(1001, 1000, 1010, 1000, 1000),
        weight = c(18, 15, 10, 15, 15),
        lenhei = c(100, 80, 100, 100, 100),
        wealthq = c("Q1", "Q2", "Q3", "a", "a")
      ),
      "wealthq",
      all = TRUE
    )
  })
  it("labels wealth quintiles from Q1:Q5", {
    res <- anthro_prevalence(
      sex = c(1, 2),
      age = 1000,
      weight = rnorm(100, 15, 2),
      lenhei = rnorm(100, 100, 2),
      wealthq = c("Q1", "Q2", "3", "4", "5")
    )
    row_names <- res$Group[grepl(res$Group, pattern = "^Wealth")]
    expected_rownames <- paste0("Wealth quintile: ", c(
      "Q1: Poorest", "Q2",
      "Q3", "Q4",
      "Q5: Richest"
    ))
    expect_true(all(expected_rownames %in% row_names))
    expect_equal(res[22:26, "HAZ_pop"], rep.int(20, 5)) # 20 per quintile
  })
  it("orders wealth quintiles ascending", {
    res <- anthro_prevalence(
      sex = c(1, 2, 2, 1, 1),
      age = c(1001, 1000, 1010, 1000, 1000),
      weight = c(18, 15, 10, 15, 15),
      lenhei = c(100, 80, 100, 100, 100),
      wealthq = c("2", "2", "1", "1", "1")
    )
    row_names <- res$Group[grepl(res$Group, pattern = "^Wealth")]
    expected_rownames <- paste0("Wealth quintile: ", c("Q1: Poorest", "Q2"))
    expect_true(all(expected_rownames %in% row_names))
  })
  it("orders Female before Male", {
    res <- anthro_prevalence(
      sex = c(1, 2, 2, 1, 1),
      age = c(1001, 1000, 1010, 1000, 1000),
      weight = c(18, 15, 10, 15, 15),
      lenhei = c(100, 80, 100, 100, 100)
    )
    row_names <-
      res$Group[grepl(res$Group, pattern = "Male|Female$")]
    expected_row_names <- c(
      paste0("Sex: ", c("Female", "Male")),
      paste0("Age + sex: 24-35 mo.", c("Female", "Male"))
    )
    expect_true(all(expected_row_names %in% row_names))
  })
  it("warns if rows get excluded for prevalence computation", {
    expect_warning(
      anthro_prevalence(
        sex = c(1, 2, 2, 1, 1),
        age = c(1001, 1000, 1010, 1000, 1000),
        weight = c(18, 15, 10, 15, 15),
        lenhei = c(100, 80, 100, 100, 100),
        cluster = c(1, 1, 1, 1, NA_real_)
      ),
      "will be excluded",
      all = TRUE
    )
  })
  it("checks if sw is NA set it to 0 and warn", {
    expect_warning(
      anthro_prevalence(
        sex = c(1, 2, 2, 1, 1),
        age = c(1001, 1000, 1010, 1000, 1000),
        weight = c(18, 15, 10, 15, 15),
        lenhei = c(100, 80, 100, 100, 100),
        sw = c(0.5, 0.5, 0.5, 0.5, NA_real_)
      ),
      "1 row had missing sampling weights",
      all = TRUE
    )
  })
  it("computes age groups with full precision", {
    res <- anthro_prevalence(
      sex = c(1, 2, 2, 1, 1),
      age = c(24, 25, 25, 24, 24),
      is_age_in_month = TRUE,
      weight = c(18, 15, 10, 15, 15),
      lenhei = c(100, 80, 100, 100, 100),
      sw = 0.5
    )
    age_row <- res$Group[grepl(res$Group, pattern = "^Age group:")]
    expect_true("Age group: 24-35 mo" %in% age_row)
  })
  it("filters out rows with age > 1826", {
    expect_warning(
      anthro_prevalence(
        sex = c(1, 2, 2, 1, 1),
        age = c(1826, 1826, 1826, 1826, 1827),
        is_age_in_month = FALSE,
        weight = c(18, 15, 10, 15, 15),
        lenhei = c(100, 80, 100, 100, 100),
        sw = 0.5
      ), "1 row will be excluded"
    )
  })
  it("filters out rows with NA clusters", {
    expect_warning(
      anthro_prevalence(
        sex = c(1, 2, 2, 1, 1),
        age = c(24, 25, 25, 24, 24),
        is_age_in_month = TRUE,
        weight = c(18, 15, 10, 15, 15),
        lenhei = c(100, 80, 100, 100, 100),
        sw = 0.5,
        cluster = c(1, 1, 1, 1, NA_real_)
      ), "1 row will be excluded"
    )
  })
  it("filters out rows with NA strata", {
    expect_warning(
      anthro_prevalence(
        sex = c(1, 2, 2, 1, 1),
        age = c(24, 25, 25, 24, 24),
        is_age_in_month = TRUE,
        weight = c(18, 15, 10, 15, 15),
        lenhei = c(100, 80, 100, 100, 100),
        sw = 0.5,
        cluster = 1,
        strata = c(1, 1, 1, 1, NA_real_)
      ), "1 row will be excluded"
    )
  })
  it("shows result also for empty levels", {
    res <- anthro_prevalence(
      sex = c(1, 1, 1, 1, 1),
      age = c(1001, 1000, 1010, 1000, 1000),
      weight = c(18, 15, 10, 15, 15),
      lenhei = c(100, 80, 100, 100, 100)
    )
    expect_equal(length(res[, 1]), 21)
    expect_true(any(grepl(x = res[, 1], pattern = "Female")))
    res_f <- res[res$Group == "Sex: Female", -1]
    expect_true(all(
      is.na(res_f[, !grepl("pop$", colnames(res_f))])
    ))
    expect_true(all(
      res_f[, grepl("pop$", colnames(res_f))] == 0
    ))
  })
  it("recycles arguments", {
    expect_silent({
      anthro_prevalence(
        sex = 1,
        age = c(25, 26),
        is_age_in_month = TRUE,
        weight = 1:64 * 10,
        lenhei = 1:64 * 40,
        measure = rep.int("l", 8),
        oedema = rep.int("n", 8),
        sw = 0.5,
        cluster = rep.int(1, 32),
        strata = rep.int(1, 8),
        typeres = rep.int("Rural", 2),
        gregion = rep.int(1, 4),
        wealthq = rep.int(1, 8),
        mothered = rep.int("Primary", 8),
        othergr = rep.int("a", 2)
      )
    })
  })
  it("has the correct backwards compatible suffix for pop/unwpop", {
    res <- anthro_prevalence(
      sex = c(1, 2, 2, 1, 1),
      age = c(1001, 1000, 1010, 1000, 1000),
      weight = c(18, 15, 10, 15, 15),
      lenhei = c(100, 80, 100, 100, 100)
    )
    expect_true(all(!(c("HA_2_WH2_pop", "HA_2_WH2_unwpop") %in% colnames(res))))
    expect_true(all(c("HA_2_WH_2_pop", "HA_2_WH_2_unwpop") %in% colnames(res)))
  })
})

test_that("bug20190222: it does not crash if all values are NA in a group", {
  expect_silent(
    res <- anthro_prevalence(
      sex = c(1, 1, 1, 1, 1, 2, 2),
      age = c(1001, 1000, 1010, 1000, 1000, 300, 300),
      weight = c(18, 15, 10, 15, 15, 15, 15),
      lenhei = c(100, 80, 100, 100, 100, 100, 100)
    )
  )
})

test_that("bug20190222: it does not crash if too few elements are in a group", {
  expect_silent(
    res <- anthro_prevalence(
      sex = c(1, 1, 1, 1, 1, 2),
      age = c(1001, 1000, 1010, 1000, 1000, 300),
      weight = c(18, 15, 10, 15, 15, 15),
      lenhei = c(100, 80, 100, 100, 100, 100)
    )
  )
})

test_that("Rounded values for age_in_days are used if age in months is provided", {
  expect_silent(
    res <- anthro_prevalence(
      sex = c(1, 1, 1, 1, 1, 2),
      age = c(59.9, 59.9, 59.92, 59.99, 59.992, 59.992),
      is_age_in_month = TRUE,
      weight = c(18, 15, 10, 15, 15, 15),
      lenhei = c(100, 80, 100, 100, 100, 100)
    )
  )
  expect_equal(res[1, 2], 5)
})

test_that("Prevalence computation does not fail if all zscores are NA", {
  expect_silent(
    anthro_prevalence(
      sex = c(1, 1, 1, 1, 1, 2),
      age = 30,
      is_age_in_month = TRUE,
      weight = 500,
      lenhei = 700
    )
  )
})

test_that("Cluster/strata/sw information is passed correctly to survey", {
  set.seed(1)
  data <- data.frame(
    sex = sample(1:2, 100, replace = TRUE),
    age = sample(0:50, 100, replace = TRUE),
    weight = pmax(rnorm(100, 20, 10), 1),
    lenhei = pmax(rnorm(100, 80, 20), 30),
    cluster = 1:100 %% 3
  )
  data$strata <- as.integer(data$age > 20)
  data$sw <- runif(nrow(data)) / nrow(data)
  res <- anthro_prevalence(
    data$sex,
    data$age,
    is_age_in_month = TRUE,
    weight = data$weight,
    lenhei = data$lenhei,
    cluster = data$cluster,
    strata = data$strata,
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
    id = ~cluster, data = zscores, nest = TRUE, weights = ~sw, strata = ~strata
  )
  design_unweighted <- survey::svydesign(
    id = ~cluster, data = zscores, nest = TRUE, weights = NULL, strata = ~strata
  )
  expected <- survey::svyby(~zlen, ~sex, design, survey::svymean,
    na.rm = TRUE,
    na.rm.all = TRUE,
    drop.empty.groups = FALSE
  )
  observed <- res[c(8, 9), c("Group", "HA_r", "HA_se")]
  expect_equal(
    observed$HA_r, rev(expected$zlen)
  )
  expect_equal(
    observed$HA_se, rev(expected$se)
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
    observed$HAZ_pop, as.numeric(expected_total_weighted[2])
  )
  expect_equal(
    observed$HAZ_unwpop, as.numeric(expected_total_unweighted[2])
  )
})

test_that("pop/unwpop are 0 if no values in that group", {
  res <- anthro_prevalence(
    sex = c(1, 1, 2, 2, 1, 2),
    age = 30,
    is_age_in_month = TRUE,
    weight = 10000,
    lenhei = 700
  )
  expect_equal(res$HAZ_pop, rep.int(0, nrow(res)))
  expect_equal(res$HAZ_unwpop, rep.int(0, nrow(res)))
  expect_equal(res$WAZ_pop, rep.int(0, nrow(res)))
  expect_equal(res$WAZ_unwpop, rep.int(0, nrow(res)))
  expect_equal(res$BMIZ_pop, rep.int(0, nrow(res)))
  expect_equal(res$BMIZ_pop, rep.int(0, nrow(res)))
})

test_that("var can be computed even if some levels have just one observation", {
  set.seed(1)
  data <- data.frame(
    sex = sample(1:2, 100, replace = TRUE),
    age = sample(0:50, 100, replace = TRUE),
    weight = pmax(rnorm(100, 20, 10), 1),
    lenhei = pmax(rnorm(100, 80, 20), 30)
  )
  data$sex <- 1
  data$sex[42] <- 2
  res <- anthro_prevalence(
    data$sex,
    data$age,
    is_age_in_month = TRUE,
    weight = data$weight,
    lenhei = data$lenhei
  )
  expect_true(is.na(res[res$Group == "Sex: Female", "HA_stdev"]))
  expect_false(is.na(res[res$Group == "Sex: Male", "HA_stdev"]))
})
