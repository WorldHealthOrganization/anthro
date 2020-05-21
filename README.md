
[![Travis build
status](https://travis-ci.org/dirkschumacher/anthro.svg?branch=master)](https://travis-ci.org/dirkschumacher/anthro)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/dirkschumacher/anthro?branch=master&svg=true)](https://ci.appveyor.com/project/dirkschumacher/anthro)
[![Coverage
status](https://codecov.io/gh/dirkschumacher/anthro/branch/master/graph/badge.svg)](https://codecov.io/github/dirkschumacher/anthro?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/anthro)](https://cran.r-project.org/package=anthro)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

# Anthro

The `anthro` package allows you to perform comprehensive analysis of
anthropometric survey data based on the
[method](https://www.who.int/childgrowth/standards/en/) developed by the
Department of Nutrition for Health and Development at the World Health
Organization.

The package is modelled after the [R
macros](https://www.who.int/childgrowth/software/en/) provided by WHO.
The package adds more accurate calculations of confidence intervals and
standard errors around the prevalence estimates, taking into account
complex sample designs, whenever is the case

## Installation

``` r
install.packages("anthro")
```

``` r
remotes::install_github("dirkschumacher/anthro")
```

## Examples

``` r
library(anthro)
```

### Z-Score

This function calculates z-scores for the eight anthropometric
indicators, weight-for- age, length/height-for-age,
weight-for-length/height, body mass index (BMI)-for-age, head
circumference-for-age, arm circumference-for-age, triceps
skinfold-for-age and subscapular skinfold-for-age based on the [WHO
Child Growth Standards](https://www.who.int/childgrowth/standards/en/).

``` r
anthro_zscores(
  sex = c(1, 2, 1, 1),
  age = c(1001, 1000, 1010, 1000),
  weight = c(18, 15, 10, 15),
  lenhei = c(120, 80, 100, 100)
)
#>   clenhei    cbmi cmeasure csex  zlen flen  zwei fwei  zwfl fwfl  zbmi fbmi zhc
#> 1     120 12.5000     <NA>    1  7.31    1  2.20    0 -2.39    0 -3.01    0  NA
#> 2      80 23.4375     <NA>    2 -3.50    0  0.95    0  4.13    0  4.66    0  NA
#> 3     100 10.0000     <NA>    1  1.62    0 -2.76    0 -5.19    1 -5.61    1  NA
#> 4     100 15.0000     <NA>    1  1.70    0  0.69    0 -0.29    0 -0.58    0  NA
#>   fhc zac fac zts fts zss fss
#> 1  NA  NA  NA  NA  NA  NA  NA
#> 2  NA  NA  NA  NA  NA  NA  NA
#> 3  NA  NA  NA  NA  NA  NA  NA
#> 4  NA  NA  NA  NA  NA  NA  NA
```

The returned value is a `data.frame` that can further be processed or
saved as a `.csv` file as in the original function.

You can also use the function with a given dataset with `with`

``` r
your_data_set <- read.csv("my_survey.csv")
with(
  your_data_set,
  anthro_zscores(
    sex = sex, age = age_in_days,
    weight = weight, lenhei = lenhei
  )
)
```

To look at all parameters, type `?anthro_zscores`.

### Prevalence estimates

The prevalence estimates are similiar to `anthro_zscores`: again they
take vectors instead of a data frame and column names for the
aforementioned reasons.

``` r
anthro_prevalence(
  sex = c(1, 2, 2, 1),
  age = c(1001, 1000, 1010, 1000),
  weight = c(18, 15, 10, 15),
  lenhei = c(100, 80, 100, 100)
)[, 1:5]
#>                         Group HAZ_pop HAZ_unwpop HA_3_r  HA_3_se
#> 1                         All       4          4     25 25.00000
#> 2         Age group: 00-05 mo      NA         NA     NA       NA
#> 3         Age group: 06-11 mo      NA         NA     NA       NA
#> 4         Age group: 12-23 mo      NA         NA     NA       NA
#> 5         Age group: 24-35 mo       4          4     25 25.00000
#> 6         Age group: 36-47 mo      NA         NA     NA       NA
#> 7         Age group: 48-59 mo      NA         NA     NA       NA
#> 8                 Sex: Female       2          2     50 40.82483
#> 9                   Sex: Male       2          2      0  0.00000
#> 10 Age + sex: 00-05 mo.Female      NA         NA     NA       NA
#> 11 Age + sex: 06-11 mo.Female      NA         NA     NA       NA
#> 12 Age + sex: 12-23 mo.Female      NA         NA     NA       NA
#> 13 Age + sex: 24-35 mo.Female       2          2     50 40.82483
#> 14 Age + sex: 36-47 mo.Female      NA         NA     NA       NA
#> 15 Age + sex: 48-59 mo.Female      NA         NA     NA       NA
#> 16   Age + sex: 00-05 mo.Male      NA         NA     NA       NA
#> 17   Age + sex: 06-11 mo.Male      NA         NA     NA       NA
#> 18   Age + sex: 12-23 mo.Male      NA         NA     NA       NA
#> 19   Age + sex: 24-35 mo.Male       2          2      0  0.00000
#> 20   Age + sex: 36-47 mo.Male      NA         NA     NA       NA
#> 21   Age + sex: 48-59 mo.Male      NA         NA     NA       NA
```

Using the function `with` it is easy to apply `anthro_prevalence` to a
full dataset.

To look at all parameters, type `?anthro_prevalence`.

### Contribution

Contributions are always very welcome. Please make sure to post an issue
before sending a pull request.

### Using the package in your own analyses

The package has been tested thoroughly, but we cannot guarantee that
there aren’t any bugs nor comes this with any warranty (as with all open
source software). If you find a bug or cannot reproduce results obtained
with other implementations, please post an issue.
