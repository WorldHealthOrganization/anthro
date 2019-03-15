#' Compute the WHO Child Growth Standards
#'
#' @description
#' A package to compute the WHO Child Growth Standards.
#' More information on the methods is available online:
#' <\url{http://www.who.int/childgrowth/standards/en/}>.
#'
#' Package lifecyle is "maturing" in the tidyverse sense (<\url{https://www.tidyverse.org/lifecycle/}>).
#' It is aimed to keep the API backward compatible, but changes to the API
#' might occur.
#'
#' @docType package
#' @name anthro
#' @aliases anthro package-anthro
NULL

globalVariables(c("growthstandards_lenanthro",
                  "growthstandards_weianthro"))

# the constant to scale between days and months
ANTHRO_DAYS_OF_MONTH <- 30.4375