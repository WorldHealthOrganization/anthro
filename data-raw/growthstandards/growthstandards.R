make_standard <- function(name) {
  growthstandard <- as.data.frame(
    readr::read_tsv(paste0("data-raw/growthstandards/", name, ".txt")),
    stringsAsFactors = FALSE
  )
  growthstandard$age <- as.integer(growthstandard$age)
  growthstandard$sex <- as.integer(growthstandard$sex)
  growthstandard
}

growthstandards_lenanthro <- make_standard("lenanthro")
growthstandards_weianthro <- make_standard("weianthro")
growthstandards_bmianthro <- make_standard("bmianthro")
growthstandards_hcanthro <- make_standard("hcanthro")
growthstandards_acanthro <- make_standard("acanthro")
growthstandards_tsanthro <- make_standard("tsanthro")
growthstandards_ssanthro <- make_standard("ssanthro")

growthstandards_wflanthro <- as.data.frame(
  readr::read_tsv(paste0("data-raw/growthstandards/wflanthro.txt")),
  stringsAsFactors = FALSE
)
growthstandards_wflanthro$sex <- as.integer(growthstandards_wflanthro$sex)

growthstandards_wfhanthro <- as.data.frame(
  readr::read_tsv(paste0("data-raw/growthstandards/wfhanthro.txt")),
  stringsAsFactors = FALSE
)
growthstandards_wfhanthro$sex <- as.integer(growthstandards_wfhanthro$sex)

usethis::use_data(
  growthstandards_weianthro,
  growthstandards_lenanthro,
  growthstandards_bmianthro,
  growthstandards_wflanthro,
  growthstandards_wfhanthro,
  growthstandards_hcanthro,
  growthstandards_acanthro,
  growthstandards_tsanthro,
  growthstandards_ssanthro,
  overwrite = TRUE,
  internal = TRUE,
  version = 2 # we set it to version 2 to be able to support older R versions
)
