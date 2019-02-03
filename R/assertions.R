assert_valid_sex <- function(sex) {
  param_sex_error_msg <- paste0("Parameter `sex` must be an integer vector ",
                                "with values 1 for male and 2 for female")
  if (!is.numeric(sex)) {
    stop(param_sex_error_msg, call. = FALSE)
  }
  if (!is.integer(sex)) {
    sex <- as.integer(sex)
    if (anyNA(sex) || any(!(sex %in% c(1L, 2L)))) {
      stop(param_sex_error_msg, call. = FALSE)
    }
  }
}

assert_valid_age_in_days <- function(age_in_days) {
  if (!is.numeric(age_in_days)) {
    stop("Parameter `age_in_days` must be integer values",
         " or values that can be coerced to integers", call. = FALSE)
  }
  age_in_days
}

assert_growthstandards <- function(growthstandards) {
  col_names <- colnames(growthstandards)
  column_is_correct <- function(col, typefun) {
    !(col %in% col_names) || typefun(growthstandards[[col]])
  }
  stopifnot(is.data.frame(growthstandards),
            any(c("age", "length", "height") %in% col_names),
            column_is_correct("age", is.integer),
            column_is_correct("length", is.numeric),
            column_is_correct("height", is.numeric),
            is.integer(growthstandards[["sex"]]))
}

assert_length <- function(x, l) {
  param_name <- as.character(substitute(x))
  if (length(x) != l) {
    stop(param_name, " must have length ", l, call. = FALSE)
  }
}

deparse_chr <- function(expr) {
  paste0(deparse(expr), collapse = " ")
}

assert_logical <- function(x) {
  assert_type(is.logical, "logical", x,
              param_name = deparse_chr(substitute(x)))
}

assert_numeric <- function(x) {
  assert_type(is.numeric, "numeric", x,
              param_name = deparse_chr(substitute(x)))
}

assert_character <- function(x) {
  assert_type(is.character, "character", x,
              param_name = deparse_chr(substitute(x)))
}

assert_character_or_numeric <- function(x) {
  assert_type(function(y) is.character(y) || is.numeric(y),
              "character or numeric", x,
              param_name = deparse_chr(substitute(x)))
}

assert_type <- function(type_fun, type_name, x, param_name) {
  if (type_fun(x)) {
    TRUE
  } else {
    stop(param_name, " must be a ", type_name, call. = FALSE)
  }
}

assert_values_in_set <- function(x, allowed) {
  param_name <- as.character(substitute(x))
  if (length(allowed) > 0L && any(!(x %in% allowed))) {
    stop("Some values in ", param_name, " are not valid.",
         " Accepted values are ", paste0(allowed, collapse = ", "),
         call. = FALSE)
  }
}