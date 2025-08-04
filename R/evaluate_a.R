#'#' Evaluate IUCN Criterion A (Population Reduction)
#'
#' This function evaluates species under IUCN Criterion A by classifying population
#' reduction percentages (`a_pop_red`) across types **A1–A4**, and identifying
#' which supporting evidence types **Aa–Ae** apply.
#'
#' @param df A data frame with the following required columns:
#' - `a_pop_red`: Numeric, population reduction percentage (range: 0–100)
#' - `type_A1` to `type_A4`: Binary flags indicating which A-types apply (1 = applies, 0 = not)
#' - `based_a` to `based_e`: Binary flags for types of evidence used
#' - `species`: Species identifier
#'
#' @return A tibble with the following columns:
#' - `species`: Species identifier
#' - `A1` to `A4`: Classification per A-type (`"CR"`, `"EN"`, `"VU"`, `"NT"`, or `NA`)
#' - `Aa` to `Ae`: Evidence labels present (`"a"` to `"e"`), or `NA` if not used
#'
#' @details
#' Classification thresholds:
#' - **A1**: CR ≥ 90, EN ≥ 70, VU ≥ 50
#' - **A2–A4**: CR ≥ 80, EN ≥ 50, VU ≥ 30
#'
#' Supporting evidence is derived from `based_a` to `based_e`, mapping directly to `Aa` to `Ae`.
#'
#' @examples
#' df <- tibble::tibble(
#'   species = "Panthera onca",
#'   a_pop_red = 85,
#'   type_A1 = 0, type_A2 = 1, type_A3 = 0, type_A4 = 0,
#'   based_a = 1, based_b = 0, based_c = 0, based_d = 0, based_e = 1
#' )
#' evaluate_a(df)
#'
#' @export
#'

evaluate_a <- function(df) {

  a_vars_upper <- c(
    "A_pop_red", "type_A1", "type_A2", "type_A3", "type_A4",
    "based_a", "based_b", "based_c", "based_d", "based_e", "species"
  )
  a_vars <- tolower(a_vars_upper)

  lower_names <- tolower(names(df))
  names(df) <- lower_names

  .check_vars_present(df, a_vars)
  .check_vars_numeric(df, a_vars_upper[[1]])

  assertthat::assert_that(
    all(df$a_pop_red >= 0 & df$a_pop_red <= 100, na.rm = TRUE),
    msg = "'a_pop_red' must be between 0 and 100."
  )

  types <- c("type_a1", "type_a2", "type_a3", "type_a4")
  thresholds_list <- list(
    c(90, 70, 50),
    c(80, 50, 30),
    c(80, 50, 30),
    c(80, 50, 30)
  )
  output_names <- c("A1", "A2", "A3", "A4")

  for (i in seq_along(types)) {
    df[[output_names[i]]] <- .classify_a(df, var_type = types[i],
                                       thresholds_list[[i]],
                                       var_th = "a_pop_red")
  }

  for (i in letters[1:5]) {
    col_from <- paste0("based_", i)
    col_to   <- paste0("A", i)
    df[[col_to]] <- ifelse(df[[col_from]] == 1, i, NA_character_)
  }

  return(df[c("A1", "A2", "A3", "A4",
              "Aa", "Ab", "Ac", "Ad", "Ae")])
}

