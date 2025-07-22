#' Classify species risk according to IUCN Criterion A
#'
#' This function validates input data and classifies species based on population reduction
#' using IUCN Criterion **A1 to A4** thresholds. It also generates categorical flags (**Aa–Ae**)
#' based on presence indicators like `based_a`, `based_b`, etc.
#'
#' @param df A data frame with the following required columns:
#'
#' - `a_pop_red`: Numeric, percent population reduction (0–100)
#' - `type_A1` to `type_A4`: Integer flags (1/0) indicating which A sub-criteria apply
#' - `based_a` to `based_e`: Integer flags (1/0) for additional categorical marks
#' - `species`: Character, species identifier
#'
#' @return A data frame with the original `species` column plus:
#'
#' - `A1` to `A4`: IUCN classification per subcriterion (`"CR"`, `"EN"`, `"VU"`, `"NT"`)
#' - `Aa` to `Ae`: Single-letter codes (`"a"` to `"e"`) when corresponding `based_*` is 1
#'
#' @details
#' Classification thresholds:
#'
#' - **A1**: CR ≥ 90, EN ≥ 70, VU ≥ 50, else NT
#' - **A2–A4**: CR ≥ 80, EN ≥ 50, VU ≥ 30, else NT
#'
#' The function uses internal validation and issues CLI messages to inform the user
#' about data integrity and classification steps.
#'
#' @examples
#' df <- data.frame(
#'   species = c("sp1", "sp2"),
#'   a_pop_red = c(85, 40),
#'   type_A1 = c(1, 1),
#'   type_A2 = c(0, 0),
#'   type_A3 = c(0, 0),
#'   type_A4 = c(0, 0),
#'   based_a = c(1, 0),
#'   based_b = c(0, 1),
#'   based_c = 0, based_d = 0, based_e = 0
#' )
#' evaluate_a(df)
#'
#' @importFrom cli cli_alert_info cli_alert_success
#' @importFrom assertthat assert_that
#' @export
#'
evaluate_a <- function(df) {

  cli::cli_alert_info("Checking presence of required variables...")

  required_vars <- c(
    "a_pop_red", "type_A1", "type_A2", "type_A3", "type_A4",
    "based_a", "based_b", "based_c", "based_d", "based_e", "species"
  )

  missing_vars <- setdiff(required_vars, names(df))
  assertthat::assert_that(
    length(missing_vars) == 0,
    msg = paste("Missing required variables:", paste(missing_vars, collapse = ", "))
  )

  cli::cli_alert_success("All required variables are present.")


  cli::cli_alert_info("Checking data types and value ranges...")

  assertthat::assert_that(
    is.numeric(df$a_pop_red),
    msg = "'a_pop_red' must be numeric."
  )

  assertthat::assert_that(
    all(df$a_pop_red >= 0 & df$a_pop_red <= 100, na.rm = TRUE),
    msg = "'a_pop_red' must be between 0 and 100."
  )

  cli::cli_alert_success("Data types and value ranges are valid.")



  cli::cli_alert_info("Classifying 'a_pop_red' for types A1–A4...")

  types <- c("type_A1", "type_A2", "type_A3", "type_A4")
  thresholds_list <- list(
    c(90, 70, 50),
    c(80, 50, 30),
    c(80, 50, 30),
    c(80, 50, 30)
  )
  output_names <- c("A1", "A2", "A3", "A4")

  for (i in seq_along(types)) {
    df[[output_names[i]]] <- .classify(df, var_type = types[i],
                                       thresholds_list[[i]],
                                       var_th = "a_pop_red")
  }

  cli::cli_alert_info("Assigning based_a to based_e categories...")

  for (i in letters[1:5]) {
    col_from <- paste0("based_", i)
    col_to   <- paste0("A", i)
    df[[col_to]] <- ifelse(df[[col_from]] == 1, i, NA_character_)
  }

  cli::cli_alert_success("Classification complete.")

  return(df[c("species", "A1", "A2", "A3", "A4",
              "Aa", "Ab", "Ac", "Ad", "Ae")])
}

