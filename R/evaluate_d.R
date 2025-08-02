#' Evaluate IUCN Criterion D: Very Small or Restricted Population
#'
#' This function assesses a taxon under IUCN Criterion D, including both subcriteria D1 and D2,
#' based on population size and geographical metrics.
#'
#' The function returns three columns:
#'
#' - **D**: Overall classification under Criterion D:
#'     - `"CR"`: Number of mature individuals < 50
#'     - `"EN"`: < 250
#'     - `"VU"`: < 1000
#'     - `"VU"`: ≥ 1000 but with Area of Occupancy < 20 km² or ≤ 5 locations
#'     - `"NT"`: Does not meet above but has valid population data
#'
#' - **D1**: Assigned `"D1"` if population is between 250–1000 individuals
#'
#' - **D2**: Assigned `"D2"` if population is ≥ 1000 and the taxon is restricted
#' (AoO < 20 or ≤ 5 locations), indicating vulnerability to stochastic events
#'
#' @param df A data frame containing the following columns (case-insensitive):
#' - `C_n_mat`: Number of mature individuals
#' - `B2_aoo`: Area of Occupancy (in km²)
#' - `B_AND_a`: Number of locations
#'
#' @return A tibble with 3 columns: `D`, `D1`, and `D2`, each indicating the corresponding
#' assessment category or `NA` if no condition is met.
#'
#'
#' @export

evaluate_d <- function(df) {

  d_vars_upper <- c("C_n_mat", "B2_aoo", "B_AND_a")
  d_vars <- tolower(d_vars_upper)

  lower_names <- tolower(names(df))
  names(df) <- lower_names


  missing_vars <- setdiff(d_vars, lower_names)

  assertthat::assert_that(
    length(missing_vars) == 0,
    msg = paste("Missing required variables:", paste(missing_vars, collapse = ", "))
  )

  non_numeric <- d_vars[!sapply(df[d_vars], is.numeric)]
  if (length(non_numeric) > 0) {
    stop(glue::glue("The following variables are not numeric: {toString(non_numeric)}"))
  }

  D <- dplyr::case_when(
    df[["c_n_mat"]] < 50 ~ "CR",
    df[["c_n_mat"]] >= 50 & df[["c_n_mat"]] < 250 ~ "EN",
    df[["c_n_mat"]] >= 250 & df[["c_n_mat"]] < 1000 ~ "VU",
    df[["c_n_mat"]] >= 1000 & (df[["b2_aoo"]] < 20 | df[["b_and_a"]] < 5) ~ "VU",
    !is.na(df[["c_n_mat"]]) ~ "NT",
    TRUE ~ NA_character_
  )

  D1 <- dplyr::case_when(
    df[["c_n_mat"]] > 250 & df[["c_n_mat"]] < 1000 ~ "D1",
    TRUE ~ NA_character_
  )

  D2 <- dplyr::case_when(
    df[["c_n_mat"]] >= 1000 & (df[["b2_aoo"]] < 20 | df[["b_and_a"]] < 5) ~ "D2",
    TRUE ~ NA_character_
  )

  return(tibble::tibble(D = D, D1 = D1, D2 = D2))

}

