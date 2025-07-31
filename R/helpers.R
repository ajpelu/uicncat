#' Classify species based on thresholds
#'
#' This function classifies species into categories based on their variable type
#' and thresholds.
#'
#' @param df A data frame containing the species data.
#' @param var_type The variable type to classify (e.g., "type_A1", "type_A2").
#' @param thresholds A numeric vector containing the thresholds for classification.
#' @param var_th The variable to use for classification (e.g., "A_Pop_red").
#'
#' @return A character vector with the classification results.


.classify_a <- function(df, var_type, thresholds, var_th) {
  v <- df[[var_th]]
  th <- thresholds

  dplyr::case_when(
    df[[var_type]] == 1 & v >= th[1] ~ "CR",
    df[[var_type]] == 1 & v >= th[2] & v < th[1] ~ "EN",
    df[[var_type]] == 1 & v >= th[3] & v < th[2] ~ "VU",
    df[[var_type]] == 1 & v < th[3] ~ "NT",
    TRUE ~ NA_character_
  )
}

#' Classify species based on a single variable and numeric thresholds (Criterion B)
#'
#' Internal function used to assign a Red List category (`CR`, `EN`, `VU`) based on a
#' continuous numeric variable and threshold values.
#'
#' Typically used to classify geographic range measures such as:
#' - Extent of Occurrence (EoO, `B1_eoo`)
#' - Area of Occupancy (AoO, `B2_aoo`)
#'
#' @param df A data frame containing the variable to classify.
#' @param thresholds A numeric vector of length 3, defining the thresholds for:
#' - `CR`: values < thresholds `[1]`
#' - `EN`: values < thresholds `[2]`
#' - `VU`: values < thresholds `[3]`
#' @param var_th A character string indicating the name of the column in `df` to classify.
#'
#' @return A character vector with classification results:
#' - `"CR"` = Critically Endangered
#' - `"EN"` = Endangered
#' - `"VU"` = Vulnerable
#' - `NA` if no thresholds are met
#'
.classify_b <- function(df, thresholds, var_th) {
  v <- df[[var_th]]
  th <- thresholds

  dplyr::case_when(
    v < th[1] ~ "CR",
    v >= th[1] & v < th[2] ~ "EN",
    v >= th[2] & v < th[3] ~ "VU",
    TRUE ~ NA_character_
  )
}



#' Evaluate criterion B(a): small population size
#'
#' Assigns a conservation category (CR, EN, VU) based on the number of individuals
#' in a population.
#'
#' @param df A data frame that must include the variable "B_AND_a".
#'
#' @return A character vector with values: "CR", "EN", "VU", or NA.
.evaluate_ba <- function(df) {

  ba_vars <- "B_AND_a"
  # Check if the required variable is present
  assertthat::assert_that(
    ba_vars %in% names(df),
    msg = glue::glue("Variable '{ba_vars}' is not present in the data.")
  )
  # Check if the variable is numeric
  assertthat::assert_that(
    is.numeric(df[[ba_vars]]),
    msg = glue::glue("{ba_vars} must be numeric.")
  )

  category <- .classify_b(df, thresholds = c(1, 5, 10), var_th = ba_vars)

  return(category)

}

#' Evaluate criterion B(b): continuing decline
#'
#' Checks which sub-criteria under criterion B(b) are triggered (i.e., have value
#' 1 or are TRUE), and returns a label indicating which ones.
#'
#' @param df A data frame that must include the variables "B_AND_b_i" to "B_AND_b_v".
#'
#' @return A string of the form "b(i,ii,...)" if any sub-criterion is met, or NA
#' if none apply.


.evaluate_bb <- function(df) {

  bb_vars <- c("B_AND_b_i", "B_AND_b_ii", "B_AND_b_iii", "B_AND_b_iv", "B_AND_b_v")

  missing_vars <- setdiff(bb_vars, names(df))

  assertthat::assert_that(
    length(missing_vars) == 0,
    msg = paste("Missing required variables:", paste(missing_vars, collapse = ", "))
  )

  valores <- df[bb_vars]

  # Identify columns with value 1 (ignoring NA)
  activos <- names(valores)[!is.na(valores) & valores == 1]

  # Extract criteria
  if (length(activos) > 0) {
    criterios <- gsub("B_AND_b_", "", activos)
    category <- paste0("b(", paste(criterios, collapse = ","), ")")
  } else {
    category <- NA_character_
  }

  return(category)

}

#' Evaluate criterion B(c): extreme fluctuations
#'
#' Checks which sub-criteria under criterion B(c) are triggered (i.e., have value 1), and returns a label indicating which ones.
#'
#' @param df A data frame that must include the variables "B_AND_c_i" to "B_AND_c_iv".
#'
#' @return A string of the form "c(i,ii,...)" if any sub-criterion is met, or NA if none apply.

.evaluate_bc <- function(df) {

  bc_vars <-  c("B_AND_c_i", "B_AND_c_ii", "B_AND_c_iii", "B_AND_c_iv")

  missing_vars <- setdiff(bc_vars, names(df))

  assertthat::assert_that(
    length(missing_vars) == 0,
    msg = paste("Missing required variables:", paste(missing_vars, collapse = ", "))
  )

  valores <- df[bc_vars]

  # Identify columns with value 1 (ignoring NA)
  activos <- names(valores)[!is.na(valores) & valores == 1]

  # Extract criteria
  if (length(activos) > 0) {
    criterios <- gsub("B_AND_c_", "", activos)
    category <- paste0("c(", paste(criterios, collapse = ","), ")")
  } else {
    category <- NA_character_
  }

  return(category)

}
