#' Classify species based on thresholds
#' This function classifies species into categories based on their variable type and thresholds.
#' @param df A data frame containing the species data.
#' @param var_type The variable type to classify (e.g., "type_A1", "type_A2").
#' @param thresholds A numeric vector containing the thresholds for classification.
#' @param var_th The variable to use for classification (e.g., "A_Pop_red").
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
