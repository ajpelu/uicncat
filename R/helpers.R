#' auxiliar function to check if variables are present in a data frame
.check_vars_present <- function(df, vars) {
  vars_lower <- tolower(vars)
  df_names_lower <- tolower(names(df))

  missing <- setdiff(vars_lower, df_names_lower)

  assertthat::assert_that(
    length(missing) == 0,
    msg = paste0("Missing required variables: ",
                 paste(vars[vars_lower %in% missing], collapse = ", "))
  )

  invisible(TRUE)  # invisible if all variables are present
}



#' Classify species using type-based thresholds (Criterion A)
#'
#' Internal function to classify species under Criterion A based on a type indicator
#' (e.g., `type_A1`) and a numeric variable (e.g., population reduction), using
#' predefined thresholds.
#'
#' @param df A data frame containing species information.
#' @param var_type A character string with the name of the type column (e.g., `"type_A1"`).
#' It is the variable type to classify
#' @param thresholds A numeric vector containing the thresholds for classification.
#' By default it must contains 3 values, used to classify into `CR`, `EN`, or `VU`.
#' @param var_th A character string with the name of the numeric variable to
#' classify (e.g., `"a_pop_red"`).
#'
#' @return A character vector with classification results:
#' - `"CR"` (Critically Endangered)
#' - `"EN"` (Endangered)
#' - `"VU"` (Vulnerable)
#' - `"NT"` (Near Threatened)

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

#' Classify species based on a single variable and thresholds (Criterion B)
#'
#' Internal function that assigns a conservation category based on a continuous
#' numeric variable and threshold cutoffs (e.g., extent of occurrence -eoo- or
#' area of occupancy -aoo-). Typically used to classify geographic range measures
#' such as:
#' - Extent of Occurrence (EoO, `B1_eoo`)
#' - Area of Occupancy (AoO, `B2_aoo`)
#'
#' @param df A data frame containing the variable to classify.
#' @param thresholds A numeric vector of length 3, defining the thresholds for:
#' - `CR` if value < `thresholds[1]`
#' - `EN` if value < `thresholds[2]`
#' - `VU` if value < `thresholds[3]`
#' @param var_th A character string specifying the variable name to classify.
#'
#' @return A character vector with classification results:
#' - `"CR"` = Critically Endangered
#' - `"EN"` = Endangered
#' - `"VU"` = Vulnerable
#' - `NA` if no thresholds are met

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

#### Documentar !!!!!
.classify_c <- function(df, thresholds, var_th) {
  v <- df[[var_th]]
  th <- thresholds

  dplyr::case_when(
    v < th[1] ~ "CR",
    v >= th[1] & v < th[2] ~ "EN",
    v >= th[2] & v < th[3] ~ "VU",
    TRUE ~ NA_character_
  )
}


#### Documentar !!!!!
.classify_c2ai <- function(df, thresholds, var_th) {
  v <- df[[var_th]]
  th <- thresholds

  dplyr::case_when(
    v < th[1] ~ "CR",
    v >= th[1] & v < th[2] ~ "EN",
    v >= th[2] & v < th[3] ~ "VU",
    TRUE ~ NA_character_
  )
}




#' Evaluate Criterion B(a): number of locations
#'
#' Classifies species based on the number of locations (`B_AND_a`) using thresholds:
#' - CR: 1 location
#' - EN: 2–5 locations
#' - VU: 6–10 locations
#'
#' Column names are case-insensitive.
#'
#' @param df A data frame containing the variable `B_AND_a` (or equivalent).
#'
#' @return A character vector with values `"CR"`, `"EN"`, `"VU"`, or `NA`.

.evaluate_ba <- function(df) {
  # Define the variable to check
  ba_vars_upper <- "B_AND_a"
  ba_vars <- "b_and_a"
  #
  lower_names <- tolower(names(df))
  names(df) <- lower_names

  # Check if the required variable is present
  # assertthat::assert_that(
  #   ba_vars %in% lower_names,
  #   msg = glue::glue("Variable '{ba_vars_upper}' is not present in the data.")
  # )

  .check_vars_present(df, ba_vars_upper)


  # Check if the variable is numeric
  assertthat::assert_that(
    is.numeric(df[[ba_vars]]),
    msg = glue::glue("{ba_vars_upper} must be numeric.")
  )

  category <- .classify_b(df, thresholds = c(1, 5, 10), var_th = ba_vars)

  return(category)

}

#' Evaluate criterion B(b): continuing decline
#'
#' Identifies which subcriteria under Criterion B(b) are active (i.e., have value
#' 1 or are TRUE)), and returns a label indicating the codes, such as `"b(i,ii,iv)"`.
#'
#' @param df A data frame containing variables `B_AND_b_i` to `B_AND_b_v` (or lowercased).
#'
#' @return A character vector with the same number of rows as `df`. Each value is:
#' - `"b(i,...)"` if subcriteria are active
#' - `NA` if none apply
#'

.evaluate_bb <- function(df) {

  bb_vars_upper <- c("B_AND_b_i", "B_AND_b_ii", "B_AND_b_iii", "B_AND_b_iv", "B_AND_b_v")
  bb_vars <- tolower(bb_vars_upper)

  lower_names <- tolower(names(df))
  names(df) <- lower_names

  missing_vars <- setdiff(bb_vars, lower_names)

  assertthat::assert_that(
    length(missing_vars) == 0,
    msg = paste("Missing required variables:", paste(missing_vars, collapse = ", "))
  )

  # Initialize output (to store results for each row)
  output <- character(nrow(df))

  for (i in seq_len(nrow(df))) {
    # Get the row i with the variable selected
    row_sel <- df[i, bb_vars, drop = FALSE]

    # Identify columns with value 1 (ignoring NA)
    activos <- names(row_sel)[!is.na(row_sel) & row_sel == 1]

    # Extract criteria
    if (length(activos) > 0) {
      criterios <- gsub("b_and_b_", "", activos)
      output[i] <- paste0("b(", paste(criterios, collapse = ","), ")")
    } else {
      output[i] <- NA_character_
    }
  }

  return(output)
}

#' Evaluate Criterion B(c): extreme fluctuations
#'
#' Identifies which subcriteria under Criterion B(c) are active (i.e., have value
#' 1 or are TRUE)), and returns a label indicating the codes, such as `"c(ii,iv)"`.
#'
#' @param df A data frame containing variables `B_AND_c_i` to `B_AND_c_iv` (or lowercased).
#'
#' @return A character vector with the same number of rows as `df`. Each value is:
#' - `"c(i,...)"` if subcriteria are active
#' - `NA` if none apply

.evaluate_bc <- function(df) {

  bc_vars_upper <-  c("B_AND_c_i", "B_AND_c_ii", "B_AND_c_iii", "B_AND_c_iv")
  bc_vars <- tolower(bc_vars_upper)

  lower_names <- tolower(names(df))
  names(df) <- lower_names

  missing_vars <- setdiff(bc_vars, lower_names)

  assertthat::assert_that(
    length(missing_vars) == 0,
    msg = paste("Missing required variables:", paste(missing_vars, collapse = ", "))
  )


  # Initialize output (to store results for each row)
  output <- character(nrow(df))

  for (i in seq_len(nrow(df))) {
    # Get the row i with the variable selected
    row_sel <- df[i, bc_vars, drop = FALSE]

    # Identify columns with value 1 (ignoring NA)
    activos <- names(row_sel)[!is.na(row_sel) & row_sel == 1]

    # Extract criteria
    if (length(activos) > 0) {
      criterios <- gsub("b_and_c_", "", activos)
      output[i] <- paste0("c(", paste(criterios, collapse = ","), ")")
    } else {
      output[i] <- NA_character_
    }
  }

  return(output)

}

#' Evaluate Criterion C: number of mature individuals
#'#### Documentar !!!!!


.evaluate_cmat <- function(df) {
  # Define the variable to check
  cmat_vars_upper <- "C_n_mat"
  cmat_vars <- "c_n_mat"

  lower_names <- tolower(names(df))
  names(df) <- lower_names

  # Check if the required variable is present
  assertthat::assert_that(
    cmat_vars %in% lower_names,
    msg = glue::glue("Variable '{cmat_vars_upper}' is not present in the data.")
  )
  # Check if the variable is numeric
  assertthat::assert_that(
    is.numeric(df[[cmat_vars]]),
    msg = glue::glue("{cmat_vars_upper} must be numeric.")
  )

  category <- .classify_c(df, thresholds = c(250, 2500, 10000), var_th = cmat_vars)

  return(category)

}

#### Documentar !!!!!
.evaluate_c2aii <- function(df) {

  # Define the variable to check
  c2aii_vars_upper <- "C2_aii"
  c2aii_vars <- "c2_aii"

  lower_names <- tolower(names(df))
  names(df) <- lower_names

  # Check if the required variable is present
  assertthat::assert_that(
    c2aii_vars %in% lower_names,
    msg = glue::glue("Variable '{c2aii_vars_upper}' is not present in the data.")
  )
  # Check if the variable is numeric
  assertthat::assert_that(
    is.numeric(df[[c2aii_vars]]),
    msg = glue::glue("{c2aii_vars_upper} must be numeric.")
  )

  v <- df[[c2aii_vars]]

  dplyr::case_when(
    v == 100 ~ "VU",
    v >= 95 & v < 100 ~ "EN",
    v >= 90 & v < 95 ~ "CR",
    TRUE ~ NA_character_
  )


}



