#' Evaluate IUCN Criterion E: Quantitative Analysis of Extinction Risk
#'
#' Based on binary variables `E1`, `E2`, `E3`, and `E4`, this function classifies
#' a taxon into a category depending on the probability of extinction as determined
#' by a quantitative analysis.
#'
#' The variables must indicate whether the species meets a specific extinction risk threshold:
#'
#' - **E1**: ≥ 50% probability of extinction in **10 years or 3 generations**, whichever is longer (max. 100 years); classified as `"CR"`
#' - **E2**: ≥ 20% probability in **20 years or 5 generations**, whichever is longer (max. 100 years); classified as `"EN"`
#' - **E3**: ≥ 10% probability in **100 years**; classified as `"VU"`
#' - **E4**: Any other quantitative analysis justifying a lower threat level; classified as `"NT"`
#'
#' The function returns the highest applicable category. If none of the variables are `1`, it returns `NA`.
#'
#' @param df A data frame containing binary (0/1) variables `E1`, `E2`, `E3`, and `E4` (case-insensitive).
#'
#' @return A tibble with one column `E`, with values `"CR"`, `"EN"`, `"VU"`, `"NT"`, or `NA`.
#'

evaluate_e <- function(df) {

  e_vars_upper <- c("E1", "E2", "E3", "E4")
  e_vars <- tolower(e_vars_upper)

  lower_names <- tolower(names(df))
  names(df) <- lower_names

  .check_vars_present(df, e_vars)

  tibble::tibble(
    E = dplyr::case_when(
    df[[e_vars[1]]] == 1 ~ "CR",
    df[[e_vars[2]]] == 1 ~ "EN",
    df[[e_vars[3]]] == 1 ~ "VU",
    df[[e_vars[4]]] == 1 ~ "NT",
    TRUE ~ NA_character_
  ))

}
