#' Evaluate IUCN Criterion B: Geographic Range and Subcriteria
#'
#' This function evaluates species against IUCN Red List Criterion **B**, which includes geographic range
#' measures and subcriteria related to fragmentation, decline, and extreme fluctuations.
#'
#' The function computes classifications for:
#' - **B1**: Extent of Occurrence (`B1_eoo`)
#' - **B2**: Area of Occupancy (`B2_aoo`)
#' - **ba**: Number of locations (`B_AND_a`)
#' - **bb**: Evidence of ongoing decline (`B_AND_b_*`)
#' - **bc**: Evidence of extreme fluctuations (`B_AND_c_*`)
#'
#' @param df A data frame containing the following required columns:
#' - `B1_eoo`, `B2_aoo`: Numeric values for extent and area of occupancy
#' - `B_AND_a`: Number of locations (numeric)
#' - `B_AND_b_i` to `B_AND_b_v`: Binary indicators of ongoing decline
#' - `B_AND_c_i` to `B_AND_c_iv`: Binary indicators of extreme fluctuations
#'
#' @return A tibble with one row and five columns:
#' - `B1`: Category based on `B1_eoo` using thresholds (CR < 100, EN < 5,000, VU < 20,000)
#' - `B2`: Category based on `B2_aoo` using thresholds (CR < 10, EN < 500, VU < 2,000)
#' - `ba`: Category based on number of locations (`CR`, `EN`, `VU`, or NA)
#' - `bb`: A string showing which B(b) subcriteria are active, e.g., `"b(i,ii,iv)"`, or `NA`
#' - `bc`: A string showing which B(c) subcriteria are active, e.g., `"c(ii,iv)"`, or `NA`
#'
#' @details
#' The function validates:
#' - That required columns are present
#' - That `B1_eoo` and `B2_aoo` are numeric
#'
#' Thresholds for classification:
#' - **B1 (EoO)**: CR < 100, EN < 5,000, VU < 20,000
#' - **B2 (AoO)**: CR < 10, EN < 500, VU < 2,000
#'
#' @examples
#' df <- tibble::tibble(
#'   B1_eoo = 90,
#'   B2_aoo = 8,
#'   B_AND_a = 1,
#'   B_AND_b_i = 1, B_AND_b_ii = NA, B_AND_b_iii = 0, B_AND_b_iv = 1, B_AND_b_v = 0,
#'   B_AND_c_i = NA, B_AND_c_ii = 1, B_AND_c_iii = NA, B_AND_c_iv = 0
#' )
#' evaluate_b(df)
#'
#' @export


evaluate_b <- function(df) {

  b_vars <- c("B1_eoo", "B2_aoo")
  missing_vars <- setdiff(b_vars, names(df))

  .check_vars_present(df, b_vars)

  non_numeric <- b_vars[!sapply(df[b_vars], is.numeric)]
  if (length(non_numeric) > 0) {
    stop(glue::glue("The following variables are not numeric: {toString(non_numeric)}"))
  }

  return(
    tibble::tibble(
      B1 = .classify_b(df, thresholds = c(100, 5000, 20000), var_th = "B1_eoo"),
      B2 = .classify_b(df, thresholds = c(10, 500, 2000), var_th = "B2_aoo"),
      ba = .evaluate_ba(df),
      bb = .evaluate_bb(df),
      bc = .evaluate_bc(df)
    )
  )

}








