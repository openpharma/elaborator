#' Returns row of a matrix containing all combinations of elements 0 and 1 of a pre-specified length
#'
#' @description
#' This function is mostly useful for generating the reference-value based pattern analysis plots. It returns the (i-1)-th row of a matrix containing all combinations of elements 0 and 1 of a pre-specified length.
#'
#' @param index selected row -1 of the combination matrix which shall be returned
#' @param number_combinations length of the combination vector
#'
#' @return A numeric vector containing a specific combination of elements 0 and 1; this combination is used to calculate pattern for reference-value based pattern analysis plot.
#'
#' @keywords internal

elaborator_calculate_ref_pattern <- function(index, number_combinations) {
  tmp <- rep(0, number_combinations)
  while (index > 0) {
    tmp[number_combinations] <- index %% 2
    index <- index %/% 2
    number_combinations <- number_combinations - 1
  }
  tmp
}
