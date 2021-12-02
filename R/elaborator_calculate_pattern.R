#' Returns row of a matrix containing all combinations of elements -1, 0, 1 of a pre-specified length
#'
#' @description
#' This function is mostly useful for generating the qualitative trend analysis plots. It returns the (i-1)-th row of a matrix containing all combinations of elements -1, 0 and 1 of a pre-specified length.
#'
#'@param index selected row -1 of the combination matrix which shall be returned
#'@param number_combinations length of the combination vector
#'
#'@return A numeric vector containing a specific combination of elements -1, 0 and 1; this combination is used to calculate pattern for qualitative trend analysis plot.
#'
#'@keywords internal

elaborator_calculate_pattern <- function(index, number_combinations) {
  i <- 0
  r <- rep(0, number_combinations)
  while (index > 0) {
    i <- i + 1
    r[i] <- index %% 3
    index <- index %/% 3
  }
  r - 1
}
