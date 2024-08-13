#' Pattern number for input patterns defined by decreases, stability and increases
#'
#' @description
#' This function is mostly useful for generating the qualitative trend analysis plots. It returns the pattern numbers for input patterns.
#'
#'@param p matrix with patterns within each row. A pattern is described by a combination of elements -1, 0, 1 for decrease, stability and increase between adjacent visits.
#'
#'@return A numeric vector giving the pattern numbers for all input patterns.
#'
#'@keywords internal

elaborator_calculate_pattern_number <- function(p) {
  . <- NULL
  as.vector((as.matrix(p + 1) %*% as.matrix(3^(0:(dim(p)[2] - 1)))))
}

