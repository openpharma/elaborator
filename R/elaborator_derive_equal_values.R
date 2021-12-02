#' Returns indices for all elements of a vector x that are contained in another vector y
#'
#' @description
#' This function is mostly useful for generating the qualitative trend analysis plots. It returns the indices or all elements of a vector x that are contained in another vector y
#'
#'@param y numeric vector
#'@param x numeric vector
#'
#'@return A numeric vector with the indices for all elements of a vector x that are contained in another vector y.
#'
#'@keywords internal

elaborator_derive_equal_values <- function(y, x) {
  unlist(lapply(y, function(y) which(y == x)))
}
