#' Calculates the index of the color vector in the qualitative trend analysis plot
#'
#' @description
#' This function is mostly useful for generating the qualitative trend analysis plots. It returns the index of the color for a percentage which is observed for a specific pattern characterized by decreases/stability/increases from one visit to the next.
#'
#'@param r numeric value specifying the percentage of patients with a specific pattern of decreased/stable/increased values from one visit to the next
#'
#'@return A numeric value between 1 and 11. This value is used as an index for the color legend in the qualitative trends plot in which darker colors reflect more frequent patterns
#'
#'@keywords internal

elaborator_calculate_color_index <- function(r) {
  if (is.na(r)) {
    return(NA)
  }
  h <- floor(0.2 * r) + 1
  if (h <= 11) h else 11
}
