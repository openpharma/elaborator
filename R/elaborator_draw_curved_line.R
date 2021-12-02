#' Draws a curved line from start to end point
#'
#' @description
#' This function is mostly useful for generating the reference-value based pattern analysis plots. It draws a curved line from a start to an end point.
#'
#' @param x1 x-coordinate of the starting point
#' @param x2 x-coordinate of the end point
#' @param y1 y-coordinate of the starting point
#' @param y2 y-coordinate of the end point
#' @param ... further parameters
#'
#' @return No return value; this function is called to draw curved lines of the reference-value based pattern plot.
#'
#' @keywords internal

elaborator_draw_curved_line <- function(x1, y1, x2, y2, ...) {

  A <- matrix(c(x1, x2, 1, 1), nrow = 2, ncol = 2)
  b <- c(-5, 5)
  z <- solve(A, b)

  x <- seq(x1, x2, 0.05)
  y <- (y2 - y1) / 2 * tanh(z[1] * x + z[2]) + (y1 + y2) / 2
  graphics::points(x, y, type = "l", ...)
}
