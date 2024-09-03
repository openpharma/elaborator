#' Draws a circle with specified size, coordinates and color in a plot window
#'
#' @description
#' This function is mostly useful for generating the reference-value based pattern analysis plots. It draws a circle with specified size, coordinates and color in a plot window.
#'
#' @param x x-coordinate of circle
#' @param y y-coordinate of circle
#' @param height height of curved lines which connect circles
#' @param dot_col color of circle
#' @param pattern_Matrix matrix with the reference-value based pattern information
#' @param number_column number of layers (visits)
#' @param dot_Radius circle radius for circles representing existing patterns
#' @param dot_radius circle radius for circles representing non-existing patterns
#' @param fontsize font size of numbers printed inside the circles
#'
#' @return No return value; this function is called to draw the circles for the reference-value based pattern analysis plot.
#'
#' @keywords internal

elaborator_draw_dots <- function(x,y,
                                 height,
                                 dot_col,
                                 pattern_Matrix,
                                 number_column,
                                 dot_Radius,
                                 dot_radius,
                                 fontsize,
                                 empty_color = "#A9A9A9",
                                 upper_color = "#2fb39f",
                                 lower_color = "#f78300") {

  mid_point <- (y + height) / 2
  total <- sum(pattern_Matrix$number[height:y])
  j <- x + 1
  if (x < number_column) {
    elaborator_draw_curved_line(x, mid_point, j, (mid_point + height) / 2, lwd = 1, col = empty_color)
    elaborator_draw_curved_line(x, mid_point, j, (mid_point + y) / 2, lwd = 1, col = empty_color)
  }

  A1 <- dot_Radius ** 2 * pi
  A2 <- total / sum(pattern_Matrix$number) * A1

  if (total == 0) {
    shape::filledcircle(r1 = 0.5 * dot_radius, r2 = 0, mid = c(x, mid_point), lwd = 1, lcol = empty_color, col = empty_color)
  } else {
    shape::filledcircle(r1 = max(sqrt(A2 / pi), dot_radius), r2 = 0, mid = c(x, mid_point), lwd = 1, lcol = empty_color, col = dot_col)
    if (x == 0) {
      if (fontsize != 0) {
        graphics::text(x, mid_point, total, cex = fontsize, col = "black")
      }
    } else {
      if (fontsize !=0) {
        graphics::text(x, mid_point, total, cex = fontsize, col = "white")
      }
    }
  }

  if (x < number_column) {
    elaborator_draw_dots(j, mid_point, height, dot_col = upper_color, pattern_Matrix = pattern_Matrix, number_column = number_column, dot_Radius = dot_Radius,
                         dot_radius = dot_radius, fontsize = fontsize)
    elaborator_draw_dots(j, y, mid_point, dot_col = lower_color, pattern_Matrix = pattern_Matrix, number_column = number_column, dot_Radius = dot_Radius,
                         dot_radius = dot_radius, fontsize = fontsize)
  }
}
