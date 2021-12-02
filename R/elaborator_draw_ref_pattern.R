#' Generates tree diagram according to the reference-value based pattern analysis
#'
#' @description
#' Generate tree diagram showing the number of patients with normal/abnormal values at each visit of a study according to the reference-value based pattern analysis.
#'
#' @param pattern_Matrix matrix with reference-value based pattern information
#' @param fontsize font size of numbers printed inside the circles
#' @param number_columns number of layers (visits) of tree diagram
#' @param background_color background color
#'
#' @return No return value; this function is called to draw a tree diagram according to the reference-value based pattern analysis.
#'
#' @keywords internal

elaborator_draw_ref_pattern <- function(pattern_Matrix, fontsize, number_columns, background_color = "#E2F3F2") {

  graphics::plot(
    0,
    0,
    type = "n",
    axes = FALSE,
    xlab = "",
    ylab = "",
    xlim = c(-1, number_columns + 0.5),
    ylim = c(0, 2^number_columns + 1)
  )

  graphics::rect(
    xleft = graphics::par()$usr[1] - 0.1,
    ybottom = graphics::par()$usr[3] - 0.1,
    xright = graphics::par()$usr[2] + 0.1,
    ytop = graphics::par()$usr[4] + 0.1,
    col = background_color
  )

  elaborator_draw_dots(
    x = 0,
    y = 1,
    height = 2^number_columns,
    dot_col = "white",
    pattern_Matrix = pattern_Matrix,
    number_column = number_columns,
    dot_Radius = 0.3,
    dot_radius = 0.07,
    fontsize = fontsize
  )

}
