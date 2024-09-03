elaborator_draw_lines <- function(x){
    apply(x, 1, function(z) {
      lines(
        x = 1:length(colnames(x)),
        y = z,
        col = "#00000040"
      )
      }
    )
}
