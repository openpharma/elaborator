elaborator_plot_quant_trends_treatment_label <- function(
    dat1
){
  graphics::par(
    mfrow = c(
    length((unique(dat1$TRTP))),
    1
    ),
    bty = "n",
    mar = c(0, 0, 0, 0),
    oma = c(0, 0, 3, 0),
    cex.main = 1,
    bg = ColorBG
  )
  sapply(
    levels(dat1$TRTP), function(treat) {
      graphics::plot(NULL, NULL, ylim = c(0, 1), xlim = c(0, 1), axes = FALSE, ylab = "", xlab = "")
      graphics::text(x = 0.5, y = 0.5, labels = treat, cex = 1, srt = 90)
    }
  )
}
