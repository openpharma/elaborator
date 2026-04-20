#' Default colours and palettes for the elaborator Shiny app
#'
#' Used as `r$theme` for sharing across golem module servers.
#'
#' @returns A list of colour constants and `colChoice` palettes.
#' @noRd
elaborator_app_theme <- function() {
  colBoxplot4 <- "#004a8a"
  colBoxplot3 <- "#0075bc"
  colBoxplot2 <- "#00b4cb"
  colBoxplot1 <- "#2fb39f"

  colDecrease <- "#47d2bc"
  colIncrease <- "#ffeeaa"

  colLines <- "#f78300"

  colQualitative1 <- "#dff2fd"
  colQualitative2 <- "#c9e1f6"
  colQualitative3 <- "#b0d5f2"
  colQualitative4 <- "#95c7ed"
  colQualitative5 <- "#78b7e5"
  colQualitative6 <- "#57a7d9"
  colQualitative7 <- "#0092cd"
  colQualitative8 <- "#0082be"
  colQualitative9 <- "#0072a9"
  colQualitative10 <- "#00639b"
  colQualitative11 <- "#005c90"
  textcol <- "#f78300"
  arrowcol <- "#f78300"

  colRvbpPos <- "#2fb39f"
  colRvbpNeg <- "#f78300"

  ColorBG <- "#E2F3F2"
  ColorApp <- "#00b4cb"
  ColorPanel <- "#11c4d4"
  ColorHighlight <- "#f6ad82"
  ColorElements <- "#e3e3e3"
  ColorFont <- "#3c3c3b"

  colChoice <- list(
    'sequential orange' = list(
      'col' = RColorBrewer::brewer.pal(9, 'Oranges'),
      'gradient' = TRUE
    ),
    'sequential blue' = list(
      'col' = RColorBrewer::brewer.pal(9, 'Blues'),
      'gradient' = TRUE
    ),
    'sequential green' = list(
      'col' = RColorBrewer::brewer.pal(9, 'Greens'),
      'gradient' = TRUE
    ),
    'sequential grey' = list(
      'col' = RColorBrewer::brewer.pal(9, 'Greys'),
      'gradient' = TRUE
    ),
    'sequential purple' = list(
      'col' = RColorBrewer::brewer.pal(9, 'Purples'),
      'gradient' = TRUE
    ),
    'sequential red' = list(
      'col' = RColorBrewer::brewer.pal(9, 'Reds'),
      'gradient' = TRUE
    ),
    'sequential blue - green' = list(
      'col' = RColorBrewer::brewer.pal(9, 'BuGn'),
      'gradient' = TRUE
    ),
    'sequential blue - purple' = list(
      'col' = RColorBrewer::brewer.pal(9, 'BuPu'),
      'gradient' = TRUE
    ),
    'sequential green - blue' = list(
      'col' = RColorBrewer::brewer.pal(9, 'GnBu'),
      'gradient' = TRUE
    ),
    'sequential orange - red' = list(
      'col' = RColorBrewer::brewer.pal(9, 'OrRd'),
      'gradient' = TRUE
    ),
    'sequential purple - blue' = list(
      'col' = RColorBrewer::brewer.pal(9, 'PuBu'),
      'gradient' = TRUE
    ),
    'sequential purple - blue - green' = list(
      'col' = RColorBrewer::brewer.pal(9, 'PuBuGn'),
      'gradient' = TRUE
    ),
    'sequential purple - red' = list(
      'col' = RColorBrewer::brewer.pal(9, 'PuRd'),
      'gradient' = TRUE
    ),
    'sequential red - purple' = list(
      'col' = RColorBrewer::brewer.pal(9, 'RdPu'),
      'gradient' = TRUE
    ),
    'sequential yellow - green' = list(
      'col' = RColorBrewer::brewer.pal(9, 'YlGn'),
      'gradient' = TRUE
    ),
    'sequential yellow - green - blue' = list(
      'col' = RColorBrewer::brewer.pal(9, 'YlGnBu'),
      'gradient' = TRUE
    ),
    'sequential yellow - orange - brown' = list(
      'col' = RColorBrewer::brewer.pal(9, 'YlOrBr'),
      'gradient' = TRUE
    ),
    'sequential yellow - orange - red' = list(
      'col' = RColorBrewer::brewer.pal(9, 'YlOrRd'),
      'gradient' = TRUE
    )
  )

  list(
    colBoxplot1 = colBoxplot1,
    colBoxplot2 = colBoxplot2,
    colBoxplot3 = colBoxplot3,
    colBoxplot4 = colBoxplot4,
    colDecrease = colDecrease,
    colIncrease = colIncrease,
    colLines = colLines,
    colQualitative1 = colQualitative1,
    colQualitative2 = colQualitative2,
    colQualitative3 = colQualitative3,
    colQualitative4 = colQualitative4,
    colQualitative5 = colQualitative5,
    colQualitative6 = colQualitative6,
    colQualitative7 = colQualitative7,
    colQualitative8 = colQualitative8,
    colQualitative9 = colQualitative9,
    colQualitative10 = colQualitative10,
    colQualitative11 = colQualitative11,
    textcol = textcol,
    arrowcol = arrowcol,
    colRvbpPos = colRvbpPos,
    colRvbpNeg = colRvbpNeg,
    ColorBG = ColorBG,
    ColorApp = ColorApp,
    ColorPanel = ColorPanel,
    ColorHighlight = ColorHighlight,
    ColorElements = ColorElements,
    ColorFont = ColorFont,
    colChoice = colChoice
  )
}
