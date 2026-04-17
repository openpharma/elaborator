#global settings
SUBJIDN <- TRTP <- LBTESTCD <- NULL

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
  'sequential orange' = list('col' = brewer.pal(9, 'Oranges'), 'gradient' = TRUE),
  'sequential blue'   = list('col' = brewer.pal(9, 'Blues'),   'gradient' = TRUE),
  'sequential green'  = list('col' = brewer.pal(9, 'Greens'),  'gradient' = TRUE),
  'sequential grey'   = list('col' = brewer.pal(9, 'Greys'),   'gradient' = TRUE),
  'sequential purple' = list('col' = brewer.pal(9, 'Purples'), 'gradient' = TRUE),
  'sequential red'    = list('col' = brewer.pal(9, 'Reds'),    'gradient' = TRUE),

  'sequential blue - green'  = list('col' = brewer.pal(9, 'BuGn'), 'gradient' = TRUE),
  'sequential blue - purple' = list('col' = brewer.pal(9, 'BuPu'), 'gradient' = TRUE),
  'sequential green - blue'  = list('col' = brewer.pal(9, 'GnBu'), 'gradient' = TRUE),
  'sequential orange - red'  = list('col' = brewer.pal(9, 'OrRd'), 'gradient' = TRUE),
  'sequential purple - blue' = list('col' = brewer.pal(9, 'PuBu'), 'gradient' = TRUE),

  'sequential purple - blue - green'= list('col' = brewer.pal(9, 'PuBuGn'),'gradient' = TRUE),
  'sequential purple - red'         = list('col' = brewer.pal(9, 'PuRd'),  'gradient' = TRUE),
  'sequential red - purple'         = list('col' = brewer.pal(9, 'RdPu'),  'gradient' = TRUE),

  'sequential yellow - green'         = list('col' = brewer.pal(9, 'YlGn'),  'gradient' = TRUE),
  'sequential yellow - green - blue'  = list('col' = brewer.pal(9, 'YlGnBu'),'gradient' = TRUE),
  'sequential yellow - orange - brown'= list('col' = brewer.pal(9, 'YlOrBr'),'gradient' = TRUE),
  'sequential yellow - orange - red'  = list('col' = brewer.pal(9, 'YlOrRd'),'gradient' = TRUE)
)

# Shiny Module for boxplot Color (UI Part)
boxPlotColorUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(ns("controls"))
}

# Shiny Module for boxplot Color (Server Part)
boxPlotColor <- function(input, output, session, dat, name, start_color, number) {


  ns <- session$ns
  output$controls <- shiny::renderUI({

    if (start_color %in% c("Color1","Color5","Color9","Color13","Color17")) {
      bg_col <- colBoxplot1
      sel_col <- "Color1"
    }
    if (start_color %in% c("Color2","Color6","Color10","Color14","Color18")) {
      bg_col <- colBoxplot2
      sel_col <- "Color2"
    }
    if (start_color %in% c("Color3","Color7","Color11","Color15","Color19")) {
      bg_col <- colBoxplot3
      sel_col <- "Color3"
    }
    if (start_color %in% c("Color4","Color8","Color12","Color16","Color20")) {
      bg_col <- colBoxplot4
      sel_col <- "Color4"
    }

    tags$div(
      tags$head(
        tags$style(
          shiny::HTML(
            paste0("
            .btn-info", number, "{color: #fff; background-color:", bg_col,
                   ";}"
            )
          )
        )
      ),

      shinyWidgets::pickerInput(
        inputId = ns("col"),
        label = paste0(name),
        choices = dat,
        selected = sel_col,
        multiple = FALSE,
        options = list(style = paste0("btn-info", number)),
        choicesOpt = list(
          style = c(
            "background-color:#2fb39f !important;color: #ffffff; font-weight: bold;",
            "background-color:#00b4cb !important;color: #ffffff; font-weight: bold;",
            "background-color:#0075bc !important;color: #ffffff; font-weight: bold;",
            "background-color:#004a8a !important;color: #ffffff; font-weight: bold;"
          )
        )
      ), width = "100%")
  })
  return(
    shiny::reactive({
      shiny::validate(shiny::need(input$col, FALSE))
      dat[,input$col]
    })
  )
}

