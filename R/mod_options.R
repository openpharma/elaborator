#' options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_options_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::menuItem(
      text = 'Panel/Plot Size ',
      tabName = 'panelsizeoptions',
      icon = icon('arrows-alt'),
      shiny::sliderInput(
        inputId = 'zoompx',
        label = 'Zoom / Pixel ratio (px)',
        min = 10,
        max = 820,
        value = 100,
        step = 10
      ),
      shiny::sliderInput(
        inputId = 'panelheight',
        label = 'Change panel height',
        min = 400,
        max = 2400,
        value = 500,
        step = 100
      )
    ),
    shinydashboard::menuItem(
      text = 'Arrange Lab Parameters ',
      icon = icon('sort-alpha-down'),
      tabName = 'ordersequoptions',
      bsplus::use_bs_popover(),
      bsplus::use_bs_tooltip(),
      bsplus::bs_embed_tooltip(
        tag = h5(span(shiny::tagList(
          "Order of lab parameters",
          icon("question")
        ))),
        title = "You can choose between three options to arrange laboratory parameters. Details on the AI-sortng option are given in the 'Information'-tab.",
        placement = "top",
        expanded = TRUE
      ),
      shinyWidgets::prettyRadioButtons(
        inputId = "orderinglab",
        label = "",
        choices = c(
          "As in input" = "asinp",
          "AI sorted" = "auto",
          "Alphabetically" = "alphabetically",
          "Manual" = "manual"
        ),
        selected = "alphabetically",
        status = "warning"
      ),
      shiny::conditionalPanel(
        condition = "input.orderinglab == 'manual'",
        shiny::selectizeInput(
          inputId = 'arrange.lab',
          label = 'Labparameter (drag and drop)',
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list('plugins' = list('drag_drop'))
        )
      ),
      shiny::conditionalPanel(
        condition = "input.orderinglab == 'auto'",
        shinyWidgets::pickerInput(
          inputId = 'select.ai.first',
          label = 'Select first visit for change assessment',
          choices = NULL,
          selected = NULL
        ),
        shinyWidgets::pickerInput(
          inputId = 'select.ai.last',
          label = 'Select second visit for change assessment',
          choices = NULL,
          selected = NULL
        ),
        shinyWidgets::pickerInput(
          inputId = 'clusterMethod',
          label = 'Seriation algorithm',
          choices = sort(
            c(
              'BBURCG',
              'BBWRCG',
              'TSP',
              'R2E',
              'MDS_metric',
              'GW_single',
              'GW_complete',
              'GW_average',
              'GW_ward',
              'OLO_single',
              'OLO_complete',
              'OLO_average',
              'OLO_ward',
              'VAT',
              'SA',
              'Spectral',
              'SPIN_NH',
              'SPIN_STS'
            )
          ),
          selected = 'OLO_average',
          multiple = FALSE,
          options = list(
            `live-search` = TRUE,
            `header` = 'Select item'
          )
        )
      ),
      shiny::actionButton(
        inputId = "go3",
        label = "Update selection!",
        icon = icon("redo")
      )
    ),
    shinydashboard::menuItem(
      text = 'Boxplot Colors',
      icon = icon('palette'),
      selected = TRUE,
      startExpanded = FALSE,
      purrr::map(paste0("id", 1:20), ~ boxPlotColorUI(id = .x)),
      shiny::actionButton(
        inputId = "go",
        label = "Update Colors!",
        icon = icon("redo")
      )
    )
  )
}

#' options Server Functions
#'
#' @noRd
mod_options_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_options_ui("options_1")

## To be copied in the server
# mod_options_server("options_1")
