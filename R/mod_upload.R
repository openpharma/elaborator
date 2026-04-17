#' upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::prettyRadioButtons(
      inputId = 'impswitch',
      label = 'Select file format',
      status = "warning",
      shape = 'round',
      animation = 'smooth',
      choices = c('*.RData file', '*.CSV file')
    ),
    htmlOutput("err_message"),
    shiny::uiOutput('impdata'),
    shiny::conditionalPanel(
      condition = "output.flag == true",
      shiny::selectizeInput(
        inputId = 'select.visit',
        label = 'Visits (exclude and rearrange)',
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list('plugins' = list('remove_button', 'drag_drop'))
      ),
      shiny::selectizeInput(
        inputId = 'select.treatments',
        label = 'Treatment groups (exclude and rearrange)',
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(
          'plugins' = list('remove_button', 'drag_drop')
        )
      ),
      shinyWidgets::pickerInput(
        inputId = 'select.lab',
        label = 'Lab parameters',
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = 'count > 0',
          `count-selected-text` = '{0} selected (of {1})',
          `live-search` = TRUE,
          `header` = 'Select multiple items',
          `none-selected-text` = 'All dropped!'
        )
      ),
      bsplus::bs_embed_tooltip(
        tag = h5(span(shiny::tagList(
          "Tolerated missings percentage:",
          icon("question")
        ))),
        title = "Select percentage of missing values per
            visit allowed to be still included in the analysis
            (100% means no visits will be removed).",
        placement = "top",
        expanded = TRUE
      ),
      shiny::sliderInput(
        inputId = 'select.toleratedPercentage',
        label = '',
        min = 25,
        max = 100,
        value = 50,
        step = 5,
        post = "%"
      )
    )
  )
}

#' upload Server Functions
#'
#' @noRd
mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_upload_ui("upload_1")

## To be copied in the server
# mod_upload_server("upload_1")
