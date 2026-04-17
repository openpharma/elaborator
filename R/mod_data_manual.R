#' data_manual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_data_manual_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList()
}

#' data_manual Server Functions
#'
#' @noRd
mod_data_manual_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_data_manual_ui("data_manual_1")

## To be copied in the server
# mod_data_manual_server("data_manual_1")
