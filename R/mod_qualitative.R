#' qualitative UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_qualitative_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList()
}

#' qualitative Server Functions
#'
#' @noRd
mod_qualitative_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_qualitative_ui("qualitative_1")

## To be copied in the server
# mod_qualitative_server("qualitative_1")
