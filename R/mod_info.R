#' info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_info_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList()
}

#' info Server Functions
#'
#' @noRd
mod_info_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_info_ui("info_1")

## To be copied in the server
# mod_info_server("info_1")
