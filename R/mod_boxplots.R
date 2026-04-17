#' boxplots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_boxplots_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList()
}

#' boxplots Server Functions
#'
#' @noRd
mod_boxplots_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_boxplots_ui("boxplots_1")

## To be copied in the server
# mod_boxplots_server("boxplots_1")
