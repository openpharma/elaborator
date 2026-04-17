#' trees UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_trees_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList()
}

#' trees Server Functions
#'
#' @noRd
mod_trees_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_trees_ui("trees_1")

## To be copied in the server
# mod_trees_server("trees_1")
