#' filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_filter_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput("filter_percentage"),
    shiny::uiOutput("pickerinput_filter"),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::actionButton(
          inputId = "insertBtn",
          label = "Add",
          icon = icon("plus")
        )
      ),
      shiny::column(
        4,
        shiny::actionButton(
          inputId = "removeBtn",
          label = "Delete",
          icon = icon("minus")
        )
      )
    ),
    shiny::tags$div(id = "placeholder"),
    shiny::actionButton(
      inputId = "apply",
      label = "Apply Filter Selection!",
      icon = icon("redo"),
      class = "redo-button"
    )
  )
}

#' filter Server Functions
#'
#' @noRd
mod_filter_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_filter_ui("filter_1")

## To be copied in the server
# mod_filter_server("filter_1")
