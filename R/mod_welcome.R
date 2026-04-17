#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_welcome_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::HTML(
      "<img src = 'www/BAY_eLaborator_Logo.svg'
        alt = 'elaborator logo'
        width = '682'
        height = '286'>"
    ),
    h2(
      "is a novel concept for generating knowledge and gaining insights into laboratory data. You will be able to efficiently and easily explore your laboratory data
              from different perspectives."
    ),
    br(),
    tags$div(
      HTML(
        paste(
          "<i class='fa fa-file-upload'></i>&emsp;",
          tags$span(
            class = "larger-font",
            "Upload your",
            tags$span(class = "color-orange", "laboratory data"),
            " by using the 'Data Upload'-tab in the task bar on the left.
                    Select the file format and click
                    the 'Browse...'-button.",
            sep = ""
          )
        )
      )
    ),
    tags$div(
      HTML(
        paste(
          "<i class= 'fa fa-file'></i>&emsp;",
          tags$span(
            class = "larger-font",
            "Click the 'Data Manual'-tab for the required format and structure for laboratory data file."
          )
        )
      )
    ),
    tags$div(
      HTML(
        paste(
          "<i class='fa fa-info'></i>&emsp;",
          tags$span(
            class = "larger-font",
            " If you want to access information on the elaborator, click the 'Information'-tab.",
            sep = ""
          )
        )
      )
    )
  )
}

#' welcome Server Functions
#'
#' @noRd
mod_welcome_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_welcome_ui("welcome_1")

## To be copied in the server
# mod_welcome_server("welcome_1")
