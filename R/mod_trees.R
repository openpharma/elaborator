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
  shiny::tagList(
    shinydashboard::box(
      width = NULL,
      title = span(shiny::tagList('', icon("cogs"))),
      background = 'black',
      solidHeader = TRUE,
      collapsible = TRUE,
      shiny::column(
        2,
        bsplus::use_bs_popover(),
        bsplus::use_bs_tooltip(),
        bsplus::bs_embed_tooltip(
          tag = h5(span(shiny::tagList(
            "Font size",
            icon("question")
          ))),
          title = "Adapt font size. Set font size to 0 to suppress any text.",
          placement = "top",
          expanded = TRUE
        ),
        shiny::sliderInput(
          inputId = 'cex.rvbp',
          label = '',
          min = 0,
          max = 5,
          value = 0,
          step = 0.5
        )
      ),
      shiny::column(
        2,
        bsplus::use_bs_popover(),
        bsplus::use_bs_tooltip(),
        bsplus::bs_embed_tooltip(
          tag = h5(span(shiny::tagList(
            "Definition of abnormal values",
            icon("question")
          ))),
          title = "Select how to define abnormal values based on the upper limit of normal (ULN) and lower limit of normal (LLN).",
          placement = "top",
          expanded = TRUE
        ),
        shinyWidgets::prettyRadioButtons(
          inputId = "criterion",
          label = tags$div(tags$h5("")),
          choices = c(
            "above ULN OR below LLN" = "within",
            "above ULN" = "greater",
            "below LLN" = "less"
          ),
          selected = "within",
          status = "warning"
        )
      ),
      shiny::column(
        2,
        bsplus::use_bs_popover(),
        bsplus::use_bs_tooltip(),
        bsplus::bs_embed_tooltip(
          tag = h5(span(shiny::tagList(
            "Factor multiplied with ULN or LLN",
            icon("question")
          ))),
          title = "Define abnormal values in terms of ULN or LLN multiplied with a positive value. E.g. the factor 2
                  means that abnormal values are defined as values above 2xULN and/or below 2xLLN.",
          placement = "top",
          expanded = TRUE
        ),
        shiny::numericInput(
          inputId = "abnormal_values_factor",
          label = "",
          value = 1,
          min = 0,
          step = 0.1
        )
      ),
      shiny::column(
        width = 2,
        offset = 4,
        shiny::helpText(
          class = "color-white",
          "You can minimize/maximize this window with the -/+ button on the top right of the panel"
        )
      )
    ),
    shiny::conditionalPanel(
      condition = "output.ai == true",
      shinydashboard::box(
        width = NULL,
        title = span(shiny::tagList(
          '',
          icon("sort-alpha-down"),
          'Dendrogram - (Click on the + symbol to open)'
        )),
        solidHeader = TRUE,
        background = 'black',
        collapsible = TRUE,
        collapsed = TRUE,
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::plotOutput(
              outputId = 'dendro_3',
              height = "250px"
            )
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::conditionalPanel(
          condition = "input.abnormal_values_factor >= 0 && input.abnormal_values_factor != undefined && output.flag == true",
          shiny::fluidRow(
            shiny::column(
              2,
              shiny::actionButton(
                inputId = "apply_ref_plot",
                label = paste0('Create Plots'),
                icon = icon("object-group")
              ),
              shiny::uiOutput('cont3')
            ),
            shiny::column(
              5,
              offset = 2,
              shiny::uiOutput('cont3_text')
            )
          )
        ),
        shiny::conditionalPanel(
          condition = "input.abnormal_values_factor < 0 || input.abnormal_values_factor == undefined",
          class = "color-orange",
          "Please enter a non-negative numeric percentage value."
        ),
        shiny::uiOutput('tab3', width = 'auto'),
        shiny::uiOutput('hoverpanel3')
      )
    )
  )
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
