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
  shiny::fluidPage(
    shiny::conditionalPanel(
      condition = "output.flag == false",
      shiny::HTML(
        "<img src = 'www/BAY_eLaborator_Logo.svg'
              alt = 'Graphic cannot be displayed'
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
              tags$span(
                class = "color-orange",
                "laboratory data"
              ),
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
    ),
    shiny::conditionalPanel(
      condition = "output.flag == true",
      shinydashboard::box(
        width = NULL,
        title = span(shiny::tagList(' ', icon("cogs"))),
        background = 'black',
        solidHeader = TRUE,
        collapsible = TRUE,
        shiny::fluidRow(
          shiny::column(
            2,
            bsplus::bs_embed_tooltip(
              tag = h5(span(shiny::tagList(
                "Font size",
                icon("question")
              ))),
              title = "Adapt font size. Set font size to 0 to exclude any text.",
              placement = "top",
              expanded = TRUE
            ),
            shiny::sliderInput(
              inputId = 'cex.trend',
              label = '',
              min = 0,
              max = 5,
              value = 0,
              step = 0.5
            )
          ),
          shiny::column(
            2,
            bsplus::bs_embed_tooltip(
              tag = h5(span(shiny::tagList(
                "Choose method for defining stability",
                icon("question")
              ))),
              title = "You can specify a tolerated difference in which a change in two adjacent lab values are considered stable ('='). This tolerated difference can be derived as a (small) percentage of the interquartile range (IQR), the range or the reference range. The IQR and the range is evaluated at the first visit across all treatment groups.",
              placement = "bottom",
              expanded = TRUE
            ),
            shinyWidgets::prettyRadioButtons(
              inputId = 'method',
              label = ' ',
              choices = c(
                'Interquartile Range' = 'InQuRa',
                'Range' = 'Range',
                'Reference Range' = 'Reference Range'
              ),
              selected = "InQuRa",
              status = "warning"
            )
          ),
          shiny::column(
            2,
            bsplus::bs_embed_tooltip(
              tag = h5(span(shiny::tagList(
                "Select percentage",
                icon("question")
              ))),
              title = "Select a percent value in the method chosen in order to derive the critical boundary. If set to 0, then adjacent lab values must be exactly equal in order to be considered stable.",
              placement = "top",
              expanded = TRUE
            ),
            shiny::sliderInput(
              inputId = 'percent',
              label = "",
              min = 0,
              max = 20,
              value = 0,
              step = 0.5
            )
          ),
          shiny::column(
            2,
            bsplus::bs_embed_tooltip(
              tag = h5(span(shiny::tagList(
                "Select a color scale",
                icon("question")
              ))),
              title = "Select your favorite color scale used for highlighting frequent patterns.",
              placement = "top",
              expanded = TRUE
            ),
            shinyWidgets::pickerInput(
              inputId = 'select.pal1',
              label = "",
              choices = names(colChoice),
              selected = names(colChoice)[1],
              multiple = FALSE,
              options = list(
                `live-search` = TRUE,
                `style` = 'background: btn-warning',
                `header` = 'Select item'
              )
            ),
            shiny::plotOutput('prev.pal1', height = '20px')
          ),
          shiny::column(
            width = 2,
            offset = 4,
            shiny::helpText(
              class = "color-white",
              "You can minimize/maximize this window with the -/+ button on the top right of the panel"
            )
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
                'dendro_2',
                height = "250px"
              )
            )
          )
        )
      ),
      shiny::conditionalPanel(
        condition = "output.flag == true",
        shiny::fluidRow(
          shiny::column(
            2,
            shiny::actionButton(
              inputId = "apply_qual_plot",
              label = paste0('Create Plots'),
              icon = icon("object-group")
            ),
            shiny::uiOutput('cont2')
          ),
          shiny::column(
            5,
            offset = 2,
            shiny::uiOutput('cont2_text')
          )
        )
      ),
      shiny::uiOutput('tab2', width = 'auto'),
      shiny::uiOutput('legendpanel'),
      shiny::uiOutput('hoverpanel2')
    )
  )
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
