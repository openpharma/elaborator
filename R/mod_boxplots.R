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
  shiny::tagList(
    shiny::fluidPage(
      shiny::conditionalPanel(
        condition = "output.flag == true",
        shinydashboard::box(
          width = NULL,
          title = span(shiny::tagList('', icon("cogs"))),
          solidHeader = TRUE,
          background = 'black',
          collapsible = TRUE,
          collapsed = FALSE,
          shiny::fluidRow(
            bsplus::use_bs_popover(),
            bsplus::use_bs_tooltip(),
            shiny::column(
              3,
              shiny::checkboxInput(
                inputId = "sameaxes",
                label = tagList(
                  "Use same scales within lab parameter",
                  bsplus::bs_embed_tooltip(
                    tag = bsplus::shiny_iconlink("question"),
                    title = "Define whether the scales are the same among all treatment groups.
                        Using the same scales among all
                        treatment groups enables a much better comparison between treatment groups.
                        Otherwise, each plot will have its own scale.",
                    placement = "top"
                  )
                ),
                value = FALSE
              ),
              shiny::checkboxInput(
                inputId = "outlier",
                label = tagList(
                  "Use outlier corrected scale",
                  bsplus::bs_embed_tooltip(
                    tag = bsplus::shiny_iconlink("question"),
                    title = "Define whether the scales are outlier corrected or not. Outlier correction
                        uses the five times interquartile range as a definition of outliers.",
                    placement = "top"
                  )
                ),
                value = FALSE
              ),
              bsplus::use_bs_popover(),
              bsplus::use_bs_tooltip(),
              shiny::checkboxInput(
                inputId = "add_points",
                label = tagList(
                  "Patient-specific values",
                  bsplus::bs_embed_tooltip(
                    tag = bsplus::shiny_iconlink("question"),
                    title = "Tick box for plotting patient-specific lab values as single points sorted from smallest to largest.
                        ",
                    placement = "top"
                  )
                ),
                value = FALSE
              ),
              shiny::conditionalPanel(
                condition = "input.add_points == true",
                shiny::checkboxInput(
                  inputId = "sortpoint",
                  label = "Sort patient-specific values",
                  value = FALSE
                )
              ),
              shiny::checkboxInput(
                inputId = "con_lin",
                label = tagList(
                  "Draw connection lines",
                  bsplus::bs_embed_tooltip(
                    tag = bsplus::shiny_iconlink("question"),
                    title = "Tick box for plotting connection lines between patient measurements.
                          If the option 'First/last visit' is selected, the colors indicating increasing or decreasing lab
                          values from first to last visit.
                          If 'Each visit' is selected, the colors indicating increase/decrease between each visit for a single subject.
                          The 'Custom visit' option can be used to select two visits for the increase/decrease indication.
                          If more or less then 2 visits are selected, all lines appear grey.
                          This is also the case for the last option 'All grey'.
                          ",
                    placement = "right"
                  )
                ),
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.con_lin == true",
                prettyRadioButtons(
                  inputId = "con_lin_options",
                  label = "",
                  choices = c(
                    "First/last visit" = "first_last",
                    "Each visit" = "each_visit",
                    "Custom visits" = "custom_visits",
                    "All grey" = "all_grey"
                  ),
                  selected = "first_last",
                  status = "warning",
                  inline = TRUE
                )
              ),
              conditionalPanel(
                condition = "input.con_lin_options == 'custom_visits'",
                shiny::checkboxGroupInput(
                  inputId = "custom_visits",
                  label = "",
                  choices = NULL,
                  selected = NULL,
                  inline = TRUE
                ),
                conditionalPanel(
                  condition = "input.custom_visits.length != 2",
                  class = "color-red",
                  "Please select exactly two visits"
                )
              )
            ),
            shinydashboard::box(
              background = 'black',
              shiny::column(
                4,
                bsplus::use_bs_popover(),
                bsplus::use_bs_tooltip(),
                bsplus::bs_embed_tooltip(
                  tag = h5(span(shiny::tagList(
                    "Test for explorative trend detection",
                    icon("question")
                  ))),
                  title = "Explore whether there are any trends over time (comparison of test results between treatment groups is only recommended for balanced designs). Choose the approproate statistical test. The statistical test aims to assess whether patient-specific changes in laboratory values occur.",
                  placement = "bottom",
                  expanded = TRUE
                ),
                shinyWidgets::prettyRadioButtons(
                  inputId = "stattest",
                  label = "",
                  choices = c(
                    "None" = "none",
                    "Sign test" = "signtest",
                    "t-test" = "ttest"
                  ),
                  selected = "none",
                  status = "warning"
                ),
                conditionalPanel(
                  condition = "input.trtcompar.length > 1 | input.stattest == 'none'",
                  shiny::actionButton(
                    inputId = "go_select2",
                    label = "Update!",
                    icon = icon("redo")
                  )
                )
              ),
              shiny::conditionalPanel(
                condition = "input.stattest != 'none'",
                shiny::column(
                  4,
                  bsplus::use_bs_popover(),
                  bsplus::use_bs_tooltip(),
                  bsplus::bs_embed_tooltip(
                    tag = h5(span(shiny::tagList(
                      "Visits to compare",
                      icon("question")
                    ))),
                    title = "Select which visits you want to test for the existence of a trend. If more than two visits are selected, the first selection is tested against any of the others (pairwise testing).",
                    placement = "top",
                    expanded = TRUE
                  ),
                  shiny::checkboxGroupInput(
                    inputId = "trtcompar",
                    label = "",
                    choices = NULL,
                    selected = NULL
                  ),
                  shiny::conditionalPanel(
                    condition = "output.check <2",
                    class = "color-red",
                    shiny::helpText(
                      "Please select at least 2 visits!"
                    )
                  )
                ),
                shiny::column(
                  3,
                  bsplus::use_bs_popover(),
                  bsplus::use_bs_tooltip(),
                  bsplus::bs_embed_tooltip(
                    tag = h5(span(shiny::tagList(
                      "p-value cutoff",
                      icon("question")
                    ))),
                    title = "Statistical tests are performed for each lab parameter and treatment group. Backgrounds are colored if the respective p-value lies below this p-value threshold.",
                    placement = "top",
                    expanded = TRUE
                  ),
                  shiny::sliderInput(
                    inputId = "pcutoff",
                    label = tags$div(tags$h5(" ")),
                    min = 0,
                    max = 0.2,
                    value = 0.01,
                    step = 0.005
                  )
                )
              )
            ),
            shiny::column(
              2,
              shiny::helpText(
                class = "color-white",
                "You can minimize/maximize this window with the -/+ button on the top right of the panel"
              ),
              shiny::conditionalPanel(
                condition = "input.stattest != 'none'",
                bsplus::use_bs_popover(),
                bsplus::use_bs_tooltip(),
                bsplus::bs_embed_tooltip(
                  tag = h5(span(shiny::tagList(
                    tags$i(
                      class = "fa-solid fa-square decrease"
                    ),
                    "Decrease"
                  ))),
                  title = "Statistical test indicates a decrease in values.",
                  placement = "top",
                  expanded = TRUE
                ),
                bsplus::bs_embed_tooltip(
                  tag = h5(span(shiny::tagList(
                    tags$i(
                      class = "fa-solid fa-square increase"
                    ),
                    "Increase"
                  ))),
                  title = "Statistical test indicates an increase in values.",
                  placement = "top",
                  expanded = TRUE
                ),
                bsplus::bs_embed_tooltip(
                  tag = h5(span(shiny::tagList(
                    tags$i(
                      class = "fa-solid fa-square missing"
                    ),
                    "Missing"
                  ))),
                  title = "Statistical test indicates missing values.",
                  placement = "top",
                  expanded = TRUE
                )
              )
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
          height = "100%",
          shiny::fluidRow(
            shiny::column(
              12,
              shiny::plotOutput(
                outputId = 'dendro_1',
                height = "450px"
              )
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          12,
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
            shiny::fluidRow(
              shiny::column(
                2,
                shiny::actionButton(
                  inputId = "apply_quant_plot",
                  label = paste0('Create Plots'),
                  icon = icon("object-group")
                ),
                shiny::uiOutput('cont1')
              ),
              shiny::column(
                5,
                offset = 2,
                shiny::uiOutput('cont1_text')
              )
            )
          ),
          shiny::uiOutput('tab1', width = 'auto'),
          shiny::conditionalPanel(
            condition = "output.flag == true",
            shiny::uiOutput('hoverpanel')
          )
        )
      )
    )
  )
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
