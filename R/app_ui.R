#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    shiny::fluidPage(
      shinydashboard::dashboardPage(
        title = "elaborator",
        shinydashboard::dashboardHeader(
          title = shiny::img(
            src = 'BAY_eLaborator_Logo-lang_Negativ.svg',
            height = 24,
            align = "left"
          ),
          titleWidth = 250
        ),
        #### dashboardSidebar ####
        shinydashboard::dashboardSidebar(
          shinydashboard::sidebarMenu(
            id = 'sidebarmenu',
            shinydashboard::menuItem(
              text = 'Quantitative Trends',
              icon = icon('chart-line'),
              tabName = 'quant'
            ),
            shinydashboard::menuItem(
              text = 'Qualitative Trends',
              icon = icon('buromobelexperte'),
              tabName = 'qual'
            ),
            shinydashboard::menuItem(
              text = 'Reference-value Based Patterns',
              icon = icon('cloudsmith'),
              tabName = 'rvbp'
            ),
            shinydashboard::menuItem(
              text = 'Graphic Options',
              tabName = 'options',
              icon = icon('cogs'),
              startExpanded = FALSE,
              mod_options_ui("options_1")
            ),
            # shinydashboard::menuItem(
            #   text = 'Graphic Options',
            #   tabName = 'options',
            #   icon = icon('cogs')
            # ),
            # shinydashboard::menuItem(
            #   text = 'Data Upload',
            #   tabName = 'datimport',
            #   icon = icon('file-upload'),
            #   selected = TRUE
            # ),
            shinydashboard::menuItem(
              text = 'Data Upload',
              tabName = 'datimport',
              icon = icon('file-upload'),
              selected = TRUE,
              startExpanded = TRUE,
              mod_upload_ui("upload_1")
            ),
            shinydashboard::menuItem(
              "Filter",
              icon = icon("filter"),
              tabName = "filter"
            ),
            shinydashboard::menuItem(
              "Raw Data",
              icon = icon("file-lines"),
              tabName = "raw_data"
            ),
            shinydashboard::menuItem(
              text = "Data Manual",
              icon = icon("file"),
              tabName = "datamanual"
            ),
            shinydashboard::menuItem(
              text = "Information",
              icon = icon("info"),
              tabName = "helptext"
            ),
            "elaborator Version 1.3"
          )
        ),
        #### dashboardBody ####
        shinydashboard::dashboardBody(
          shinyWidgets::chooseSliderSkin(
            skin = "Modern",
            color = "#f6ad82"
          ),
          shinydashboard::tabItems(
            shinydashboard::tabItem(
              tabName = "quant",
              mod_boxplots_ui("boxplots_1")
            ),
            # shinydashboard::tabItem(
            #   tabName = "datimport",
            #   mod_upload_ui("upload_1")
            # ),
            # shinydashboard::tabItem(
            #   tabName = "options",
            #   mod_options_ui("options_1")
            # ),
            shinydashboard::tabItem(
              tabName = "datamanual",
              mod_data_manual_ui("data_manual_1")
            ),
            shinydashboard::tabItem(
              tabName = "helptext",
              mod_info_ui("info_1")
            ),
            shinydashboard::tabItem(
              tabName = "qual",
              mod_qualitative_ui("qualitative_1")
            ),
            shinydashboard::tabItem(
              tabName = "filter",
              shiny::fluidPage(
                shiny::conditionalPanel(
                  condition = "output.flag == false",
                  mod_welcome_ui("welcome_filter")
                ),
                shiny::conditionalPanel(
                  condition = "output.flag == true",
                  mod_filter_ui("filter_1")
                )
              )
            ),
            shinydashboard::tabItem(
              tabName = "raw_data",
              shiny::fluidPage(
                shiny::conditionalPanel(
                  condition = "output.flag == false",
                  mod_welcome_ui("welcome_raw_data")
                ),
                shiny::conditionalPanel(
                  condition = "output.flag == true",
                  mod_raw_data_ui("raw_data_1")
                )
              )
            ),
            shinydashboard::tabItem(
              tabName = "rvbp",
              shiny::fluidPage(
                shiny::conditionalPanel(
                  condition = "output.flag == false",
                  mod_welcome_ui("welcome_trees")
                ),
                shiny::conditionalPanel(
                  condition = "output.flag == true",
                  mod_trees_ui("trees_1")
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "elaborator"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
