#global settings
SUBJIDN <- TRTP <- LBTESTCD <- NULL

colBoxplot4 <- "#004a8a"
colBoxplot3 <- "#0075bc"
colBoxplot2 <- "#00b4cb"
colBoxplot1 <- "#2fb39f"

colDecrease <- "#47d2bc"
colIncrease <- "#ffeeaa"

colLines <- "#f78300"

colQualitative1 <- "#dff2fd"
colQualitative2 <- "#c9e1f6"
colQualitative3 <- "#b0d5f2"
colQualitative4 <- "#95c7ed"
colQualitative5 <- "#78b7e5"
colQualitative6 <- "#57a7d9"
colQualitative7 <- "#0092cd"
colQualitative8 <- "#0082be"
colQualitative9 <- "#0072a9"
colQualitative10 <- "#00639b"
colQualitative11 <- "#005c90"

textcol <- "#f78300"
arrowcol <- "#f78300"
colRvbpPos <- "#2fb39f"
colRvbpNeg <- "#f78300"

ColorBG <- "#E2F3F2"
ColorApp <- "#00b4cb"
ColorPanel <- "#11c4d4"
ColorHighlight <- "#f6ad82"
ColorElements <- "#e3e3e3"
ColorFont <- "#3c3c3b"

colChoice <- list(
  'sequential orange' = list('col' = brewer.pal(9, 'Oranges'), 'gradient' = TRUE),
  'sequential blue'   = list('col' = brewer.pal(9, 'Blues'),   'gradient' = TRUE),
  'sequential green'  = list('col' = brewer.pal(9, 'Greens'),  'gradient' = TRUE),
  'sequential grey'   = list('col' = brewer.pal(9, 'Greys'),   'gradient' = TRUE),
  'sequential purple' = list('col' = brewer.pal(9, 'Purples'), 'gradient' = TRUE),
  'sequential red'    = list('col' = brewer.pal(9, 'Reds'),    'gradient' = TRUE),

  'sequential blue - green'  = list('col' = brewer.pal(9, 'BuGn'), 'gradient' = TRUE),
  'sequential blue - purple' = list('col' = brewer.pal(9, 'BuPu'), 'gradient' = TRUE),
  'sequential green - blue'  = list('col' = brewer.pal(9, 'GnBu'), 'gradient' = TRUE),
  'sequential orange - red'  = list('col' = brewer.pal(9, 'OrRd'), 'gradient' = TRUE),
  'sequential purple - blue' = list('col' = brewer.pal(9, 'PuBu'), 'gradient' = TRUE),

  'sequential purple - blue - green'= list('col' = brewer.pal(9, 'PuBuGn'),'gradient' = TRUE),
  'sequential purple - red'         = list('col' = brewer.pal(9, 'PuRd'),  'gradient' = TRUE),
  'sequential red - purple'         = list('col' = brewer.pal(9, 'RdPu'),  'gradient' = TRUE),

  'sequential yellow - green'         = list('col' = brewer.pal(9, 'YlGn'),  'gradient' = TRUE),
  'sequential yellow - green - blue'  = list('col' = brewer.pal(9, 'YlGnBu'),'gradient' = TRUE),
  'sequential yellow - orange - brown'= list('col' = brewer.pal(9, 'YlOrBr'),'gradient' = TRUE),
  'sequential yellow - orange - red'  = list('col' = brewer.pal(9, 'YlOrRd'),'gradient' = TRUE)
)

# Shiny Module for boxplot Color (UI Part)
boxPlotColorUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(ns("controls"))
}

# Shiny Module for boxplot Color (Server Part)
boxPlotColor <- function(input, output, session, dat, name, start_color, number) {


  ns <- session$ns
  output$controls <- shiny::renderUI({

    if (start_color %in% c("Color1","Color5","Color9","Color13","Color17")) {
      bg_col <- colBoxplot1
      sel_col <- "Color1"
    }
    if (start_color %in% c("Color2","Color6","Color10","Color14","Color18")) {
      bg_col <- colBoxplot2
      sel_col <- "Color2"
    }
    if (start_color %in% c("Color3","Color7","Color11","Color15","Color19")) {
      bg_col <- colBoxplot3
      sel_col <- "Color3"
    }
    if (start_color %in% c("Color4","Color8","Color12","Color16","Color20")) {
      bg_col <- colBoxplot4
      sel_col <- "Color4"
    }

    tags$div(
      tags$head(
        tags$style(
          shiny::HTML(
            paste0("
            .btn-info", number, "{color: #fff; background-color:", bg_col,
                   ";}"
            )
          )
        )
      ),

      shinyWidgets::pickerInput(
        inputId = ns("col"),
        label = paste0(name),
        choices = dat,
        selected = sel_col,
        multiple = FALSE,
        options = list(style = paste0("btn-info", number)),
        choicesOpt = list(
          style = c(
            "background-color:#2fb39f !important;color: #ffffff; font-weight: bold;",
            "background-color:#00b4cb !important;color: #ffffff; font-weight: bold;",
            "background-color:#0075bc !important;color: #ffffff; font-weight: bold;",
            "background-color:#004a8a !important;color: #ffffff; font-weight: bold;"
          )
        )
      ), width = "100%")
  })
  return(
    shiny::reactive({
      shiny::validate(shiny::need(input$col, FALSE))
      dat[,input$col]
    })
  )
}

#' User Interface of the elaborator application
#'
#' @param id Internal parameters for {shiny}.
#'
#' @return No return value. User interface part of the app, used in launch_elaborator-function.
#'
#' @keywords internal

#### dashboardPage ####
elaborator_ui <- function() {

  shinydashboard::dashboardPage(
  title = "elaborator",
  shinydashboard::dashboardHeader(
    title = shiny::img(
      src = 'www/BAY_eLaborator_Logo-lang_Negativ.svg',
      height = 24,
      align ="left"
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
        shinydashboard::menuItem(
          text = 'Panel/Plot Size ',
          tabName = 'panelsizeoptions',
          icon = icon('arrows-alt'),
          shiny::sliderInput(
            inputId = 'zoompx',
            label = 'Zoom / Pixel ratio (px)',
            min = 10,
            max = 820,
            value = 100,
            step = 10
          ),
          shiny::sliderInput(
            inputId = 'panelheight',
            label = 'Change panel height',
            min = 400,
            max = 2400,
            value = 500,
            step = 100
          )
        ),
        shinydashboard::menuItem(
          text = 'Arrange Lab Parameters ',
          icon = icon('sort-alpha-down'),
          tabName = 'ordersequoptions',
          bsplus::use_bs_popover(),
          bsplus::use_bs_tooltip(),
          bsplus::bs_embed_tooltip(
            tag = h5(span(shiny::tagList("Order of lab parameters", icon("question")))),
            title = "You can choose between three options to arrange laboratory parameters. Details on the AI-sortng option are given in the 'Information'-tab.", placement = "top", expanded = TRUE
          ),
          shinyWidgets::prettyRadioButtons(
            inputId = "orderinglab",
            label = "",
            choices = c(
              "As in input" = "asinp",
              "AI sorted" = "auto",
              "Alphabetically" = "alphabetically",
              "Manual" = "manual"
            ),
            selected = "alphabetically",
            status = "warning"
          ),
          shiny::conditionalPanel(condition = "input.orderinglab == 'manual'",
            shiny::selectizeInput(
              inputId = 'arrange.lab',
              label = 'Labparameter (drag and drop)',
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list('plugins' = list('drag_drop'))
            )
          ),
          shiny::conditionalPanel(condition = "input.orderinglab == 'auto'",
            shinyWidgets::pickerInput(
              inputId = 'select.ai.first',
              label = 'Select first visit for change assessment',
              choices = NULL,
              selected = NULL
            ),
            shinyWidgets::pickerInput(
              inputId = 'select.ai.last',
              label = 'Select second visit for change assessment',
              choices = NULL,
              selected = NULL
            ),
            shinyWidgets::pickerInput(
              inputId ='clusterMethod',
              label = 'Seriation algorithm',
              choices = sort(
                c(
                  'BBURCG', 'BBWRCG', 'TSP', 'R2E', 'MDS_metric',
                  'GW_single', 'GW_complete', 'GW_average', 'GW_ward',
                  'OLO_single', 'OLO_complete', 'OLO_average', 'OLO_ward',
                  'VAT','SA', 'Spectral',
                  'SPIN_NH', 'SPIN_STS'
                )
              ),
              selected = 'OLO_average',
              multiple = FALSE,
              options = list(
                `live-search` = TRUE,
                `header` = 'Select item'
              )
            )
          ),
          shiny::actionButton(
            inputId = "go3",
            label = "Update selection!",
            icon = icon("redo"),
            style = paste0(
              "color: ",
              ColorBG,"; background-color: ",
              ColorHighlight,"; border-color: ",
              ColorBG
            )
          )
        ),
        shinydashboard::menuItem(
          text = 'Boxplot Colors',
          icon = icon('palette'),
          selected = TRUE,
          startExpanded = FALSE,
          purrr::map(paste0("id", 1:20), ~ boxPlotColorUI(id = .x)),
          shiny::actionButton(
            inputId = "go",
            label = "Update Colors!",
            icon = icon("redo"),
            style = paste0("color: ", ColorBG, "; background-color: ", ColorHighlight, "; border-color: ", ColorBG)
          )
        )
      ),
      shinydashboard::menuItem(
        text = 'Data Upload',
        tabName = 'datimport',
        icon = icon('file-upload'),
        selected = TRUE,
        startExpanded = TRUE,
        shinyWidgets::prettyRadioButtons(
          inputId = 'impswitch',
          label = 'Select file format',
          status ="warning",
          shape = 'round',
          animation = 'smooth',
          choices = c('*.RData file', '*.CSV file','Use file creation tab')
        ),
        htmlOutput("err_message"),
        tags$head(
          tags$style(
            "#err_message{color: red;
           font-size: 12px;
           margin-size: 20px;
           }"
          )
        ),
        shiny::uiOutput('impdata'),
        shiny::conditionalPanel(condition = "output.flag == true",
          shiny::selectizeInput(
            inputId = 'select.visit',
            label = 'Visits (exclude and rearrange)',
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list('plugins' = list('remove_button', 'drag_drop'))
          ),
          shiny::selectizeInput(
            inputId = 'select.treatments',
            label = 'Treatment groups (exclude and rearrange)',
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(
              'plugins' = list('remove_button', 'drag_drop')
            )
          ),
          shinyWidgets::pickerInput(
            inputId = 'select.lab',
            label = 'Lab parameters',
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `selected-text-format` = 'count > 0',
              `count-selected-text` =  '{0} selected (of {1})',
              `live-search` = TRUE,
              `header` = 'Select multiple items',
              `none-selected-text` = 'All dropped!'
            )
          ),
          bsplus::bs_embed_tooltip(
            tag = h5(span(shiny::tagList("Tolerated missings percentage:", icon("question")))),
            title = "Select percentage of missing values per
            visit allowed to be still included in the analysis
            (100% means no visits will be removed).",
            placement = "top",
            expanded =TRUE
          ),
          shiny::sliderInput(
            inputId = 'select.toleratedPercentage',
            label ='',
            min = 25,
            max = 100,
            value = 50,
            step = 5,
            post = "%"
          )
        )
      ),
      shinydashboard::menuItem(
        "File Creation (SAS data)",
        tabName = "sas_data",
        icon = icon("calculator"),
        badgeLabel = "new",
        badgeColor = "green"
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
      paste0("elaborator Version ",packageVersion("elaborator"))
    )
  ),
  #### dashboardBody ####
  shinydashboard::dashboardBody(
    shiny::tags$head(
      shiny::tags$style(
        HTML(".shiny-notification {
        position:fixed;
        top: calc(50%);
        left: calc(40%);
        width: 350px;
        font-size: 30px;
        background-color: white;
        font-color: black;
        color: #424242;
        }"
        )
      )
    ),
    tags$head(
      tags$style(
        ".shiny-progress {
          top: 50% !important;
          left: 50% !important;
          margin-top: -100px !important;
          margin-left: -250px !important;
          color: blue;
          font-size: 20px;
          font-style: italic;
        }"
      )
    ),
    tags$head(
      tags$script(
        '$(document).on("shiny:connected", function(e) {
          Shiny.onInputChange("innerWidth", window.innerWidth);
        });
        $(window).resize(function(e) {
          Shiny.onInputChange("innerWidth", window.innerWidth);
        });'
      )
    ),
    tags$head(
      tags$style(
        shiny::HTML(
          paste0(
            ".content-wrapper, .right-side { background-color: ", ColorBG, ";}
             .checkbox-inline, .radio-inline {text-align: center; margin-left: 0px;
             margin-right: 0px;padding: 0px;width: 20%;}
             .main-sidebar .sidebar .sidebar-menu .treeview-menu  {background-color: ", ColorPanel, " !important;}
             .main-sidebar .sidebar .sidebar-menu .treeview-menu li:hover a {background-color: ", ColorApp, " !important;}
             .skin-blue .main-header .logo { background-color: ", ColorApp, ";}
             .skin-blue .main-header .logo:hover {background-color: ", ColorApp, ";}
             .progress-bar{background-color:", ColorHighlight, ";}
             .radio-item-warning {color: ", ColorHighlight, "}
             .btn-warning{ background-color:", ColorHighlight, ";}
             .btn-warning:hover{ background-color:", ColorHighlight, ";}
             .skin-blue .main-header .navbar {background-color: ", ColorApp, ";}
             /* main sidebar */
             .skin-blue .main-sidebar {background-color: ", ColorApp, ";}
             /* active selected tab in the sidebarmenu */
             .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: ", ColorPanel, ";}
             /* other links in the sidebarmenu */
             .skin-blue .main-sidebar .sidebar .sidebar-menu a{background-color: ", ColorApp, ";color: #ffffff;}
             .skin-blue .sidebar-menu > li.active > a,
             .skin-blue .sidebar-menu > li:hover > a {border-left-color: ", ColorHighlight, ";}
             /* other links in the sidebarmenu when hovered */
             .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{background-color: ", ColorPanel, ";}
             /* toggle button when hovered  */
             .skin-blue .main-header .navbar .sidebar-toggle:hover{background-color: ", ColorBG, ";}
             .skin-blue .main-sidebar .navbar { background-color: ", ColorApp, ";}
              .skin-blue .main-header .navbar .sidebar-toggle:hover{background-color: ", ColorBG, ";}"
          )
        )
      )
    ),
    shinyWidgets::chooseSliderSkin(
      skin = "Modern",
      color = "#f6ad82"
    ),
    tags$style(
      type = 'text/css',
      paste0(".bg-black {background-color: ", ColorApp, "!important; }")
    ),
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "quant",
        shiny::fluidPage(
          shiny::tags$head(
            shiny::tags$style(
              ".fa-question {color:#e3e3e3}",
              ".fa-plus {color:#ffffff}",
              ".fa-minus {color:#ffffff}",
              ".fa-square {color:#47d2bc}",
              ".fa-stop {color: #ffeeaa}",
              ".fa-flask {color: ", ColorBG, "}"
            )
          ),
          shiny::conditionalPanel(condition = "output.flag == true",
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
                shiny::column(3,
                    shiny::checkboxInput(
                      inputId = "sameaxes",
                      label = tagList("Use same scales within lab parameter",bsplus::bs_embed_tooltip(
                        tag = bsplus::shiny_iconlink("question"),
                        title = "Define whether the scales are the same among all treatment groups.
                        Using the same scales among all
                        treatment groups enables a much better comparison between treatment groups.
                        Otherwise, each plot will have its own scale.",
                        placement = "top"
                      )),
                      value = FALSE
                    ),
                    shiny::checkboxInput(
                      inputId = "outlier",
                      label = tagList("Use outlier corrected scale",bsplus::bs_embed_tooltip(
                        tag = bsplus::shiny_iconlink("question"),
                        title = "Define whether the scales are outlier corrected or not. Outlier correction
                        uses the five times interquartile range as a definition of outliers.",
                        placement = "top"
                      )),
                      value = FALSE
                    ),
                    bsplus::use_bs_popover(),
                    bsplus::use_bs_tooltip(),
                    shiny::checkboxInput(
                      inputId = "add_points",
                      label = tagList("Patient-specific values",bsplus::bs_embed_tooltip(
                        tag = bsplus::shiny_iconlink("question"),
                        title = "Tick box for plotting patient-specific lab values as single points sorted from smallest to largest.
                        ",
                        placement = "top"
                      )),
                      value = FALSE
                    ),
                    shiny::conditionalPanel(condition = "input.add_points == true",
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
                  conditionalPanel(condition = "input.con_lin == true",
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
                  conditionalPanel(condition = "input.con_lin_options == 'custom_visits'",
                    shiny::checkboxGroupInput(
                      inputId = "custom_visits",
                      label = "",
                      choices = NULL,
                      selected = NULL,
                      inline = TRUE
                    ),
                    conditionalPanel(condition = "input.custom_visits.length != 2",
                      HTML("<p style='color: red'> Please select exactly two visits </p>")
                    )
                  )
                ),
                shinydashboard::box(
                  background = 'black',
                  shiny::column(4,
                    bsplus::use_bs_popover(),
                    bsplus::use_bs_tooltip(),
                    bsplus::bs_embed_tooltip(
                      tag = h5(span(shiny::tagList("Test for explorative trend detection", icon("question")))),
                      title = "Explore whether there are any trends over time (comparison of test results between treatment groups is only recommended for balanced designs). Choose the approproate statistical test. The statistical test aims to assess whether patient-specific changes in laboratory values occur.",
                      placement = "bottom",
                      expanded =TRUE
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
                    conditionalPanel(condition = "input.trtcompar.length > 1 | input.stattest == 'none'",
                      shiny::actionButton(
                        inputId = "go_select2",
                        label = "Update!",
                        icon = icon("redo"),
                        style = paste0(
                          "color: ",
                          ColorBG,
                          "; background-color: ",
                          ColorHighlight,
                          "; border-color: ",
                          ColorBG
                        )
                      )
                    )
                  ),
                  shiny::conditionalPanel(condition = "input.stattest != 'none'",
                    shiny::column(4,
                      bsplus::use_bs_popover(),
                      bsplus::use_bs_tooltip(),
                      bsplus::bs_embed_tooltip(
                        tag = h5(span(shiny::tagList("Visits to compare", icon("question")))),
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
                      shiny::conditionalPanel(condition = "output.check <2",
                        shiny::helpText(
                          HTML(
                            '<p style="color:red"> Please select at least 2 visits! </p>'
                          )
                        )
                      )
                    ),
                    shiny::column(3,
                      bsplus::use_bs_popover(),
                      bsplus::use_bs_tooltip(),
                      bsplus::bs_embed_tooltip(
                        tag = h5(span(shiny::tagList("p-value cutoff", icon("question")))),
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
                shiny::column(2,
                  shiny::helpText(
                    HTML(
                      '<p style="color:white"> You can minimize/maximize this window with the -/+ button on the top right of the panel </p>'
                    )
                  ),
                  shiny::conditionalPanel(condition = "input.stattest != 'none'",
                    bsplus::use_bs_popover(),
                    bsplus::use_bs_tooltip(),
                    bsplus::bs_embed_tooltip(tag = h5(span(shiny::tagList(tags$i(class = "fa-solid fa-square", style = "color:#47d2bc"), "Decrease"))),
                      title = "Statistical test indicates a decrease in values.",
                      placement = "top",
                      expanded = TRUE
                    ),
                    bsplus::bs_embed_tooltip(tag = h5(span(shiny::tagList(tags$i(class = "fa-solid fa-square", style = "color:#ffeeaa"), "Increase"))),
                      title = "Statistical test indicates an increase in values.",
                      placement = "top",
                      expanded = TRUE
                    ),
                    bsplus::bs_embed_tooltip(tag = h5(span(shiny::tagList(tags$i(class = "fa-solid fa-square", style = "color:#A9A9A9"),"Missing"))),
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
              title = span(shiny::tagList('', icon("sort-alpha-down"),'Dendrogram - (Click on the + symbol to open)')),
              solidHeader = TRUE,
              background = 'black',
              collapsible = TRUE,
              collapsed = TRUE,
              height = "100%",
              shiny::fluidRow(
                shiny::column(12,
                  shiny::plotOutput(
                    outputId = 'dendro_1',
                    height = "450px"
                  )
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(12,
              shiny::conditionalPanel(condition = "output.flag == false",
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
                        style = "font-size:150%",
                        "Upload your",
                        tags$span(style = "color:#f78300", "laboratory data"),
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
                      tags$span(style = "font-size:150%","Click the 'Data Manual'-tab for the required format and structure for laboratory data file.")
                    )
                  )
                ),
                tags$div(
                  HTML(
                    paste("<i class='fa fa-info'></i>&emsp;",
                      tags$span(style = "font-size:150%"," If you want to access information on the elaborator, click the 'Information'-tab.", sep = "")
                    )
                  )
                )
              ),
              shiny::conditionalPanel(condition = "output.flag == true",
                shiny::fluidRow(
                  shiny::column(2,
                    shiny::actionButton(
                      inputId = "apply_quant_plot",
                      label = paste0('Create Plots'),
                      icon = icon("object-group")
                    ),
                    shiny::uiOutput('cont1')
                  ),
                  shiny::column(5, offset = 2,
                    shiny::uiOutput('cont1_text')
                  )
                )
              ),
              shiny::uiOutput('tab1', width = 'auto'),
              shiny::conditionalPanel(condition = "output.flag == true",
                shiny::uiOutput('hoverpanel')
              )
            )
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "sas_data",
        # shiny::conditionalPanel(condition = "input.sidebarmenu =='sas_data'",
          file_creation_ui("file_creation")
        # )
      ),
      shinydashboard::tabItem(
        tabName = "datamanual",
        list(
          HTML(
            "<h2>File Format and Structure </h2>
            <h4>File Format</h4>
            Currently, the following two file formats are supported:
            <ul>
            <li> A <b>c</b>omma <b>s</b>eparated <b>v</b>alues (CSV) file </li>
            <li> An RData file <br>
            </ul><br>

            <h4>File Structure</h4>

            In order to use the e<b>lab</b>orator, your laboratory data file has to include the following columns:<br>
            <ul>
            <li>  a subject identifier (called <kbd>SUBJIDN</kbd>) </li>
            <li>  the visit (called <kbd>AVISIT</kbd>) </li>
            <li>  the treatment group (called <kbd>TRTP</kbd>) </li>
            <li>  an (abbreviated) name of the laboratory parameter (called <kbd>LBTESTCD</kbd>) </li>
            <li>  the laboratory value measurement (called <kbd>LBORRES</kbd>) </li>
            <li>  the lower limit of normal (LLN) (called <kbd>LBORNRLO</kbd>) </li>
            <li>  the upper limit of normal (ULN) (called <kbd>LBORNRHI</kbd>) </li>
            </ul>

            <h5>Example</h5>
            The first 6 lines of an <i> examplary dataset </i> are shown in the following.<br>
            <ul>

            <samp>

            SUBJIDN &ensp;          AVISIT &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;           TRTP &ensp;&ensp; LBTESTCD LBORRES LBORNRLO LBORNRHI<br>
            100080021    Randomization &ensp;&ensp;&ensp;Placebo      HGB &ensp;&ensp;&ensp;&ensp;&ensp;    15.2 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>
            100080021    Visit 5 &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp; Placebo      HGB &ensp;&ensp;&ensp;&ensp;&ensp;    15.3 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>
            100080021 End of Treatment Placebo      HGB &ensp;&ensp;&ensp;&ensp;&ensp;    15.9 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>
            100080021        Follow-up &ensp;&ensp;&ensp;&ensp;&ensp;&ensp; Placebo      HGB &ensp;&ensp;&ensp;&ensp;&ensp;    16.2 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>
            100080053    Randomization &ensp;&ensp; 1 mg &ensp;&ensp;        HGB &ensp;&ensp;&ensp;&ensp;&ensp;    14.7 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>
            100080053          Visit 5 &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp; 1 mg &ensp;&ensp;         HGB &ensp;&ensp;&ensp;&ensp;&ensp;    13.9 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>

            </samp>
            </ul>
            <br>





            <h4>Important points to consider</h4>
            <ul>
            <li> Missing laboratory values must be coded as NA . We recommend carefully reading the section
            on <i>Handling Missing Data</i> in the &nbsp; <i class='fa fa-info'></i> "),
          shiny::actionLink("link_to_tab_info", "Information"),
          HTML(
            "-tab for correct interpretation. The section describes in detail how the e<b>lab</b>orator deals with missing data. </li>
            <li> If a laboratory parameter has no lower  or upper limit of normal, please do not insert any character in the respective cell but leave the cell empty or use the NA coding. Please do not use blank/space. </li>
            <li> Variable names must be spelled correctly as shown above (please use upper case letters). </li>
            <li> Do not use special characters for variable names or laboratory parameter names. </li>
            <li> All laboratory measurements have to be numeric. That means, do not use '+', '-', '>', '<', 'negative' etc. For example, '<1' is not a valid laboratory measurement. </li>
            <li> <b> Please always check your data carefully before uploading it to the e<b>lab</b>orator.  </b> You can also inspect the data loaded in the e<b>lab</b>orator app via the &nbsp; <i class='fa fa-file-lines'></i> <b> Raw Data</b>-tab. </li>
            </ul>

            "
          )
        )
      ),
      shinydashboard::tabItem(
        tabName = "helptext",
        list(
          HTML(
            "<h2>The Concept of the e<b>lab</b>orator for Clinical Trial Laboratory Data</h2>

            The e<b>lab</b>orator app provides a <i>complete overview</i> of laboratory results for each laboratory parameter
            and treatment group in a matrix-like structure. All the results related to a specific laboratory parameter are shown in one column.
            Results for a treatment group are presented within a row.
            By providing this overview, you will be able to
            identify <i>differences between treatment groups, similarities in laboratory parameters</i> and <i>frequent patterns</i>.<br> <br>

            By using various types of analyses you will be able to view  your laboratory data from different perspectives.
            The following three different types of analyses are available:
            <ul> <li> Quantitative trends analysis </li>
            <li> Qualitative trends analysis </li>
            <li> Reference-value based pattern analysis </li></ul>


            You can find a concept description of each type of analysis in the following.
            Available graphic and filter options as well as missing data handling are described below. <br>

            <br>
            <h4><i class='fa fa-chart-line'></i><b><i> &nbsp;Quantitative Trends</i></b></h4>
            Aim: Examine changes in laboratory values across study visits and explore whether changes differ between treatment groups. <br><br>

            This type of analysis depicts the distribution of laboratory parameters in each study visit.
            An example is shown in Figure 1. Figure 1 shows the distribution of platelets (in giga/l) in the 2 mg dose group at
            all four visits during a study ('Randomization', 'Treatment 1', 'End of Treatment' and 'Follow-up 3').
            Distributions are shown using boxplots. The middle 50% of patient-specific values fall inside the box.
            The median value is represented by the horizontal line crossing through the box and might be used as an indicator for the central tendency.
            The whiskers indicate the variability in the values. The upper whisker is derived as the smaller of the maximum observed laboratory value
            and the third quartile (i.e. upper limit of the box) + 1.5 x interquartile range.
            The upper whisker is derived as the larger of the minimum observed laboratory value and
            the first quartile (i.e. lower limit of the box) - 1.5 x interquartile range. Values outside of the box and whiskers (outliers) are indicated as filled points. <br>
            Changes over time can be easily detected by a shift in the boxplots along the y-axis.
            In this example, a  decrease in platelets is observed until the End of Treatment-Visit followed by a subsequent increase between the End of Treatment-Visit and the Follow-Up 3-Visit. <br><br>

            <img src='www/Fig1.png' alt='Graphic cannot be displayed' width='300' height='300'>
            <p> <i><b>Figure 1</b>: Example plot for quantitative trends analysis.
            The distribution of platelets (in giga/l) is shown for the 2 mg dose group at
            four study visits 'Randomization', 'Treatment 1', 'End of Treatment' and 'Follow-up 3'.
            Normal range, i.e. upper limit of normal and lower limit of normal, are indicated by dotted
            horizontal lines. </i></p>
            Click the 'Open/Close Zoom Panel'-button and use the mouse to hover over a specific plot
            (if option 'hover' is selected within the zoom panel) or, alternatively, click on a selected plot
            (if option 'click' is selected) to see an enlarged version. Further options are described below.


            <ul>
            <li> <h6><b>Same scales within lab parameter</b></h6>
            You can select whether the y-axis range is the same as a specific laboratory parameter (default) or
            it has to be on the data in the respective treatment group.
            Using the same range, simplyfies the comparison between the treatment groups.
            Using this option, extreme outliers will not appear (due to the cut-off scale),
            but they are indicated by arrows.
            The values next to the arrow indicate the values of the outliers.
            When 'same scale among treatments' is not ticked, outliers are still shown when present.</li>

            <li> <h6><b>Patient-specific values</b></h6>
            You can permit or plot patient-specific values. When permitted (default),
            patient-specific values will be added as dots to the boxplots.
            Note that outliers are indicated through dots as well and 'belong' to the boxplot
            (i.e. you can not suppress showing outliers).
            Moreover, you can choose whether patient-specific values are sorted
            from smallest to largest (default). When 'draw connection lines' is ticked,
            the patients' measurements at
            study visits are connected. This option can be useful in particular for small patient numbers.
            A blue connection line indicates a decrease, and an orange
            connection line indicates an increase in the values.</li>

            <li> <h6><b>Test for explorative trend detection</b></h6>
            You can search for changes between study visits by applying hypothesis testing.
            Note that a comparison of test results between the treatment groups is only recommended
            for balanced treatment groups, i.e. if treatment groups are of the same size.
            When treatment groups have different sizes, comparisons between treatment groups should
            not be made because of a difference in the statistical power. The figure background is
            colored if the p-value of the respective test falls below a specified local significance
            level (called 'p-value cutoff').
            The background is green for decreases, and yellow for increases (see e.g. Figure 1).  <br>
            The user can choose between two types of tests: the sign test and the t-test.
            The sign test is recommended for general use as it does not rely on distributional
            assumptions. It checks, for a specific laboratory parameter and treatment group,
            whether there are more patients with an increase than patients with a decrease
            between the two visits, or vice versa. Patients with consistent values
            (i.e. without any change in the values) are eliminated when applying the sign test.
            The t-test is recommended for expert users only because test assumptions should apply. <br>
            The user is required to select at least two visits used for detecting any changes.
            If more than two visits are selected the first visit selected is tested against each of
            the remaining visits (pairwise tests). <br>
            No adjustments for multiple testing are performed (tests for several treatment groups,
            several laboratory parameters and eventually several visits). Be aware that the multiple
            testing problem might lead to many falsely detected changes.
            <b>The use of this feature is specifically for exploration,
            where significant test results must be interpreted with caution.</b></li>
            </ul><br>

            <h4><i class='fab fa-buromobelexperte'></i><b><i>&nbsp; Qualitative Trends</i></b></h4>
            Aim: Study frequent time courses and check if they differ between treatment groups. <br><br>

            This type of analysis assesses frequent time courses that are described through
            increases/decreases between two subsequent study visits.
            A patient might, for example, have the following measurements for a specific laboratory parameter:
            Value 3.2 at Randomization Visit; 1.6 at Treatment 1-Visit; 2.9 at the End of Treatment-Visit;
            2.9 at the Follow-Up 3-visit.
            The time course for this patient will be characterized as decrease (from 3.2 to 1.6) -
            increase (from 1.6 to 2.9) - stable (from 2.9 to 2.9). This pattern is represented as '- + ='.<br>
            In this way, the patterns / time courses for each patient can be derived and the frequency of
            each pattern / time course can be counted. The time courses and frequencies are transferred to a
            diagram-like structure. Each cell of this diagram represents one specific pattern / time course.
            The time courses are arranged in a symmetric way within the diagram.
            For example, the time course '+ + +' is represented in the cell in the top, while the 'opposite'
            time course '- - -' is in the cell at the bottom of the diagram.
            There are three entries within each of the cells:
            the first and second entries show the absolute and relative number of subjects in the
            treatment group which have the specific time course, and the third entry shows the
            respective time course. You can use the font size slider to display the entries and
            increase the size of the numbers. By default, the font size is set to 0, that is,
            all entries are blocked.
            When a time course does not occur at all (i.e. the frequency and percentage are 0),
            the entries of the cell are not shown by default.
            <br>
            The frequency of a time course is shown by the color of the cell.
            Darker colors reflect more frequent and lighter colors less frequent time courses.
            The color key is provided on the right side of Figure 2. It can also be suppressed by
            clicking on the 'Open/close'-button above the color legend.
            No more than approx. 5 visits are recommended because diagrams will get too complex
            with increasing number of cells.<br>

            <img src='www/Fig2.png' alt='Graphic cannot be displayed' width='500' height='350'>
            <p><i><b>Figure 2</b>: Example plot for qualitative trends analysis (left) and color key (right).
            Frequent patterns of increases/decreases in platelets between four subsequent study visits within
            the 2mg dose group are shown.
            The background of the cell is colored depending on the frequency of the respective pattern
            (cf. color key).</i></p>

            Use the 'Open/Close Zoom Panel'-button to inspect a specific plot and see details.
            Further options are described below.
            <ul>
            <li><h6><b>Font size</b></h6>
            Use the slider to increase (larger value) or decrease (smaller value).
            If the font size slider is set to 0 (default), no numbers or patterns are printed inside the cells.
            The background colors are more visible when numbers are not printed.
            </li>
            <li><h6><b>Method for defining stability</b></h6>
            Often, laboratory parameters are measured on a continuous scale and measurements have several decimals.
            In this case, it might make sense not to consider very slight changes in laboratory values from one visit to
            another as increases or decreases. Instead laboratory values might be considered equal/stable even
            though they differ slightly.
            This 'tolerated difference' can be controlled by the user.
            By default, stability is defined only when two values are exactly equal, that is, the tolerated
            difference is set at 0.
            There are three options available for determining the tolerated difference:
            <ul>
            <li> Select the option 'IQR' (for interquartile range derived based on patient data at first visit)
            to determine the tolerated difference for each laboratory parameter as a (user-specified) percentage of the IQR. </li>
            <li> Select the option 'range' (i.e., maximum value minus minimum value observed based on patients data at first visit) to determine the tolerated
            difference for each laboratory parameter as a (user-specified) percentage of the range. Note that the range is sensitive to extreme values observed
            in the data (outliers). </li>
            <li> Select the option 'reference range' (i.e., upper limit of normal minus lower limit of normal) to determine the
            tolerated difference for each laboratory parameter as a (user-specified) percentage of the reference range.
            Note that the tolerated difference cannot be calculated for laboratory parameters which do not have a
            reference range defined by both the upper and the lower limit of normal. </li>
            </ul>
            The tolerated differences, derived based on the method you have chosen and the percentage, will be printed
            next to the diagram for each laboratory parameter.
            </li>
            <li><h6><b>Percentage</b></h6>
            Use the slider to specify the percentage of IQR, range or reference range to determine the tolerated
            difference (see also 'Method for defining stability'). If set to 0 (default) the tolerated difference
            is 0, that is, stability is defined only if two values are completely equal. <br>
            The exact value ('tolerated difference') is printed next to the diagram for each laboratory parameter.
            </li>
            <li><h6><b>Color scale</b></h6>
            Use the drop-down menu to select your favorite color scale.
            This color scale is used to color the cell backgrounds.
            The darker the background color, the more frequent the pattern.
            </li>
            </ul>

            <br>

            <h4><i class='fab fa-cloudsmith'></i><b><i>&nbsp; Reference-value Based Patterns</i></b></h4>
            Aim: Assess how many patients have laboratory values outside the normal range during the study
            and whether there is a difference between treatment groups. <br><br>

            The tree diagram consists of a starting point (i.e. the root of the tree) and several layers.
            The first layer represents the first visit, the second layer the second visit, and so on.
            An example for a specific laboratory parameter in the placebo group is shown in Figure 3.
            You are able to track patients during the trial, and identify at which visits abnormal
            laboratory values occur. From the starting point the sample is split up into two groups:
            one group with patients who have laboratory values outside the normal range at the first visit
            (lower path / orange circle) and the other group of patients with laboratory values inside the
            normal range at the first visit (upper path / green circle). Each of the groups is then split
            up based on the laboratory values at the second visit, and so on.
            Note that patients may have different normal ranges for the same laboratory parameter. <br>
            The size of the circles is proportional to the number of patients.
            This enables users to identify frequent patterns (e.g. normal - abnormal - abnormal - normal)
            among visits.
            The total number of patients is depicted inside the circle at the starting point. <br><br>

            No more than approx. 5 visits are recommended because tree structures will get too complex
            with an increasing number of layers.<br>
            Laboratory parameters without reference range(s) are not analysed.
            Thus, for the reference-value based pattern analysis the total number of plots shown might be
            smaller than for the other two analyses types.<br>

            <img src='www/Fig3.png' alt='Graphic cannot be displayed' width='350' height='400'>
            <p> <i><b>Figure 3</b>: Example plot for reference-value based pattern analysis.
            The number of patients with hematocrit (HCT) values within the reference range(green)
            or outside the reference range(orange) at four visits, 'Randomization', 'Treatment 1',
            'End of Treatment' and 'Follow-up 3', for the placebo group are shown. </i></p>
            Use the 'Open/Close Zoom Panel'-button to check a specific plot and see details.
            Further options are described below.

            <ul>
            <li><h6><b>Font size</b></h6>
            Use the slider to increase (larger value) or decrease (smaller value) the font size of the numbers inside the circles.
            When the font size is set to 0 (default), the numbers inside the circles are not shown.
            </li>
            <li><h6><b>Definition of abnormal values</b></h6>
            Choose the definition of abnormal values. The following three options are available:
            <ul>
            <li> Select the option 'above ULN or below LLN' if laboratory values are considered abnormal if they either exceed the upper limit of normal (ULN) or if they fall below the lower limit of normal (LLN). </li>
            <li> Select the option 'above ULN' if laboratory values are considered abnormal only if they exceed the upper limit of normal (ULN). </li>
            <li> Select the option 'below LLN' if laboratory values are considered abnormal only if they fall below the lower limit of normal (LLN). </li>
            </ul>
            </li>

            <li><h6><b> Factor multiplied with ULN or LLN</b></h6>
            Define abnormal values in terms of ULN or LLN multiplied with a positive value.
            For example, if entering the value 1.5 abnormal values will be defined as values above 1.5xULN and/or
            below 1.5xLLN depending on the selection within the option 'Definition of abnormal values'.
            </ul><br>

            <h4><i class='fa fa-file-upload'></i><b>&nbsp; Data Upload</b></h4>
            The data structure and format required for upload is outlined in the &nbsp; <i class='fa fa-file'></i>"
          ),
          shiny::actionLink("link_to_structure_info", "Data Manual"),
          HTML(
            "-tab. You can inspect the data uploaded to the e<b>lab</b>orator via the &nbsp; <i class='fa fa-file-lines'></i> <b> Raw Data</b>-tab.
            Searching for specific data points is possible within this tab using filters and sorting options in the header of the table. <br>

            Options for omitting laboratory parameters, treatment groups or visits are described below.
            Whenever changing options, please click on 'Create/Update graphs' to refresh the plots.  <br>

            <ul>
            <li>
            <h6><b>Visits (exclude and rearrange)</b></h6>
            Click the 'x' next to the visit to remove visits.
            Please note that this will remove the visit for all laboratory parameters.
            It is not possible to include the visit for some laboratory parameters but to exclude it for others
            (expect by manually setting the values to NA in your data file for the respective laboratory parameter).
            Drag and drop visits to change the order of the visits in the three types of analyses.</li>

            <li>
            <h6><b>Treatment groups (exclude and rearrange)</b></h6>
            Click the 'x' next to the treatment group to remove treatment groups.
            Drag and drop treatment groups to change the order of the treatment groups in the three types of analyses.</li>

            <li>
            <h6><b>Lab parameters</b></h6>
            Click the laboratory parameters in the drop-down menu to deselect laboratory parameters or re-select
            previously omitted laboratory parameters. You can also use the text field to search for specific
            laboratory parameters.<br>
            If you want to change the order of the laboratory parameters,
            please see the section below on 'Graphic Options'.</li>
            </ul><br>

            <h4><i class='fa fa-filter'></i> <b> Filters </b></h4>

            If you want to see only results for a specific subgroup of patients, you can use the filter-tab.
            Note that filtering based on treatment groups, visits or laboratory parameters can also be performed
            within the &nbsp; <i class='fa fa-file-upload'></i> <b> Data Upload</b>-tab.<br><br>

            As a first step, you can choose the variable(s) which you would like to filter on.
            You can apply multiple filters by selecting multiple filter variables.
            When all filter variables are selected, click on '+Add'.
            A menu will appear, which enables the selection of categories (for categorical filter variables)
            or ranges (for continuous filter variables) for each of the selected filtering variables.<br>

            Click on 'Apply Filter Selection' once you have finalized your selection.
            The progress bar at the top shows the approximate percentage of completion.
            If it shows 100% completion, you can inspect the results based on the selected filter options
            by selecting the desired analysis tab and refresh the plots by clicking on 'Create/Update graphs'.
            <br><br>


            <h4><i class='fa fa-cogs'></i> <b>Graphic Options</b></h4>
            The following graphic options are available:
            <ul><li> <h6> <i class='fa fa-arrows-alt'></i><b>&nbsp; Panel/Plot Size</b></h6>
            Adjust the plot size and height by using the sliders and click the 'Create/Update graphs'-button
            to reload the plots. </li>

            <li><h6><i class='fa fa-sort-alpha-down'> </i><b>&nbsp; Arrange Lab Parameters</b></h6>
            Use one of three options to change the arrangement of laboratory parameters in the three types of analyses.
            The following options are available to arrange laboratory parameters:
            <ul>
            <li> Select the option 'as in input' (default) to arrange laboratory parameters according to your
            preference. You can implement your individual arrangement of laboratory parameters by modifying the
            arrangement in your input data file, so that your input data file reflects your preferred arrangement. </li>
            <li> Select the option 'AI' (for artificial intelligence) to use an intelligent data-driven ordering.
            This option searches for an arrangement which locates laboratory parameters with (either positively
            or negatively) correlated changes over time close to each other.
            Use the drop-down menu to select the visits which will be used for deriving the change.
            Several sorting algorithms can be selected by the user.
            The default method is hierarchical clustering combined with optimal leaf ordering.
            More information on the methodology can be found in this ")
          ,
          shiny::actionLink("link_to_pdf_view", "short manual"),
          "."
        ),
        shiny::uiOutput('pdfview'),
        list(
          HTML(
            "For methods that are based on hierarchical clustering, a dendrogram is also shown above the results window.
            When laboratory parameters are not included at a specific visit and if you have chosen this visit for
            defining change, the laboratory parameters cannot be used in the sorting algorithm.
            Therefore, the respective laboratory parameters will simply be relocated
            to the arrangement/list obtained by the algorithm, and thus will be relocated at the last position. </li>
            <li> Select the option 'alphabetically' to arrange laboratory parameters alphabetically. </li>
            </ul>
            After your selection click 'Update Order!' and the 'Create/Update Plots'-button to reload the plots with the
            changed arrangement. <br></li>

            <li><h6><i class='fa fa-palette'></i> <b>&nbsp; Colors</b></h6>
            Use the drop-down menu to select colors for each visit in the quantitative analysis.
            You may e.g. choose one color for visits at which patients were on treatment and another
            color for visits (e.g. randomization, follow-up) at which patients were off treatment.</li><br>
            </ul>
            <h4><b>Missing Value Handling</b></h4>
            The  analyses of a specific laboratory parameter require that the patients data must be complete (non-missing for all included visits).
            The following mechanisms are implemented:
            <ul>
            <li> The e<b>lab</b>orator-app automatically omits study visits for a laboratory parameter if more than 50% of patients
            have missing values for that laboratory parameter.
            You can also change this percentage using the &nbsp; <i class='fa fa-file-upload'></i> <b> Data Upload</b>-tab. </li>
            <li> Patients who have a missing laboratory value at any of the 'considered' visits (i.e., excluding visits with more
            than 50% missing values, see first item, and visits that are manually removed by the user) will be excluded from all
            analyses of the respective laboratory parameter.</li></li>
            </ul>
            There are many different 'patterns' of missing values that might lead to a substantially reduced sample size.
            The user can, however, decide to automatically exclude some visits in order to avoid a possible substantial reduction in the sample size.
            For example, if a specific laboratory parameter is missing at a specific visit for 40% of the subjects, then the analyses can
            only use the remaining 60% of subjects with non-missing values for that visit (assuming no missing values for the remaining
            subjects at any of the other visits).
            A single visit with many missing values can therefore reduce the number of evaluable patients drastically.
            If you want to avoid the exclusion of too many subjects due to a large percentage of missing values at a specific visit
            (and accept the omission of visits instead), you can set the percentage of 'tolerated' missing values
            (which is by default set to 50%) to a small value (down to 25%, which is the smallest possible value).<br> <br>

            You can check the number of patients per treatment group used for all analyses of a specific laboratory parameters
            by clicking on the 'Open/Close Zoom Panel'. In the lower part, detailed information about the numbers used for the analysis
            of a specific laboratory parameter is shown as well as missing values at each considered visit.
            Note that the number of subjects analyzed might differ between the laboratory parameters, because the laboratory parameters
            are analyzed independently of each other.<br><br>

            The following example illustrates which subjects and visits will be used in the analysis if missing data exist. <br>

            <h5> <b> Example </b></h5>
            The data of a study consists of three subjects and two laboratory parameters hematocrit (HCT) and hemoglobin (HGB). The user has not changed the percentage of 'tolerated' missing values, and therefore the default of 50% is used.
            The original data is summarized below. <br> <br>


            <style>
            table {
            font-family: arial, sans-serif;
            border-collapse: collapse;
            width: 100%;
            }

            td, th {
            border: 1px solid #dddddd;
            text-align: left;
            padding: 8px;
            }

            tr:nth-child(2) {
            background-color: #dff2fd;
            }

            tr:nth-child{
            background-color: #c9e1f6;
            }

            tr:first-child{
            background-color: #11c4d4;
            }
            </style>

            <table>

            <tr>
            <th colspan='1'> </th>
            <th colspan='3'>HCT</th>
            <th colspan='3'>HGB</th>
            </tr>
            <tr>
            <th>Subject</th>
            <th>Visit 1</th>
            <th>Visit 2</th>
            <th>Visit 3</th>
            <th>Visit 1</th>
            <th>Visit 2</th>
            <th>Visit 3</th>
            </tr>
            <tr>
            <th> 1 </th>
            <th> 42.8 </th>
            <th> <font color='#f78300'> NA </font>  </th>
            <th> <font color='#f78300'> NA </font>  </th>
            <th>  13.8</th>
            <th>  13.8</th>
            <th> 14.1 </th>
            </tr>
            <tr>
            <th>2</th>
            <th> 41.2 </th>
            <th>  <font color='#f78300'> NA </font> </th>
            <th>  42.2</th>
            <th>  16.2</th>
            <th>  15.8</th>
            <th>  16.4</th>
            </tr>
            <tr>
            <th>3</th>
            <th> <font color='#f78300'> NA </font> </th>
            <th> 40.9 </th>
            <th>  40.7</th>
            <th>  <font color='#f78300'> NA </font> </th>
            <th>  14.3</th>
            <th>  13.3</th>
            </tr>
            </table>
            <br>
            <i> Which visits will be omitted for each of the two laboratory parameters? </i>
            <ul>
            <li>Visit 2 will be automatically omitted for HCT since more than 50% of the values are missing.
            Visits 1 and 3 remain for HCT and will be used in the analyses. </li>
            <li> No visit will be omitted for HGB. At maximum 1/3 of the values are missing, thus all three visits will be saved for HGB.
            Visits which are not automatically deleted will be referred to as 'considered' visits in the following.
            </li></ul>
            <i> Which subjects will be omitted from the analyses for each of the two laboratory parameters? </i>
            <ul>
            <li> Subject 1 will not be used for the analysis of HCT because this subject has a missing value at the considered visit 3.
            In contrast, subject 1 has no missing value for any of the considered visits 1, 2 and 3, and is therefore included in the analysis of HGB.
            </li>
            <li> Subject 2 is included in both the analyses of HCT and HGB because it has no missing values for any of the considered visits (note that visit 2 has been omitted for HCT).
            </li>
            <li> Subject 3 is excluded for both HCT and HGB because it has a missing value at any of the considered visit.
            </li></ul>

            "
          )
        )
      ),
      shinydashboard::tabItem(tabName = "qual",
        shiny::fluidPage(
          shiny::conditionalPanel(condition = "output.flag == false",
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
                  tags$span(style = "font-size:150%",
                    "Upload your",
                    tags$span(style = "color:#f78300", "laboratory data"),
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
                  tags$span(style = "font-size:150%","Click the 'Data Manual'-tab for the required format and structure for laboratory data file.")
                )
              )
            ),
            tags$div(
              HTML(
                paste(
                  "<i class='fa fa-info'></i>&emsp;",
                  tags$span(
                    style = "font-size:150%"," If you want to access information on the elaborator, click the 'Information'-tab.", sep = ""
                  )
                )
              )
            )
          ),
          shiny::conditionalPanel(condition = "output.flag == true",
            shinydashboard::box(
              width = NULL,
              title = span(shiny::tagList(' ',icon("cogs"))),
              background = 'black',
              solidHeader = TRUE,
              collapsible = TRUE,
              shiny::fluidRow(
                shiny::column(2,
                  bsplus::bs_embed_tooltip(
                    tag = h5(span(shiny::tagList("Font size", icon("question")))),
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
                shiny::column(2,
                  bsplus::bs_embed_tooltip(
                    tag = h5(span(shiny::tagList("Choose method for defining stability", icon("question")))),
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
                shiny::column(2,
                  bsplus::bs_embed_tooltip(tag = h5(span(shiny::tagList("Select percentage", icon("question")))),
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
                shiny::column(2,
                  bsplus::bs_embed_tooltip(tag = h5(span(shiny::tagList("Select a color scale", icon("question")))),
                    title = "Select your favorite color scale used for highlighting frequent patterns.", placement = "top", expanded = TRUE
                  ),
                  shinyWidgets::pickerInput(
                    inputId = 'select.pal1',
                    label = "",
                    choices = names(colChoice),
                    selected = names(colChoice)[1],
                    multiple = FALSE,
                    options = list(
                      `live-search` = TRUE,
                      `style`='background: btn-warning',
                      `header`='Select item'
                    )
                  ),
                  shiny::plotOutput('prev.pal1', height = '20px')
                ),
                shiny::column(width = 2, offset = 4,
                  shiny::helpText(
                    HTML('<p style="color:white"> You can minimize/maximize this window with the -/+ button on the top right of the panel </p>')
                  )
                )
              )
            ),
            shiny::conditionalPanel(condition = "output.ai == true",
              shinydashboard::box(
                width = NULL,
                title = span(shiny::tagList('', icon("sort-alpha-down"),'Dendrogram - (Click on the + symbol to open)')),
                solidHeader = TRUE,
                background = 'black',
                collapsible = TRUE,
                collapsed = TRUE,
                shiny::fluidRow(
                  shiny::column(12,
                    shiny::plotOutput(
                      'dendro_2',
                      height = "250px"
                    )
                  )
                )
              )
            ),
            shiny::conditionalPanel(condition = "output.flag == true",
              shiny::fluidRow(
                shiny::column(2,
                  shiny::actionButton(
                    inputId = "apply_qual_plot",
                    label = paste0('Create Plots'),
                    icon = icon("object-group")
                  ),
                  shiny::uiOutput('cont2')
                ),
                shiny::column(5, offset = 2,
                  shiny::uiOutput('cont2_text')
                )
              )
            ),
            shiny::uiOutput('tab2', width = 'auto'),
            shiny::uiOutput('legendpanel'),
            shiny::uiOutput('hoverpanel2')
          )
        )
      ),
      shinydashboard::tabItem(tabName = "filter",
        shiny::fluidPage(
          shiny::conditionalPanel(condition = "output.flag == false",
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
                    style = "font-size:150%",
                    "Upload your",
                    tags$span(style = "color:#f78300", "laboratory data"),
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
                  tags$span(style = "font-size:150%","Click the 'Data Manual'-tab for the required format and structure for laboratory data file.")
                )
              )
            ),
            tags$div(
              HTML(
                paste("<i class='fa fa-info'></i>&emsp;",
                  tags$span(style = "font-size:150%"," If you want to access information on the elaborator, click the 'Information'-tab.", sep = "")
                )
              )
            )
          ),
          shiny::conditionalPanel(condition = "output.flag == true",
            shiny::uiOutput("filter_percentage"),
            shiny::uiOutput("pickerinput_filter"),
            shiny::fluidRow(
              shiny::column(4,
                tags$head(tags$style(HTML('#insertBtn{background-color:#47d2bc;border-color: #000000;}'))),
                shiny::actionButton(
                  inputId = "insertBtn",
                  label = "Add",
                  icon = icon("plus")
                )
              ),
              shiny::column(4,
                tags$head(tags$style(HTML('#removeBtn{background-color:#ffeeaa;border-color: #000000;}'))),
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
                style = "color: #fff; background-color: #f78300; border-color: #fff"
            )
          )
        )
      ),
      shinydashboard::tabItem(tabName = "raw_data",
        shiny::fluidPage(
          shiny::conditionalPanel(condition = "output.flag == false",
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
                  tags$span(style = "font-size:150%",
                    "Upload your",
                    tags$span(style = "color:#f78300", "laboratory data"),
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
                  tags$span(style = "font-size:150%","Click the 'Data Manual'-tab for the required format and structure for laboratory data file.")
                )
              )
            ),
            tags$div(
              HTML(
                paste(
                  "<i class='fa fa-info'></i>&emsp;",
                  tags$span(style = "font-size:150%"," If you want to access information on the elaborator, click the 'Information'-tab.", sep = "")
                )
              )
            )
          ),
          shiny::conditionalPanel(condition = "output.flag == true",
            DT::DTOutput(
              'raw_data_table'
            )#,
            #shiny::uiOutput('raw_data_plot_panel', width = 'auto'),
            #plotOutput('raw_data_plot')
          )
        )
      ),
      shinydashboard::tabItem(tabName = "rvbp",
        shiny::fluidPage(
          shiny::conditionalPanel(condition = "output.flag == false",
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
                  tags$span(style = "font-size:150%",
                    "Upload your",
                    tags$span(style = "color:#f78300", "laboratory data"),
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
                  tags$span(style = "font-size:150%","Click the 'Data Manual'-tab for the required format and structure for laboratory data file.")
                )
              )
            ),
            tags$div(
              HTML(
                paste(
                  "<i class='fa fa-info'></i>&emsp;",
                  tags$span(style = "font-size:150%"," If you want to access information on the elaborator, click the 'Information'-tab.", sep = "")
                )
              )
            )
          ),
          shiny::conditionalPanel(condition = "output.flag == true",
            shinydashboard::box(
              width = NULL,
              title = span(shiny::tagList('', icon("cogs"))),
              background = 'black',
              solidHeader = TRUE,
              collapsible = TRUE,
              shiny::column(2,
                bsplus::use_bs_popover(),
                bsplus::use_bs_tooltip(),
                bsplus::bs_embed_tooltip(
                  tag = h5(span(shiny::tagList("Font size", icon("question")))),
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
              shiny::column(2,
                bsplus::use_bs_popover(),
                bsplus::use_bs_tooltip(),
                bsplus::bs_embed_tooltip(
                  tag = h5(span(shiny::tagList("Definition of abnormal values", icon("question")))),
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
              shiny::column(2,
                bsplus::use_bs_popover(),
                bsplus::use_bs_tooltip(),
                bsplus::bs_embed_tooltip(
                  tag = h5(span(shiny::tagList("Factor multiplied with ULN or LLN", icon("question")))),
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
              shiny::column(width = 2, offset = 4,
                shiny::helpText(HTML('<p style="color:white"> You can minimize/maximize this window with the -/+ button on the top right of the panel </p>'))
              )
            ),
            shiny::conditionalPanel(
              condition = "output.ai == true",
              shinydashboard::box(
                width = NULL,
                title = span(shiny::tagList('',icon("sort-alpha-down"),'Dendrogram - (Click on the + symbol to open)')),
                solidHeader = TRUE,
                background = 'black',
                collapsible = TRUE,
                collapsed = TRUE,
                shiny::fluidRow(
                  shiny::column(12,
                    shiny::plotOutput(
                      outputId = 'dendro_3',
                      height = "250px"
                    )
                  )
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(12,
                shiny::conditionalPanel(condition = "input.abnormal_values_factor >= 0 && input.abnormal_values_factor != undefined && output.flag == true",
                  shiny::fluidRow(
                    shiny::column(2,
                      shiny::actionButton(
                        inputId = "apply_ref_plot",
                        label = paste0('Create Plots'),
                        icon = icon("object-group")
                      ),
                      shiny::uiOutput('cont3')
                    ),
                    shiny::column(5, offset = 2,
                      shiny::uiOutput('cont3_text')
                    )
                  )
                ),
                shiny::conditionalPanel(condition = "input.abnormal_values_factor < 0 || input.abnormal_values_factor == undefined",
                  HTML('<p style="color: #f78300"> Please enter a non-negativ numeric percentage value.')
                ),
                shiny::uiOutput('tab3', width = 'auto'),
                shiny::uiOutput('hoverpanel3')
              )
            )
          )
        )
      )
    ),
    tags$script(
      HTML("$('body').addClass('sidebar-mini');")
    ),
    tags$head(
      tags$style(
        HTML(
          " h1 {font-family: 'Arial';line-height: 1.1;color: #fffff;}"
        )
      )
    ),
    tags$script(
      HTML(
        '$(document).ready(function() {
        $("header").find("nav").append(\' <h4 style="color:white"> A New Perspective on Laboratory Data </h4>\');
        })'
      )
    ),
    tags$style(
      type = 'text/css',
      ".selectize-dropdown-content {max-height: 50px;}"
    ),
    tags$style(
      type = 'text/css', ".selectize-input { background-color: #F8F8F8;}"
    )
  )
)
}
