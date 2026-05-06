#' upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::prettyRadioButtons(
      inputId = ns("impswitch"),
      label = 'Select file format',
      status = "warning",
      shape = 'round',
      animation = 'smooth',
      choices = c('*.RData file', '*.CSV file')
    ),
    shiny::htmlOutput(ns("err_message")),
    shiny::uiOutput(ns("impdata")),
    shiny::conditionalPanel(
      condition = "output.flag == true",
      shiny::selectizeInput(
        inputId = ns("select.visit"),
        label = 'Visits (exclude and rearrange)',
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list('plugins' = list('remove_button', 'drag_drop'))
      ),
      shiny::selectizeInput(
        inputId = ns("select.treatments"),
        label = 'Treatment groups (exclude and rearrange)',
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(
          'plugins' = list('remove_button', 'drag_drop')
        )
      ),
      shinyWidgets::pickerInput(
        inputId = ns("select.lab"),
        label = 'Lab parameters',
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = 'count > 0',
          `count-selected-text` = '{0} selected (of {1})',
          `live-search` = TRUE,
          `header` = 'Select multiple items',
          `none-selected-text` = 'All dropped!'
        )
      ),
      bsplus::bs_embed_tooltip(
        tag = h5(span(shiny::tagList(
          "Tolerated missings percentage:",
          icon("question")
        ))),
        title = "Select percentage of missing values per
            visit allowed to be still included in the analysis
            (100% means no visits will be removed).",
        placement = "top",
        expanded = TRUE
      ),
      shiny::sliderInput(
        inputId = ns("select.toleratedPercentage"),
        label = '',
        min = 25,
        max = 100,
        value = 50,
        step = 5,
        post = "%"
      )
    )
  )
}

#' upload Server Functions
#'
#' @noRd
mod_upload_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #### Import data ####
    output$impdata <- shiny::renderUI({
      shiny::req(input$impswitch)
      if (input$impswitch == '*.RData file') {
        shiny::fileInput(
          inputId = ns("file"),
          label = 'Choose RData file',
          multiple = FALSE,
          accept = '.RData'
        )
      } else if (input$impswitch == '*.CSV file') {
        shiny::tagList(
          shiny::fixedRow(
            shiny::fileInput(
              inputId = ns("csv_file"),
              label = 'Choose CSV file',
              multiple = TRUE,
              accept = c(
                'text/csv',
                'text/comma-separated-values,text/plain',
                '.csv'
              )
            ),
            shinyWidgets::prettyRadioButtons(
              inputId = ns("sep"),
              label = 'Select separator',
              inline = TRUE,
              choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = '\t'),
              status = "warning",
              animation = "smooth",
              selected = ','
            ),
            shinyWidgets::prettyRadioButtons(
              inputId = ns("quote"),
              label = 'Select quote',
              inline = TRUE,
              choices = c(
                None = '',
                'Double Quote (")' = '"',
                "Single Quote (')" = "'"
              ),
              selected = '"',
              status = "warning",
              animation = "smooth"
            ),
            shinyWidgets::prettyRadioButtons(
              inputId = ns("dec"),
              label = 'Select decimal character',
              status = "warning",
              animation = "smooth",
              inline = TRUE,
              choices = c(
                'Point (.)' = '.',
                'Comma (,)' = ','
              ),
              selected = '.'
            )
          )
        )
      }
    })

    shiny::observeEvent(r$app_input(), {
      if (!is.null(r$app_input())) {
        shinyWidgets::updatePrettyRadioButtons(
          session,
          inputId = "impswitch",
          choices = c('Loaded Data', '*.RData file', '*.CSV file')
        )
      }
    })

    #### data pre-processing for graphs ####
    # 1. load data and check for requirements
    #    reactive:  raw_data_and_warnings()
    # 2. filter by filter-tab
    #    reactive:  filtered_raw_data()
    # 3. filter by upload selection
    #    reactive:  filtered_and_reduced_raw_data()
    # 4. create 'remove'-flags due to tolerated missing percentage
    #    reactive:  data_with_missing_flag()
    # 5. remove visits due to tolerated missing
    #    reactive:  data_without_missing_visits()
    # 6. create/change correct variable classes
    #    reactive:  data_filtered_by_app_selection()
    # 7. re-factor lab parameter value
    #    reactive:  data_with_selected_factor_levels()
    # 8. reduce data to patients with all lab parameters non missing
    #    reactive:  data_with_only_non_missings_over_visits()

    #### 1.load data and perform checks: (raw_data_and_warnings()) ####
    # used function(s):
    # elaborator_load_and_check() & elaborator_fill_with_missings()
    # purpose:
    # to first load the data and check for required variables
    # and than merge an empty data set with every potential subjectid, visit
    # and lab parameter to ensure the calculations for empty visits are correct.
    # reactivity triggers :
    # input$impswitch / input$file$datapath / input$csv_file$datapath / app_input()
    # input$sep / input$quote / input$dec

    raw_data_and_warnings <- shiny::reactive({
      input$impswitch
      fpath <- if (!is.null(input$file)) input$file$datapath else NULL
      csvp <- if (!is.null(input$csv_file)) input$csv_file$datapath else NULL
      tmp <- elaborator_load_and_check(
        data_switch = input$impswitch,
        rdata_file_path = fpath,
        csv_file_path = csvp,
        loaded_file = r$app_input(),
        separator = input$sep,
        quote = input$quote,
        decimal = input$dec
      )
      if (!is.null(tmp$data)) {
        tmp$data <- elaborator_fill_with_missings(
          elab_data = tmp$data
        )
      }

      #function to expand data by subject id, visits and lab parameter to
      #avoid wrong calculations by tolerated missing function
      elaborator_expand_grid <- function(dat) {
        tmp <- expand.grid(
          unique(dat$SUBJIDN),
          unique(dat$AVISIT),
          unique(dat$LBTESTCD)
        )
        treatment <- dat %>%
          dplyr::select(SUBJIDN, TRTP) %>%
          distinct()
        colnames(tmp) <- c("SUBJIDN", "AVISIT", "LBTESTCD")
        dat2 <- dat %>%
          dplyr::right_join(tmp, by = c("SUBJIDN", "AVISIT", "LBTESTCD")) %>%
          dplyr::select(-TRTP)
        dat3 <- dat2 %>%
          dplyr::right_join(treatment, by = c("SUBJIDN"))
        return(dat3)
      }
      if (!is.null(tmp$data)) {
        #expand data with missing lab values
        tmp$data <- elaborator_expand_grid(dat = tmp$data)
      }

      list(
        data = tmp$data,
        message = tmp$message
      )
    })

    #Datatable output with raw data
    output$raw_data_table <- DT::renderDataTable(
      DT::datatable(
        shiny::req(raw_data_and_warnings()$data),
        extensions = "Buttons",
        options = list(
          dom = "Brtip",
          buttons = c("copy", "print", "pageLength", I("colvis"))
        ),
        caption = "Raw data:",
        filter = list(
          position = 'top',
          clear = FALSE
        )
      )
    )

    # Output Loading error message if available
    output$err_message <- renderText({
      if (!is.null(raw_data_and_warnings()$message)) {
        str1 <- raw_data_and_warnings()$message
        paste(str1)
      }
    })

    shiny::observe({
      rv <- raw_data_and_warnings()
      r$raw_data_and_warnings <- rv
      if (!is.null(rv$data)) {
        r$start$dat <- TRUE
      }
    })

    shiny::observe({
      if (elaborator_demo_rdata_available()) {
        shinyWidgets::updatePrettyRadioButtons(
          session,
          inputId = "impswitch",
          label = "Select file format",
          choices = c("*.RData file", "*.CSV file", "Demo data"),
          prettyOptions = list(status = "warning")
        )
      }
    })
  })
}

## To be copied in the UI
# mod_upload_ui("upload_1")

## To be copied in the server
# mod_upload_server("upload_1")
