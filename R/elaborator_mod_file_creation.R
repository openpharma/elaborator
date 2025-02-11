#' File creation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
file_creation_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::uiOutput({
       ns('start_text')
    }),
    shiny::uiOutput(ns("update_button_panel")),
    shinyWidgets::prettyRadioButtons(
      inputId = ns('adlb_data'),
      label = 'Input type',
      shape = 'round',
      animation = 'smooth',
      choices = c(
        "SAS file (from Disc)" = "sas"
      )
    ),
    shiny::conditionalPanel(condition = paste0("input['", ns("adlb_data"), "\'] == \'sas\'"),
      shiny::fluidRow(
        shiny::column(4,
          shiny::uiOutput(ns("adlb_file")),
          shiny::uiOutput(ns("wrong_adlb_format_text"))
        ),
        shiny::column(4,
          shiny::fileInput(
            inputId =  ns("adsl_file"),
            label = "ADSL data (optional for treatment variable)",
            multiple = FALSE,
            accept = NULL,
            width = NULL
          ),
          shiny::uiOutput(ns("wrong_adsl_format_text"))
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(3,
        shiny::uiOutput(ns("sel_subjid"))#,
       # shiny::uiOutput(ns("sel_subjid_check"))
      ),
      shiny::column(3,
        shiny::uiOutput(ns("sel_treatment"))
      ),
      shiny::column(3,
        shiny::uiOutput(ns("sel_avisit"))
      )
    ),
    shiny::fluidRow(
      shiny::column(3,
         shiny::uiOutput(ns("sel_lab_name"))
      ),
      shiny::column(3,
        shiny::uiOutput(ns("sel_lab_value"))
      )
    ),
    shiny::fluidRow(
      shiny::column(3,
        shiny::uiOutput(ns("sel_lower_limit"))
      ),
      shiny::column(3,
        shiny::uiOutput(ns("sel_upper_limit"))
      )
    ),
    shiny::column(10,
      shinyBS::bsCollapse(
        shinyBS::bsCollapsePanel(
          shiny::HTML('<p style="color:black; font-size:100%;"> Filter: (click to open) </p>'),
          "Filter options",
          shiny::uiOutput(ns("filter_percentage_file")),
          shiny::uiOutput(ns("pickerinput_adlb")),
          shiny::fluidRow(
            shiny::column(4,
              shiny::actionButton(
                inputId = ns("insertBtn_file"),
                label = "Add",
                icon = icon("plus")
              )
            ),
            shiny::column(4,
              shiny::actionButton(
                inputId = ns("removeBtn_file"),
                label = "Delete",
                icon = icon("minus")
              )
            )
          ),
          shiny::tags$div(id = "placeholder_file"),
          shiny::conditionalPanel(condition = "output.condition_filter == true",
            shiny::actionButton(
              inputId = ns("apply"),
              label = "Apply Filter Selection!",
              icon = icon("redo"),
              style = "color: #fff; background-color: #66B512; border-color: #fff"
            )
          )
        )
      )
    ),
    shiny::column(10,
      shinyBS::bsCollapse(
        shinyBS::bsCollapsePanel(
          shiny::HTML('<p style="color:black; font-size:100%;"> Renaming: (click to open) </p>'),
          shiny::column(3,
            shiny::uiOutput(ns("treatment_renaming"))
          ),
          shiny::column(3,
            shiny::uiOutput(ns("visit_renaming"))
          ),
          shiny::column(3,
            shiny::uiOutput(ns("lab_name_renaming"))
          )
        )
      )
    ),
    shiny::column(10,
        shinyBS::bsCollapse(
          shinyBS::bsCollapsePanel(
            shiny::HTML('<p style="color:black; font-size:100%;"> adlb (+adsl) data: </p>'),
            shiny::wellPanel(
              id = "table_adlb_Panel",
              style = "color:black; overflow-y:scroll; max-height: 600px",
              shiny::dataTableOutput(ns('table_adlb'))
            )
          ),
          shinyBS::bsCollapsePanel(
            shiny::HTML('<p style="color:black; font-size:100%;"> Output file: </p>'),
            shiny::wellPanel(
              id = "table_csv_Panel",
              style = "color:black; overflow-y:scroll; max-height: 600px",
              shiny::dataTableOutput(ns('table_csv'))
            )
          )
        )
      )
    )
}

#' File creation Server Function
#'
#' @noRd
file_creation_server <- function(input, output, session) {
  #set moduke namespace
  ns <- session$ns

  #Set send-to-app button color:
  output$btn3_cont <- shiny::renderUI({
    list(
      shiny::tags$head(
        tags$style(HTML(paste0('#', session$ns("btn3"),'{color: #ffffff; background-color:#66B512;}')))
      )
    )
  })

  #Set Download button color:
  downloadData_cont <- shiny::renderUI({
    list(
      shiny::tags$head(
        tags$style(HTML(paste0('#', session$ns("downloadData"),'{color: #ffffff; background-color:#66B512;}')))
      )
    )
  })

  #Start text
  output$start_text <- shiny::renderUI({
    list(
      HTML(
        "
          <h1> Create an elaborator input file from your SAS files </h1>
          <h5> Please upload your adlb (or advs ...) file and complete all required settings.
               If necessary also upload the appropriate adsl data set.</h5>
          <h5> After completing all settings, either press the
              <span style='color: white; background-color: #66B512;height: 26px;border-radius:4px'><i class='fa-solid fa-file-import'></i>Send to app! </span>- or <span style='color: white; background-color: #66B512;height: 26px;border-radius:4px'><i class='fa-solid fa-download'></i>Save as </span>-button.
               The saved data can be uploaded via <b><i class='fa-solid fa-upload'></i> Data Upload </b>-tab.</h5> The results of the calculation appear in the 'Output file' box.</h5>
        "
      )
    )
  })

  output$update_button_panel <- shiny::renderUI({
    shiny::req(adlb_data())
    shiny::absolutePanel(
      id = "update_button_panel",
      class = "modal-content",
      fixed = TRUE,
      draggable = FALSE,
      HTML(paste0("<div style='background-color: white'>")),
      top = 200,
      left = "auto",
      right = 50,
      bottom = "auto",
      width = "auto",
      height = "auto",
        shiny::fluidRow(
          shiny::column(2,
             shiny::actionButton(
              inputId = ns("btn3"),
              label = "Send to app!",
              icon = icon("file-import")
            ),
            shiny::uiOutput(ns('btn3_cont'))
          )
        ),
        br(),
        shiny::fluidRow(
          shiny::column(2,
            shiny::downloadButton(ns("downloadData"), "Save as .csv")
          ),
          shiny::uiOutput(ns('downloadData_cont'))
        ),
      style = "z-index: 10;"
    )
  })

  #Reactive object to read ADLB data set
  adlb_data <- shiny::reactive({
    if (is.null(input$adlb_file)) {
      return(NULL)
    } else {
      #check if file format is .sas7bdat or .sas7cdat
      #extract the data path ending
      inFile <- input$adlb_file$datapath
      split_path <- strsplit(x = inFile, split = "[.]")
      path_ending <- split_path[[1]][length(split_path[[1]])]
      if (path_ending %in% c("sas7bdat", "sas7cdat")) {
        adlb <- haven::read_sas(input$adlb_file$datapath)
        if (!is.null(input$adsl_file)) {
          inFile2 <- input$adsl_file$datapath
          split_path2 <- strsplit(x = inFile2, split = "[.]")
          path_ending2 <- split_path2[[1]][length(split_path2[[1]])]
          if (path_ending2 %in% c("sas7bdat", "sas7cdat")) {
            adsl <- haven::read_sas(input$adsl_file$datapath)
            same_colnames <- intersect(colnames(adsl),colnames(adlb))
            #remove ADSNAME (dataset name) for merging
            if ("ADSNAME" %in% same_colnames) {
              same_colnames <- same_colnames[-which(same_colnames == "ADSNAME")]
            }
            adlb <- adlb %>%
              dplyr::left_join(adsl, by = same_colnames)
            output$wrong_adsl_format_text <- shiny::renderUI({
              HTML(paste0(""))
            })
            output$wrong_adsl_format_text <- shiny::renderUI({
              HTML(paste0(""))
            })
          } else {
            output$wrong_adsl_format_text <- shiny::renderUI({
              HTML(paste0("
              <b style = 'color:#E43157'>
                Wrong data format! Please upload SAS data in .sas7bdat or .sas7cdat format!
              </b>"))
            })
          }
        }
        adlb
      } else {
        output$wrong_adlb_format_text <- shiny::renderUI({
          HTML(paste0("
          <b style = 'color:#E43157'>
            Wrong data format! Please upload SAS data in .sas7bdat or .sas7cdat format!
          </b>"))
        })
        return(NULL)
      }
    }
  })

  #fileinput for adlb
  output$adlb_file <- shiny::renderUI({
     shiny::fileInput(
       inputId =  ns("adlb_file"),
       label = "ADLB/ADVS/... data (.sas7bdat format)",
       multiple = FALSE,
       accept = NULL,
       width = NULL
     )
  })


  #### Select treatment variable ####
  output$sel_treatment <- shiny::renderUI({
    if (is.null(adlb_data())) {
      return()
    } else {
      choices <- as.list(names(adlb_data()))
      choices <- c(choices[stringr::str_detect(choices, "TRT")], choices[!(stringr::str_detect(choices, "TRT"))])
      # possible treatment variable names (add more here if needed)
      trt_variable_names <- c("TRT01P", "TRT01PN", "TRT01A", "TRT01AN","TRTP", "TRTPN", "TRTA", "TRTAN")
      # determine the order of selection (here trt01p is selected as default if available)
      selected <- trt_variable_names[which(trt_variable_names %in% choices)[1]]
    }
    shinyWidgets::pickerInput(
      inputId = ns("sel_treatment"),
      label = "Select treatment variable",
      choices = choices,
      selected = selected,
      multiple = FALSE,
      options = shinyWidgets::pickerOptions(
        list(
          `actions-box` = TRUE,
          `selected-text-format` = 'count > 0',
          `count-selected-text` = '{0} selected (of {1})',
          `live-search` = TRUE,
          `header` = 'Select multiple items',
          `none-selected-text` = 'No selection!',
          `maxOptions` = 1
        )
      )
    )
  })

  #### Select verum variable ####
  output$sel_subjid <- shiny::renderUI({
     if (is.null(adlb_data())) {
      return()
    } else {
      choices <- as.list(names(adlb_data()))
      choices <- c(choices[stringr::str_detect(choices, "SUB")], choices[!(stringr::str_detect(choices, "SUB"))])
      # possible treatment variable names (add more here if needed)
      subjid_variable_names <- c("SUBJID","SUBJIDN","USUBJID","USUBJIDN")
      # determine the order of selection (here trt01p is selected as default if available)
      selected <- subjid_variable_names[which(subjid_variable_names %in% choices)[1]]
    }
    shinyWidgets::pickerInput(
      inputId = ns("sel_subjid"),
      label = "Select subject identifier variable",
      choices = choices,
      selected = selected,
      multiple = FALSE,
      options = shinyWidgets::pickerOptions(
        list(
          `actions-box` = TRUE,
          `selected-text-format` = 'count > 0',
          `count-selected-text` = '{0} selected (of {1})',
          `live-search` = TRUE,
          `header` = 'Select multiple items',
          `none-selected-text` = 'No selection!',
          `maxOptions` = 1
        )
      )
    )
  })

  output$sel_avisit <- shiny::renderUI({
     if (is.null(adlb_data())) {
      return()
    } else {
      choices <- as.list(names(adlb_data()))
      choices <- c(choices[stringr::str_detect(choices, "AVISIT")], choices[!(stringr::str_detect(choices, "AVISIT"))])
      # possible treatment variable names (add more here if needed)
      avisit_variable_names <- c("AVISIT","AVISITN","VISIT")
      # determine the order of selection (here trt01p is selected as default if available)
      selected <- avisit_variable_names[which(avisit_variable_names %in% choices)[1]]
    }
    shinyWidgets::pickerInput(
      inputId = ns("sel_avisit"),
      label = "Select visit variable",
      choices = choices,
      selected = selected,
      multiple = FALSE,
      options = shinyWidgets::pickerOptions(
        list(
          `actions-box` = TRUE,
          `selected-text-format` = 'count > 0',
          `count-selected-text` = '{0} selected (of {1})',
          `live-search` = TRUE,
          `header` = 'Select multiple items',
          `none-selected-text` = 'No selection!',
          `maxOptions` = 1
        )
      )
    )
  })

  output$sel_lab_name <- shiny::renderUI({
     if (is.null(adlb_data())) {
      return()
    } else {
      choices <- as.list(names(adlb_data()))
      choices <- c(choices[stringr::str_detect(choices, "PARAM")], choices[!(stringr::str_detect(choices, "PARAM"))])
      # possible treatment variable names (add more here if needed)
      lab_name_variable_names <- c("PARAM","LBTESTCD")
      # determine the order of selection (here trt01p is selected as default if available)
      selected <- lab_name_variable_names[which(lab_name_variable_names %in% choices)[1]]
    }
    shinyWidgets::pickerInput(
      inputId = ns("sel_lab_name"),
      label = "Select lab name variable",
      choices = choices,
      selected = selected,
      multiple = FALSE,
      options = shinyWidgets::pickerOptions(
        list(
          `actions-box` = TRUE,
          `selected-text-format` = 'count > 0',
          `count-selected-text` = '{0} selected (of {1})',
          `live-search` = TRUE,
          `header` = 'Select multiple items',
          `none-selected-text` = 'No selection!',
          `maxOptions` = 1
        )
      )
    )
  })

  output$sel_lab_value <- shiny::renderUI({
     if (is.null(adlb_data())) {
      return()
    } else {
      choices <- as.list(names(adlb_data()))
      choices <- c(choices[stringr::str_detect(choices, "AVAL")], choices[!(stringr::str_detect(choices, "AVAL"))])
      # possible treatment variable names (add more here if needed)
      lab_value_variable_names <- c("AVAL")
      # determine the order of selection (here trt01p is selected as default if available)
      selected <- lab_value_variable_names[which(lab_value_variable_names %in% choices)[1]]
    }
    shinyWidgets::pickerInput(
      inputId = ns("sel_lab_value"),
      label = "Select lab value variable",
      choices = choices,
      selected = selected,
      multiple = FALSE,
      options = shinyWidgets::pickerOptions(
        list(
          `actions-box` = TRUE,
          `selected-text-format` = 'count > 0',
          `count-selected-text` = '{0} selected (of {1})',
          `live-search` = TRUE,
          `header` = 'Select multiple items',
          `none-selected-text` = 'No selection!',
          `maxOptions` = 1
        )
      )
    )
  })

  output$sel_lower_limit <- shiny::renderUI({
     if (is.null(adlb_data())) {
      return()
    } else {
      choices <- as.list(names(adlb_data()))
      choices <- c(choices[endsWith(unlist(choices), "LO")], "NOTHING SELECTED", choices[!(endsWith(unlist(choices), "LO"))])
      # possible treatment variable names (add more here if needed)
      lower_limit_variable_names <- c("LBORNRLO","ARNLO")
      # determine the order of selection (here trt01p is selected as default if available)
      selected <- lower_limit_variable_names[which(lower_limit_variable_names %in% choices)[1]]
    }
    shinyWidgets::pickerInput(
      inputId = ns("sel_lower_limit"),
      label = "Select lab lower limit variable",
      choices = choices,
      selected = selected,
      multiple = FALSE,
      options = shinyWidgets::pickerOptions(
        list(
          `actions-box` = TRUE,
          `selected-text-format` = 'count > 0',
          `count-selected-text` = '{0} selected (of {1})',
          `live-search` = TRUE,
          `header` = 'Select multiple items',
          `none-selected-text` = 'No selection!',
          `maxOptions` = 1
        )
      )
    )
  })

  output$sel_upper_limit <- shiny::renderUI({
     if (is.null(adlb_data())) {
      return()
    } else {
      choices <- as.list(names(adlb_data()))
      choices <- c(choices[endsWith(unlist(choices), "HI")], "NOTHING SELECTED", choices[!(endsWith(unlist(choices), "HI"))])
      # possible treatment variable names (add more here if needed)
      upper_limit_variable_names <- c("LBORNRHI")
      # determine the order of selection (here trt01p is selected as default if available)
      selected <- upper_limit_variable_names[which(upper_limit_variable_names %in% choices)[1]]
    }
    shinyWidgets::pickerInput(
      inputId = ns("sel_upper_limit"),
      label = "Select lab upper limit variable",
      choices = choices,
      selected = selected,
      multiple = FALSE,
      options = shinyWidgets::pickerOptions(
        list(
          `actions-box` = TRUE,
          `selected-text-format` = 'count > 0',
          `count-selected-text` = '{0} selected (of {1})',
          `live-search` = TRUE,
          `header` = 'Select multiple items',
          `none-selected-text` = 'No selection!',
          `maxOptions` = 1
        )
      )
    )
  })

  adlb_filtered <- shiny::reactive({
    shiny::req(adlb_data())
    adlb_data <- adlb_data()
    data <- adlb_data
    if (length(id_adlb_m$myList) != 0) {
      names <- id_adlb_m$myList2
      vars <- id_adlb_m$myList
      if (length(id_adlb_m$myList) && !is.null(id_adlb_m$myList2)) {
        data_filt <- data
        for (i in 1:length(id_adlb_m$myList)) {
          if(adlb_data %>%
             dplyr::pull(id_adlb_m$myList2[i]) %>%
             is.numeric()) {
            if(!is.null(input[[id_adlb_m$myList[[i]]]]))
              data_filt <- data_filt[data_filt %>%
                                       dplyr::pull(id_adlb_m$myList2[i]) %>%
                                       dplyr::between(input[[id_adlb_m$myList[[i]]]][1],input[[id_adlb_m$myList[[i]]]][2]),]
          }else{
            data_filt <- data_filt %>%
              dplyr::filter(!! rlang::sym(id_adlb_m$myList2[i]) %in% c(input[[id_adlb_m$myList[i]]]))
          }
        }
      }
    } else {
      data_filt <- data
    }
    data_filt
  })

 # csv_file <- shiny::eventReactive(input$btn3, {
  csv_file <- shiny::reactive({
    adlb <- shiny::req(adlb_filtered())

      adlb$'NOTHING SELECTED' <- NA

      adlb <- adlb %>%
        dplyr::mutate(
          SUBJIDN = !!rlang::sym(input$sel_subjid),
          TRTP = !!rlang::sym(input$sel_treatment),
          AVISIT = !!rlang::sym(input$sel_avisit),
          LBORRES = !!rlang::sym(input$sel_lab_value),
          LBTESTCD = !!rlang::sym(input$sel_lab_name),
          LBORNRLO = !!rlang::sym(input$sel_lower_limit),
          LBORNRHI = !!rlang::sym(input$sel_upper_limit)
        ) %>%
        dplyr::select(SUBJIDN, TRTP, AVISIT, LBORRES, LBTESTCD, LBORNRLO, LBORNRHI)

      # rename treatment variable names
      vars_trtp <- unique(adlb[,"TRTP"])
      vars_trtp <- sort(as.vector(dplyr::pull(vars_trtp)))

      for(i in vars_trtp) {
        if (!is.null(input[[paste0("treatment_name", i)]])) {
         adlb$TRTP[adlb$TRTP == i] <- input[[paste0("treatment_name", i)]]
        }
      }
      # rename visit variable names
      vars_avisit <- unique(adlb[,"AVISIT"])
      vars_avisit <- sort(as.vector(dplyr::pull(vars_avisit)))

      for(i in vars_avisit) {
        if (!is.null(input[[paste0("visit_name", i)]])) {
         adlb$AVISIT[adlb$AVISIT == i] <- input[[paste0("visit_name", i)]]
        }
      }
     # rename lab variable names
      vars_lab <- unique(adlb[,"LBTESTCD"])
      vars_lab <- sort(as.vector(dplyr::pull(vars_lab)))

      for(i in vars_lab) {
        if (!is.null(input[[paste0("lab_name_name", i)]])) {
         adlb$LBTESTCD[adlb$LBTESTCD == i] <- input[[paste0("lab_name_name", i)]]
        }
      }

    #remove duplicated rows?!
    index <- adlb %>%
      dplyr::select("SUBJIDN","LBTESTCD","AVISIT","TRTP") %>%
      duplicated()

    adlb <- adlb[!index,]

    csv2 <- adlb
    return(csv2)
  })

  output$table_adlb <- renderDataTable(adlb_data(), options = list(autoWidth = FALSE))
  output$table_csv <- renderDataTable(csv_file(), options = list(autoWidth = FALSE))

  #### FILTER ####
  # Reset initial values if Remove Button is clicked
  shiny::observeEvent(input$removeBtn_file, {
    id_adlb_m$myList <- list()
    id_adlb_m$myList2 <- list()
  })

  # Delete UI Elements if Remove Button is clicked
  shiny::observeEvent(input$removeBtn_file, {
    for (i in 1:length(inserted_adlb)) {
      removeUI(selector = paste0("#", inserted_adlb[i]))
    }
  })

  output$filter_percentage_file <- shiny::renderUI({
    total_tmp <- dim(adlb_data())[1]
    value_tmp <- dim(adlb_filtered())[1]
    shinyWidgets::progressBar(
            id = ns("filter_percentage_file"),
            value = value_tmp,
            total = total_tmp,
            title = "Number of Rows of adlb (+adsl)",
            display_pct = TRUE
    )
  })

  output$pickerinput_adlb <- shiny::renderUI({
    shiny::req(adlb_data())

    adlb_data <- adlb_data()

    adlb_data_variables_tmp <- purrr::map(
      adlb_data,
      function(x) attr(x, "label", exact = TRUE)
    )
    adlb_data_variables = names(adlb_data_variables_tmp)
    names(adlb_data_variables) = paste0(
      names(adlb_data_variables_tmp),
      ifelse(
        as.character(adlb_data_variables_tmp) == "NULL",
        "",
        paste0(" - ", as.character(adlb_data_variables_tmp))
      )
    )

    choices <- adlb_data_variables

    shinyWidgets::pickerInput(
      inputId = ns('pickerinput_adlb'),
      label = 'Select filter variable(s) for adlb data set',
      choices = choices,
      selected = NULL,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = 'count > 0',
        `count-selected-text` = '{0} selected (of {1})',
        `live-search` = TRUE,
        `header` = 'Select multiple items',
        `none-selected-text` = 'No selection!'
      )
    )
  })

  inserted_adlb <- c()

  id_adlb_nr <- c()
  id_adlb_nr2 <- c()

  id_adlb_m <- shiny::reactiveValues()
  id_adlb_m$myList <- list()
  id_adlb_m$myList2 <- list()
  inserted_adlb_list <- shiny::reactive({
    list()
  })

  condition_filter <- shiny::reactiveValues(val = FALSE)
  output$condition_filter <- shiny::reactive(condition_filter$val)
  # observe the 'Insert' Button click:
  shiny::observeEvent(c(input$insertBtn_file), {

    shiny::req(adlb_data())

    adlb_data <- adlb_data()

    ins_adlb <- inserted_adlb_list()
    id_adlb_nr <<- c()
    id_adlb_nr2 <<- c()

    if (length(inserted_adlb) > 0) {
      for (i in 1:length(inserted_adlb)) {
        shiny::removeUI(
          ## pass in appropriate div id
          selector = paste0('#', inserted_adlb[i])
        )
      }
    }

    inserted_adlb <<- c()

    btn <- input$insertBtn_file

    pickerinput_adlb <- input$pickerinput_adlb

    if (length(pickerinput_adlb) > 0) {
      for (i in 1: length(pickerinput_adlb)) {
        id <- paste0(pickerinput_adlb[i], btn)
        shiny::insertUI(
          selector = '#placeholder_file',
          ui = shiny::tags$div(
            if (!is.numeric(adlb_data %>%
                           dplyr::pull(pickerinput_adlb[i]))) {
              shinyWidgets::pickerInput(
                inputId = ns(id),
                label = paste0(pickerinput_adlb[i]),
                choices = adlb_data %>%
                  dplyr::pull(pickerinput_adlb[i]) %>%
                  unique,
                selected = adlb_data %>%
                  dplyr::pull(pickerinput_adlb[i]) %>%
                  unique,
                multiple = TRUE,
                choicesOpt = list(style = rep("color: white; background: #424242;",
                  length(adlb_data %>%
                  dplyr::pull(pickerinput_adlb[i]) %>%
                  unique))),
                options = list(
                  `actions-box` = TRUE,
                  `selected-text-format` = 'count > 0',
                  `count-selected-text` = '{0} selected (of {1})',
                  `live-search` = TRUE,
                  `header` = 'Select multiple items',
                  `none-selected-text` = 'All dropped!'
                )
              )
            } else if (
              is.numeric(
                adlb_data %>%
                  dplyr::pull(pickerinput_adlb[i])
              ) && !is.integer(
                adlb_data %>%
                  dplyr::pull(pickerinput_adlb[i])
                )
            ) {
              shiny::sliderInput(
                inputId = ns(id),
                label = paste0(pickerinput_adlb[i]),
                value = c(
                  adlb_data %>%
                    dplyr::pull(pickerinput_adlb[i]) %>%
                    base::min(na.rm = TRUE), adlb_data %>%
                    dplyr::pull(pickerinput_adlb[i]) %>%
                    base::max(na.rm = TRUE)
                ),
                min = adlb_data %>%
                 dplyr::pull(pickerinput_adlb[i]) %>%
                 base::min(na.rm = TRUE),
                max = adlb_data %>%
                 dplyr::pull(pickerinput_adlb[i]) %>%
                 base::max(na.rm = TRUE)
              )
            } else if (is.numeric(adlb_data %>%
                                 dplyr::pull(pickerinput_adlb[i])) && is.integer(adlb_data %>%
                                                                                 dplyr::pull(pickerinput_adlb[i]))) {
              shiny::sliderInput(
                inputId = ns(id),
                label = paste0(pickerinput_adlb[i]),
                value = c(adlb_data %>%
                  dplyr::pull(pickerinput_adlb[i]) %>%
                  base::min(na.rm = TRUE),adlb_data %>%
                  dplyr::pull(pickerinput_adlb[i]) %>%
                  base::max(na.rm = TRUE)
                ),
                min = adlb_data %>%
                  dplyr::pull(pickerinput_adlb[i]) %>%
                  base::min(na.rm = TRUE),
                max = adlb_data %>%
                  dplyr::pull(pickerinput_adlb[i]) %>%
                  base::max(na.rm = TRUE),
                step = 1,
                sep = "",
                ticks = FALSE
              )
            },
            id = id
          )
        )
        inserted_adlb <<- c(id, inserted_adlb)
        ins_adlb[[pickerinput_adlb[i]]]  <- adlb_data %>%
          dplyr::pull(pickerinput_adlb[i])
        id_adlb_nr2 <<- c(id_adlb_nr2, pickerinput_adlb[[i]])
        id_adlb_nr <<- c(id_adlb_nr,id)
      }
    }

    if (length(id_adlb_nr) > 0) {
      condition_filter$val <- TRUE
    } else {
      condition_filter$val <- FALSE
    }
    id_adlb_m$myList2 <- id_adlb_nr2
    id_adlb_m$myList <- id_adlb_nr
  })


  #### save as csv ####
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("elaborator_", gsub(":","-", Sys.time()), ".csv", sep = "")
    },
    content = function(file) {
      tmp <- csv_file()
      tmp[is.na(tmp)] <- "."

      utils::write.csv(data.frame(lapply(tmp,as.character)), file, row.names = FALSE)
    }
  )

  #### RENAMING ####
  #create textInput widgets for treatment names for renaming
  output$treatment_renaming <- shiny::renderUI({
    shiny::req(adlb_data())

    adlb <- adlb_data()
    shiny::req(input$sel_treatment)

    vars <- unique(adlb[,input$sel_treatment])
    vars <- sort(as.vector(dplyr::pull(vars)))
    if (length(vars)>0) {
      div(
        lapply(vars, function(i) {
          textInput(
            inputId = ns(paste0("treatment_name", i)),
            label = paste0(input$sel_treatment, " : ", i),
            value = i
          )
        })
      )

    }
  })

  #create textInput widgets for visit names for renaming
  output$visit_renaming <- shiny::renderUI({
    shiny::req(adlb_data())

    adlb <- adlb_data()
    shiny::req(input$sel_avisit)

    vars <- unique(adlb[,input$sel_avisit])
    vars <- sort(as.vector(dplyr::pull(vars)))
    if (length(vars)>0) {
      div(
        lapply(vars, function(i) {
          textInput(
            inputId = ns(paste0("visit_name", i)),
            label = paste0(input$sel_avisit, " : ", i),
            value = i
          )
        })
      )
    }
  })

  #create textInput widgets for lab names for renaming
  output$lab_name_renaming <- shiny::renderUI({
    shiny::req(adlb_data())

    adlb <- adlb_data()
    shiny::req(input$sel_lab_name)

    vars <- unique(adlb[,input$sel_lab_name])
    vars <- sort(as.vector(dplyr::pull(vars)))
    if (length(vars)>0) {
      div(
        lapply(vars, function(i) {
          textInput(
            inputId = ns(paste0("lab_name_name", i)),
            label = paste0(input$sel_lab_name, " : ", i),
            value = i
          )
        })
      )
    }
  })

  #return data and send button information
  return(
    list(
      df = shiny::reactive({csv_file()}),
      send_button = shiny::reactive({input$btn3})
    )
  )
}
