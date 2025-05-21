#' Server part of the elaborator application
#'
#'@param input,output,session Internal parameters for shiny.
#'
#'@return No return value. Server part of the app, used in launch_elaborator-function.
#'
#'@keywords internal

elaborator_server <- function(input, output, session) {

  LBTESTCD <- TRTP <- AVISIT <- orderinglab <- median_value <- NULL
  LBORRES_diff <- SUBJIDN <- visits_non_missing <- tr <- data <- nonmissing <- tot <- NULL
  percentage <- trt <- LBTESTCD <- nonmiss <- nr_visits <- . <- LBORRES <- NULL
  LBORNRLO <- non_missing_values <- LBORNRHI <- highref <- lowref <- InQuRa <- NULL
  Range <- refRange <- LBORRES.y <- visit_removed <- all_complete <- LBORRES.x <- vari <- js <- NULL


  #global settings

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

  #### Import data ####
  output$impdata <- shiny::renderUI({
    if (input$impswitch == '*.RData file') {
      shiny::fileInput(
        inputId = 'file',
        label = 'Choose RData file',
        multiple = FALSE,
        accept = '.RData'
      )
    } else if (input$impswitch == '*.CSV file') {
      shiny::tagList(
        shiny::fixedRow(
          shiny::fileInput(
            inputId = 'csv_file',
            label = 'Choose CSV file',
            multiple = TRUE,
            accept = c('text/csv',
                       'text/comma-separated-values,text/plain',
                       '.csv'
            )
          ),
          shinyWidgets::prettyRadioButtons(
            inputId = 'sep',
            label = 'Select separator',
            inline = TRUE,
            choices = c('Comma' = ',',
                        'Semicolon' = ';',
                        'Tab' = '\t'),
            status ="warning",
            animation = "smooth",
            selected = ','
          ),
          shinyWidgets::prettyRadioButtons(
            inputId = 'quote',
            label = 'Select quote',
            inline = TRUE,
            choices = c(
              None = '',
              'Double Quote (")' = '"',
              "Single Quote (')" = "'"
            ),
            selected = '"',
            status ="warning",
            animation = "smooth"
          ),
          shinyWidgets::prettyRadioButtons(
            inputId = 'dec',
            label = 'Select decimal character',
            status ="warning",
            animation = "smooth",
            inline = TRUE,
            choices= c(
              'Point (.)' = '.',
              'Comma (,)' = ','
            ),
            selected = '.'
          )
        )
      )
    }
  })


  shiny::observeEvent(app_input(), {
    if(!is.null(app_input())) {
      shinyWidgets::updatePrettyRadioButtons(
        session,
        inputId = 'impswitch',
        choices = c('Loaded Data','*.RData file', '*.CSV file')
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
    tmp <- elaborator_load_and_check(
      data_switch = input$impswitch,
      rdata_file_path = input$file$datapath,
      csv_file_path = input$csv_file$datapath,
      loaded_file = app_input(),
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
    elaborator_expand_grid <- function(dat){
      tmp <- expand.grid(unique(dat$SUBJIDN), unique(dat$AVISIT), unique(dat$LBTESTCD))
      treatment <- dat %>%
        dplyr::select(SUBJIDN, TRTP) %>%
        distinct()
      colnames(tmp) <- c("SUBJIDN","AVISIT","LBTESTCD")
      dat2 <- dat %>%
        dplyr::right_join(tmp, by = c("SUBJIDN","AVISIT","LBTESTCD")) %>%
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
    if(!is.null(raw_data_and_warnings()$message)) {
      str1 <- raw_data_and_warnings()$message
      paste(str1)
    }
  })

  #### 2. filter by app filter-tab  ####
  # used function:
  # ---
  # purpose:
  # filter data set by filter-tab selection within elaborator app.
  #
  # reactivity triggers :
  # raw_data_and_warnings() / <filter selection within app>

  filtered_raw_data <- shiny::reactive({
    shiny::req(raw_data_and_warnings()$data)
    elab_data <- raw_data_and_warnings()$data
    data <- elab_data
    if (length(id_elab_m$myList) != 0) {
      names <- id_elab_m$myList2
      vars <- id_elab_m$myList
      if (length(id_elab_m$myList) && !is.null(id_elab_m$myList2)) {
        data_filt <- data
        for (i in 1:length(id_elab_m$myList)) {
          if(elab_data %>%
             dplyr::pull(id_elab_m$myList2[i]) %>%
             is.numeric()) {
            if (!is.null(input[[id_elab_m$myList[[i]]]]))
              data_filt <- data_filt[data_filt %>%
                                       dplyr::pull(id_elab_m$myList2[i]) %>%
                                       dplyr::between(input[[id_elab_m$myList[[i]]]][1],input[[id_elab_m$myList[[i]]]][2]),]
          } else {
            data_filt <- data_filt %>%
              dplyr::filter(!! rlang::sym(id_elab_m$myList2[i]) %in% c(input[[id_elab_m$myList[i]]]))
          }
        }
      }
    } else {
      data_filt <- data
    }
    data_filt
  })

  #### 3. filter data by app settings ####
  # used function:
  # elaborator_filter_by_app_selection
  #
  # purpose:
  # filter data set by upload selection within elaborator app (visits/treatment/labparameter)
  #
  # reactivity triggers :
  # filtered_raw_data() / input$select.visit / input$select.treatments / input$select.lab
  filtered_and_reduced_raw_data <- shiny::reactive({
    filtered_data <- elaborator_filter_by_app_selection(
      elab_data = filtered_raw_data(),
      visits = input$select.visit,
      treat = input$select.treatments,
      labparameter =input$select.lab
    )
    filtered_data
  })

  #### 4. create 'remove'-flags due to tolerated missing percentage ####
  # used function:
  # elaborator_remove_visits_due_tolerated_missings
  #
  # purpose:
  # create 'remove'-flags due to tolerated missing percentage.
  #
  # reactivity triggers :
  # filtered_and_reduced_raw_data() / input$select.toleratedPercentage

  data_with_missing_flag <- shiny::reactive({
    shiny::req(filtered_and_reduced_raw_data())
    shiny::req(input$select.toleratedPercentage)

    filtered_and_removed_visits <- elaborator_remove_visits_due_tolerated_missings(
      elab_data = filtered_and_reduced_raw_data(),
      tolerated_value = (input$select.toleratedPercentage/100)
    )
    filtered_and_removed_visits
  })

  ####    5. remove visits due to tolerated percentage missing:####
  # used function:
  # ---
  #
  # purpose: remove visits due to tolerated percentage missing
  #
  #
  # reactivity triggers :
  # data_with_missing_flag()

  data_without_missing_visits <- shiny::reactive({
    #remove visits
    shiny::req(data_with_missing_flag())
    filtered_and_removed_visits <- data_with_missing_flag() %>%
      dplyr::filter(visit_removed == FALSE)
    filtered_and_removed_visits
  })

  ####    6. change classes and factor levels: ####
  # used function:
  # elaborator_change_class_required_variables
  #
  # purpose: change classes and factor levels
  #
  # reactivity triggers :
  # data_without_missing_visits() / input$select.visit / input$select.treatments / raw_data_and_warnings()
  data_filtered_by_app_selection <- shiny::reactive({
    shiny::req(input$select.treatments, input$select.visit, data_without_missing_visits(), raw_data_and_warnings())
    filtered_data2 <- elaborator_change_class_required_variables(
      elab_data = data_without_missing_visits(),
      visit = input$select.visit,
      treatment = input$select.treatments,
      lab = unique(raw_data_and_warnings()$data$LBTESTCD)
    )
    filtered_data2
  })


  #### preprocess lines for quantitative trends####
  quant_plot_data_lines <- shiny::reactive({
    shiny::req(filtered_and_reduced_raw_data())
    tmp <- filtered_and_reduced_raw_data() %>%
      dplyr::group_by(
        TRTP, LBTESTCD
      ) %>%
      dplyr::select(TRTP, LBTESTCD, SUBJIDN, AVISIT, LBORRES) %>%
      ## by group? map_group?
      tidyr::pivot_wider(names_from = AVISIT, values_from = LBORRES) %>%
      dplyr::select(-SUBJIDN)
    tmp <- tmp[,c("TRTP","LBTESTCD",levels(data_with_selected_factor_levels()$AVISIT))]
    tmp
  })

  #### AI Sorting ####
  ####    a1. prepare distance matrix (only if ai sorting is selected) ####
  prepare_dist_matrix_for_clustering <- shiny::eventReactive(c(input$go3), {
    shiny::req(data_filtered_by_app_selection())
    ds <- data_filtered_by_app_selection()
    if (shiny::isolate(input$orderinglab) == "auto") {
      first <- shiny::isolate(input$select.ai.first)
      last <- shiny::isolate(input$select.ai.last)
      shiny::validate(
        shiny::need(
          first != last,
          "Please select different Timepoints for Seriation!
          The first timepoint must differ from second timepoint."
        )
      )

      elaborator_prepare_clustering_matrix(
        elab_data = ds,
        first_variable = first,
        last_variable = last
      )
    } else {
      NULL
    }
  })

  ####    a2. use package seriation for ordering lab parameter ####
  lab_parameter_order_by_clustering <- shiny::eventReactive(input$go3, {
    shiny::req(shiny::isolate(data_filtered_by_app_selection()))
    tmp2 <- shiny::isolate(prepare_dist_matrix_for_clustering())
    ds <- shiny::isolate(data_filtered_by_app_selection())

    if (input$orderinglab == "asinp") {
      as.character(unique(ds$LBTESTCD))
    }
    else if (input$orderinglab == "alphabetically") {
      sort(as.character(unique(ds$LBTESTCD)))
    }
    else if (input$orderinglab == "auto") {
      shiny::req(prepare_dist_matrix_for_clustering())
      tmp2 %>%
        elaborator_calculate_spearman_distance() %>%
        seriation::seriate(method = input$clusterMethod) %>%
        seriation::get_order() %>%
        rownames(tmp2)[.]
    } else if (input$orderinglab == "manual") {
      input$arrange.lab
    } else {
      as.character(unique(ds$LBTESTCD))
    }
  })


  #### 7. refactor lab parameter value: ####
  # used function: ---
  #
  # purpose: re-factor lab parameter value
  #
  #
  # reactivity triggers :
  # data_filtered_by_app_selection() / input$go3

  data_with_selected_factor_levels <- shiny::eventReactive(c(data_filtered_by_app_selection(),input$go3),{
    tmp <- data_filtered_by_app_selection()
    #re-level the lab parameter vector for arrangement within app
    if(shiny::isolate(input$orderinglab) == "asinp") {
      lab_levels <- unique(raw_data_and_warnings()$data$LBTESTCD)
      lab_levels <- lab_levels[lab_levels %in% input$select.lab]
      tmp$LBTESTCD <- factor(tmp$LBTESTCD, levels = lab_levels)
    } else if (shiny::isolate(input$orderinglab) =="alphabetically"){
      lab_levels <- sort(unique(raw_data_and_warnings()$data$LBTESTCD))
      lab_levels <- lab_levels[lab_levels %in% input$select.lab]
      tmp$LBTESTCD <- factor(tmp$LBTESTCD, levels = lab_levels)
    } else if (shiny::isolate(input$orderinglab) =="auto") {
      lab_levels <- shiny::isolate(lab_parameter_order_by_clustering())
      lab_levels <- c(lab_levels, as.character(unique(tmp$LBTESTCD)[which(!unique(tmp$LBTESTCD) %in% lab_levels)]))
      tmp$LBTESTCD <- factor(tmp$LBTESTCD, levels = lab_levels)
    } else if(shiny::isolate(input$orderinglab) == "manual"){
      lab_levels <- input$arrange.lab
      lab_levels <- lab_levels[lab_levels %in% input$select.lab]
      tmp$LBTESTCD <- factor(tmp$LBTESTCD, levels = lab_levels)
    }
    tmp
  })


  ####  8. reduce data to patients with all lab parameters non missing: ####
  # used function: ---
  #
  # purpose: reduce data to patients with all lab parameters non missing
  #
  #
  # reactivity triggers :
  # data_with_selected_factor_levels() / input$select.visit

  data_with_only_non_missings_over_visits <- shiny::reactive({
    shiny::req(data_with_selected_factor_levels())
    shiny::req(input$select.visit)
    tmp <- data_with_selected_factor_levels() %>%
      dplyr::full_join(
        data_with_selected_factor_levels() %>%
          dplyr::group_by(TRTP,LBTESTCD) %>%
          dplyr::summarise(visits_non_missing = length(unique(AVISIT)),.groups = "keep"),
        by = c("TRTP","LBTESTCD")
      )

    tmp2 <- tmp %>%
      dplyr::right_join(
        tmp %>%
        dplyr::group_by(SUBJIDN,LBTESTCD,TRTP) %>%
        dplyr::summarise(
          non_missing_values = sum(!is.na(LBORRES)),
          all_complete = unique(ifelse(non_missing_values == ifelse(is.null(visits_non_missing),0,visits_non_missing), TRUE,FALSE)),
          .groups = "keep"
        ) %>% dplyr::ungroup() %>% dplyr::select(SUBJIDN,LBTESTCD,TRTP,all_complete) %>%
          dplyr::distinct(),
        by = c("SUBJIDN","LBTESTCD","TRTP")
      ) %>%
      dplyr::filter(all_complete == TRUE)
    tmp2
  })

  ####    3. dendrogram output ####
  output$dendro_1 <- shiny::renderPlot({
    shiny::req(prepare_dist_matrix_for_clustering(), shiny::isolate(input$clusterMethod))
    if ((startsWith(shiny::isolate(input$clusterMethod), "OLO") | startsWith(shiny::isolate(input$clusterMethod), "GW"))) {
      tmp <- prepare_dist_matrix_for_clustering()
      ser <- seriation::seriate(elaborator_calculate_spearman_distance(tmp), method = shiny::isolate(input$clusterMethod))
      asdendro <- stats::as.dendrogram(ser[[1]])
      dendro <- dendextend::assign_values_to_leaves_edgePar(dend = asdendro)
      graphics::rect(
        xleft = graphics::grconvertX(0, 'ndc', 'user'),
        xright = graphics::grconvertX(1,'ndc', 'user'),
        ybottom = graphics::grconvertY(0, 'ndc', 'user'), ytop = graphics::grconvertY(1,'ndc', 'user'),
        border = NA,
        col = ColorBG,
        xpd = TRUE
      )
      on_ex <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(on_ex))
      graphics::par(bg = ColorBG)
      graphics::plot(dendro, ylab = "Distance", horiz = FALSE)
    }
  })


  #### QUANTITATIVE TRENDS ####
  output$hover <- shiny::renderPlot({
    input$apply_quant_plot
    shiny::req(shiny::isolate(data_with_selected_factor_levels()), input$plot_option_switch)

    # switch between hover or click options for zoom panel
    if (input$plot_option_switch == "hover") {
      plot_coords <- input$dist_hover
    } else if (input$plot_option_switch == "click") {
      plot_coords <- input$dist_click
    }

    if (!is.null(plot_coords$coords_css$y) & !is.null(plot_coords$coords_css$x)) {
      if (plot_coords$coords_css$y > 0  & plot_coords$coords_css$x > 0) {
        y <- plot_coords$coords_css$y
        x <- plot_coords$coords_css$x
        if(!is.null(y) && !is.null(x)) {

          #use only subjects with non missing values for all visits
            dat <- data_with_only_non_missings_over_visits()

            #load statistical test values (saved in values$default)

            val <- shiny::isolate(values$default)
            if (!is.list(val)) {
              info <- NA
            } else {
              info <- shiny::isolate(values$default)
            }
            #replace values$default with newer version
            #load statistical test values (saved in statistical_test_resulst$var)
            if (input$go != 0) {
              b.col <- shiny::isolate(box_col())
            } else {
              b.col <- c(colBoxplot2, colBoxplot2, colBoxplot2, colBoxplot2)
            }
            if (shiny::isolate(input$stattest) != "none") {
              bordcol <- shiny::isolate(border.col())
            } else {
              bordcol <- NULL
            }

          sortin <- levels(dat$LBTESTCD)[levels(dat$LBTESTCD) %in% unique(dat$LBTESTCD)]
          dat_filt <- dat[dat$TRTP == levels(dat$TRTP)[ceiling(y/input$zoompx)] & dat$LBTESTCD == sortin[ceiling(x / input$zoompx)] ,]
          dat_filt$TRTP <- factor(dat_filt$TRTP)
          dat_filt$LBTESTCD <- factor(dat_filt$LBTESTCD)

          if(input$con_lin){
           lines_data <- quant_plot_data_lines() %>%
            dplyr::filter(
              TRTP == dat %>%
                dplyr::pull(TRTP) %>%
                levels() %>%
                .[ceiling(y / isolate(input$zoompx))], LBTESTCD == sortin[ceiling(x / isolate(input$zoompx))]
            )
          } else {
            lines_data <- NULL
          }
          if (!is.null(statistical_test_results$var)) {
          infotest <- statistical_test_results$var %>%
            dplyr::filter(
              TRTP == dat %>%
                dplyr::pull(TRTP) %>%
                levels() %>%
                .[ceiling(y / isolate(input$zoompx))], LBTESTCD == sortin[ceiling(x / isolate(input$zoompx))]
            )
          } else {
            infotest <- NULL
          }


          elaborator_plot_quant_trends(
            dat_filt,
            #shiny::isolate(data_with_only_non_missings_over_visits()),
            signtest = ifelse(shiny::isolate(input$stattest) == "signtest", TRUE, FALSE),
            Visit1 = shiny::isolate(input$trtcompar)[1],
            Visit2 = shiny::isolate(input$trtcompar)[-1],
            labcolumn = "LBTESTCD",
            cols = b.col,
            pcutoff = shiny::isolate(input$pcutoff),
            sameaxes = shiny::isolate(input$sameaxes),
            sortpoints = shiny::isolate(input$sortpoint),
            labelvis = NULL,
            infotest = infotest,
            sortinput = levels(dat_filt$LBTESTCD),
            bordercol = bordcol,
            add_points = shiny::isolate(input$add_points),
            connect_lines = shiny::isolate(input$con_lin),
            lin_data = lines_data,
            outliers = shiny::isolate(input$outlier),
            tolerated_percentage = shiny::isolate(input$select.toleratedPercentage),
            color_lines_options = shiny::isolate(input$con_lin_options),
            custom_visits = shiny::isolate(input$custom_visits)
          )
        }
      } else {
        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          axes = FALSE,
          xlab = "",
          ylab = ""
        )
        rect(
          xleft = grconvertX(0,'ndc','user'),
          xright = grconvertX(1, 'ndc', 'user'),
          ybottom = grconvertY(0,'ndc','user'),
          ytop = grconvertY(1, 'ndc', 'user'),
          border = NA,
          col = ColorBG,
          xpd = TRUE
        )
        text(0.5, 0.6, ifelse(input$plot_option_switch == "hover", "Please move your mouse over the plots", "Please click on the plots"), col = ColorFont)
        text(0.5, 0.4, "to get an enlarged version of the plot!", col = ColorFont)
      }
    } else {
      plot(
        NULL,
        xlim = c(0, 1),
        ylim = c(0, 1),
        axes = FALSE,
        xlab = "",
        ylab = ""
      )
      rect(
        xleft = grconvertX(0,'ndc','user'),
        xright = grconvertX(1, 'ndc', 'user'),
        ybottom = grconvertY(0,'ndc','user'),
        ytop = grconvertY(1, 'ndc', 'user'),
        border = NA,
        col = ColorBG,
        xpd = TRUE
      )
      text(0.5, 0.6, ifelse(input$plot_option_switch == "hover", "Please move your mouse over the plots", "Please click on the plots"), col = ColorFont)
      text(0.5, 0.4, "to get an enlarged version of the plot!", col = ColorFont)
    }
  }, width = 400
  )

  output$hoverpanel <- shiny::renderUI({
    shiny::absolutePanel(
      id = "hoverpanel",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0("<div style='background-color:", ColorBG, "'>")),
      HTML('<button style = "background: #f6ad82; color:#ffffff", data-toggle="collapse" data-target="#demo" style="color:white;"> <i class="fa-solid fa-search-plus"></i> Open/Close Zoom Panel</button>'),
      top = 70,
      left = "auto",
      right = 100,
      bottom = "auto",
      width = 400,
      height = "auto",
      tags$div(id = 'demo',
         class = "collapse",
         shiny::fluidRow(
           shiny::column(2,
             shiny::plotOutput('hover')
           )
          ),
          shiny::fluidRow(
            shiny::column(12, offset = 4,
              shiny::radioButtons(
                inputId = "plot_option_switch",
                label = NULL,
                choices = c("hover", "click"),
                selected = c("hover"),
                inline = TRUE
              )
            )
          ),
         shiny::fluidRow(
           shiny::column(12,
             uiOutput('hover_info_text')
           )
         )

      ),
      style = "z-index: 99999;"
    )
  })

  output$hover_info_text <- shiny::renderUI({
    input$apply_quant_plot
    shiny::req(shiny::isolate(data_with_missing_flag()), input$plot_option_switch)

    # switch between hover or click options for zoom panel
    if (input$plot_option_switch == "hover") {
      plot_coords <- input$dist_hover
    } else if (input$plot_option_switch == "click") {
      plot_coords <- input$dist_click
    }

    if (!is.null(plot_coords$coords_css$y) & !is.null(plot_coords$coords_css$x)) {
      if (plot_coords$coords_css$y > 0  & plot_coords$coords_css$x > 0) {
        y <- plot_coords$coords_css$y
        x <- plot_coords$coords_css$x
        if (!is.null(y) && !is.null(x)) {


          dat <- shiny::isolate(data_with_only_non_missings_over_visits())
          #dat <- shiny::isolate(data_with_selected_factor_levels())

          sortin <- levels(dat$LBTESTCD)[levels(dat$LBTESTCD) %in% unique(dat$LBTESTCD)]
          #sortin <- levels(dat$LBTESTCD)
          val <- shiny::isolate(values$default)
          hover_treatment <- dat %>%
            dplyr::pull(TRTP) %>%
            levels() %>%
            .[ceiling(y / isolate(input$zoompx))]

          hover_labparameter <- sortin[ceiling(x / isolate(input$zoompx))]

          text <- elaborator_create_hover_info_text(
            elab_data = data_with_missing_flag(),
            labparameter = hover_labparameter,
            treat = hover_treatment,
            select.visit = input$select.visit
          )

          HTML(
            text
          )
        }
      }
    } else {
      HTML("")
    }
  })

  output$dendro_2 <- shiny::renderPlot({
    if ((startsWith(shiny::isolate(input$clusterMethod), "OLO") | startsWith(shiny::isolate(input$clusterMethod), "GW"))) {
      shiny::req(prepare_dist_matrix_for_clustering(), shiny::isolate(input$clusterMethod))
      tmp <- prepare_dist_matrix_for_clustering()
      ser <- seriation::seriate(elaborator_calculate_spearman_distance(tmp), method = shiny::isolate(input$clusterMethod))
      asdendro <- stats::as.dendrogram(ser[[1]])
      dendro2 <- dendextend::assign_values_to_leaves_edgePar(dend = asdendro)

      graphics::rect(
        xleft = graphics::grconvertX(0,'ndc','user'),
        xright = graphics::grconvertX(1, 'ndc', 'user'),
        ybottom = graphics::grconvertY(0,'ndc','user'),
        ytop = graphics::grconvertY(1, 'ndc', 'user'),
        border = NA,
        col = ColorBG,
        xpd = TRUE
      )
      on_ex <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(on_ex))
      graphics::par(bg = ColorBG)
      graphics::plot(dendro2, ylab = "Distance", horiz = FALSE)
    }
  })

  output$legend <- shiny::renderPlot({
    on_ex <- graphics::par("mfrow","oma","mar")
    on.exit(graphics::par(on_ex))
    graphics::par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(0,0,0,0))
    graphics::plot(
      NULL,
      NULL,
      ylim = c(0,10),
      xlim = c(0,1),
      axes = FALSE,
      ylab = "",
      xlab = ""
    )
    leg.x <- 0.5
    leg.y <- seq(graphics::grconvertY(0, 'npc', 'user'), graphics::grconvertY(1, 'npc', 'user'), length.out = 12)
    leg.width <- 1
    graphics::rect(
      xleft = leg.x - 2,
      xright = leg.x + 2,
      ybottom = leg.y[-1],
      ytop = leg.y[-length(leg.y)],
      xpd = NA,
      col = c(c('white', colChoice[[shiny::req(input$select.pal1)]]$col, 'black')),
      border = TRUE
    )
    graphics::text(
      x = leg.x,
      y = leg.y[-1] - 0.5,
      labels = c(
        "0-5%",
        "5-10%",
        "10-15%",
        "15-20%",
        "20-25%",
        "25-30%",
        "30-35%",
        "35-40%",
        "40-45%",
        "45-50%",
        ">50%"
      ),
      col = c(
        'black','black','black','black','black','black',
        'white','white','white','white','white'
      )
    )
  }, width = 84.53)

  output$legendpanel <- shiny::renderUI({
    shiny::absolutePanel(
      id = "legendpanel",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      top = 240,
      left = "auto",
      right = 50,
      bottom = "auto",
      width = 84.53,
      height = "auto",

      HTML('<button style="background: #f6ad82; color:#ffffff", data-toggle="collapse" data-target="#demo_co">Open/Close</button>'),
      tags$div(id = 'demo_co',  class="collapse in",
        shiny::fluidRow(
          shiny::column(2,
            shiny::plotOutput(
              'legend'
            )
          )
        )

      ),
      style = "z-index: 10;"
    )
  })

  output$hoverpanel2 <- shiny::renderUI({

    nvi <- data_param()$nvisit

    shiny::absolutePanel(
      id = "hoverpanel2",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0("<div style='background-color:", ColorBG, "'>")),
      HTML('<button style="background: #f6ad82; color:#ffffff", data-toggle="collapse" data-target="#demo2"> <i class="fa-solid fa-search-plus"></i> Open/Close Zoom Panel</button>'),
      top = 70,
      left = "auto",
      right = 100,
      bottom = "auto",
      width = nvi  * 100,
      height = "auto",
      tags$div(id = 'demo2',
               class = "collapse",
               shiny::fluidRow(
           shiny::column(2,
             shiny::plotOutput('hover2')
           )
          ),
          shiny::fluidRow(
            shiny::column(12, offset = 4,
              shiny::radioButtons(
                inputId = "plot_option_switch2",
                label = NULL,
                choices = c("hover", "click"),
                selected = c("hover"),
                inline = TRUE
              )
            )
          ),
         shiny::fluidRow(
           shiny::column(12,
             uiOutput('hover_info_text2')
           )
         )
      ),
      style = "z-index: 10;"
    )
  })

  output$hover_info_text2 <- shiny::renderUI({
    input$apply_qual_plot
    shiny::req(data_filtered_by_app_selection(), input$plot_option_switch2)

    # switch between hover or click options for zoom panel
    if (input$plot_option_switch2 == "hover") {
      plot_coords <- input$dist_hover2
    } else if (input$plot_option_switch2 == "click") {
      plot_coords <- input$dist_click2
    }

    if (!is.null(plot_coords$coords_css$y) & !is.null(plot_coords$coords_css$x)) {
      if (plot_coords$coords_css$y > 0  & plot_coords$coords_css$x > 0) {
        y <- plot_coords$coords_css$y
        x <- plot_coords$coords_css$x
        if (!is.null(y) && !is.null(x)) {

          dat <- shiny::isolate(data_with_only_non_missings_over_visits())
          #dat <- shiny::isolate(data_with_selected_factor_levels())
          sortin <- levels(dat$LBTESTCD)[levels(dat$LBTESTCD) %in% unique(dat$LBTESTCD)]
          #sortin <- levels(dat$LBTESTCD)

          val <- shiny::isolate(values$default)
          hover_treatment <- dat %>%
            dplyr::pull(TRTP) %>%
            levels() %>%
            .[ceiling(y / isolate(input$zoompx))]

          hover_labparameter <- sortin[ceiling(x / isolate(input$zoompx))]
          text <- elaborator_create_hover_info_text(
            elab_data = data_with_missing_flag(),
            labparameter = hover_labparameter,
            treat = hover_treatment,
            select.visit = input$select.visit
          )

          HTML(
            text
          )
        }
      }
    } else {
      HTML("")
    }
  })

  output$hover_info_text3 <- shiny::renderUI({
    input$apply_ref_plot
    #shiny::req(shiny::isolate(data_with_missing_flag()), input$plot_option_switch3)
    shiny::req(shiny::isolate(data_with_missing_flag()), input$plot_option_switch3)

    # switch between hover or click options for zoom panel
    if (input$plot_option_switch3 == "hover") {
      plot_coords <- input$dist_hover3
    } else if (input$plot_option_switch3 == "click") {
      plot_coords <- input$dist_click3
    }

    if (!is.null(plot_coords$coords_css$y) & !is.null(plot_coords$coords_css$x)) {
      if (plot_coords$coords_css$y > 0  & plot_coords$coords_css$x > 0) {
        y <- plot_coords$coords_css$y
        x <- plot_coords$coords_css$x
        if (!is.null(y) && !is.null(x)) {

          dat <- shiny::isolate(data_with_only_non_missings_over_visits())
          sortin <- levels(dat$LBTESTCD)[levels(dat$LBTESTCD) %in% unique(dat$LBTESTCD)]
          val <- shiny::isolate(values$default)
          hover_treatment <- dat %>%
            dplyr::pull(TRTP) %>%
            levels() %>%
            .[ceiling(y / isolate(input$zoompx))]

          hover_labparameter <- sortin[ceiling(x / isolate(input$zoompx))]

          text <- elaborator_create_hover_info_text(
            elab_data = data_with_missing_flag(),
            labparameter = hover_labparameter,
            treat = hover_treatment,
            select.visit = input$select.visit
          )

          HTML(
            text
          )
        }
      }
    } else {
      HTML("")
    }
  })

  shiny::observe({
    output$hover2 <- shiny::renderPlot({
      input$apply_qual_plot
      shiny::req(data_with_selected_factor_levels(), Summary_for_qualitative_trends(), input$plot_option_switch2)
    # switch between hover or click options for zoom panel
    if (input$plot_option_switch2 == "hover") {
      plot_coords <- input$dist_hover2
    } else if (input$plot_option_switch2 == "click") {
      plot_coords <- input$dist_click2
    }


    if (!is.null(plot_coords$coords_css$y) & !is.null(plot_coords$coords_css$x)) {


      if (plot_coords$coords_css$y > 0 & plot_coords$coords_css$x > 0) {

        dat <- data_with_only_non_missings_over_visits()
        Variab <- levels(dat$LBTESTCD)[levels(dat$LBTESTCD) %in% unique(dat$LBTESTCD)]

        dat_filt <- dat[dat$TRTP == levels(dat$TRTP)[ceiling(plot_coords$coords_css$y/input$zoompx)] & dat$LBTESTCD == Variab[ceiling(plot_coords$coords_css$x / input$zoompx)] ,]
        dat_filt$TRTP <- factor(dat_filt$TRTP)

        Summa  <- Summary_for_qualitative_trends()

        meth <- input$method
        suppressWarnings(
          elaborator_plot_qual_trends(
            dat1 = dat_filt,
            Variab[ceiling(plot_coords$coords_css$x / input$zoompx)],
            fontsize = 2,
            method = meth,
            color_palette = c('white', colChoice[[shiny::req(input$select.pal1)]]$col, 'black'),
            Summa = Summa
          )
        )
      } else {
        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          axes = FALSE,
          xlab = "",
          ylab = ""
        )
        rect(
          xleft = grconvertX(0,'ndc','user'),
          xright = grconvertX(1, 'ndc', 'user'),
          ybottom = grconvertY(0,'ndc','user'),
          ytop = grconvertY(1, 'ndc', 'user'),
          border = NA,
          col = ColorBG,
          xpd = TRUE
        )
        text(0.5, 0.6, ifelse(input$plot_option_switch2 == "hover", "Please move your mouse over the plots", "Please click on the plots"), col = ColorFont)
        text(0.5, 0.4, "to get an enlarged version of the plot!", col = ColorFont)
      }
    } else {
        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          axes = FALSE,
          xlab = "",
          ylab = ""
        )
        rect(
          xleft = grconvertX(0,'ndc','user'),
          xright = grconvertX(1, 'ndc', 'user'),
          ybottom = grconvertY(0,'ndc','user'),
          ytop = grconvertY(1, 'ndc', 'user'),
          border = NA,
          col = ColorBG,
          xpd = TRUE
        )
        text(0.5, 0.6, ifelse(input$plot_option_switch2 == "hover", "Please move your mouse over the plots", "Please click on the plots"), col = ColorFont)
        text(0.5, 0.4, "to get an enlarged version of the plot!", col = ColorFont)
      }
    }, width = data_param()$nvisit * 100)
  })

  #### REFERENCE VALUE BASED PATTERN ####
  output$dendro_3 <- shiny::renderPlot({
    shiny::req(prepare_dist_matrix_for_clustering(), shiny::isolate(input$clusterMethod))
    if ((startsWith(shiny::isolate(input$clusterMethod), "OLO") | startsWith(shiny::isolate(input$clusterMethod), "GW"))) {
      tmp <- prepare_dist_matrix_for_clustering()
      ser <- seriation::seriate(
        elaborator_calculate_spearman_distance(tmp),
        method = shiny::isolate(input$clusterMethod)
      )
      asdendro <- stats::as.dendrogram(ser[[1]])
      dendro3 <- dendextend::assign_values_to_leaves_edgePar(dend = asdendro)

      graphics::rect(
        xleft = graphics::grconvertX(0,'ndc','user'),
        xright = graphics::grconvertX(1,'ndc','user'),
        ybottom = graphics::grconvertY(0,'ndc','user'),
        ytop = graphics::grconvertY(1,'ndc','user'),
        border = NA,
        col = ColorBG,
        xpd = TRUE
      )
      on_ex <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(on_ex))
      graphics::par(bg = ColorBG)
      graphics::plot(dendro3, ylab = "Distance", horiz = FALSE)
    }
  })


  output$hoverpanel3 <- shiny::renderUI({
    shiny::absolutePanel(
      id = "hoverpanel3",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0("<div style='background-color:", ColorBG, "'>")),
      HTML('<button style="background: #f6ad82; color:#ffffff", data-toggle="collapse" data-target="#demo3"> <i class="fa-solid fa-search-plus"></i> Open/Close Zoom Panel</button>'),
      top = 70,
      left = "auto",
      right = 100,
      bottom = "auto",
      width = 400,
      height = "auto",
      tags$div(
        id = 'demo3',
        class = "collapse",
        shiny::fluidRow(
           shiny::column(2,
             shiny::plotOutput('hover3')
           )
          ),
          shiny::fluidRow(
            shiny::column(12, offset = 4,
              shiny::radioButtons(
                inputId = "plot_option_switch3",
                label = NULL,
                choices = c("hover", "click"),
                selected = c("hover"),
                inline = TRUE
              )
            )
          ),
         shiny::fluidRow(
           shiny::column(12,
             uiOutput('hover_info_text3')
           )
         )
      ),
      style = "z-index: 10;"
    )
  })

  output$hover3 <- shiny::renderPlot({
    input$apply_ref_plot
    shiny::req(data_with_selected_factor_levels(), input$abnormal_values_factor, input$plot_option_switch3)

    # switch between hover or click options for zoom panel
    if (input$plot_option_switch3 == "hover") {
      plot_coords <- input$dist_hover3
    } else if (input$plot_option_switch3 == "click") {
      plot_coords <- input$dist_click3
    }

    if (!is.null(plot_coords$coords_css$y) & !is.null(plot_coords$coords_css$x)) {


      if (plot_coords$coords_css$y > 0 & plot_coords$coords_css$x > 0
        & !is.na(input$abnormal_values_factor) & input$abnormal_values_factor >=0) {

      dat <- data_with_only_non_missings_over_visits()

      dat <-  subset(dat,!(dat$LBORNRLO == "" & dat$LBORNRHI == ""))

      dat$LBTESTCD <- factor(dat$LBTESTCD)

       sorti <- levels(dat$LBTESTCD)[levels(dat$LBTESTCD) %in% unique(dat$LBTESTCD)]
      dat_filt <- dat[dat$TRTP == levels(dat$TRTP)[ceiling(plot_coords$coords_css$y/input$zoompx)] & dat$LBTESTCD == sorti[ceiling(plot_coords$coords_css$x / input$zoompx)] ,]


      dat_filt$TRTP <- factor(dat_filt$TRTP)

      cex <- shiny::isolate(input$cex.rvbp)
      crit <- input$criterion

      elaborator_plot_ref_pattern(
        data = dat_filt,
        fontsize = 2,
        criterion = crit,
        sorting_vector = sorti[ceiling(plot_coords$coords_css$x / input$zoompx)],
        abnormal_value_factor = shiny::isolate(input$abnormal_values_factor)
      )

    } else {
      plot(
        NULL,
        xlim = c(0, 1),
        ylim = c(0, 1),
        axes = FALSE,
        xlab = "",
        ylab = ""
      )
      rect(
        xleft = grconvertX(0, 'ndc', 'user'),
        xright = grconvertX(1, 'ndc', 'user'),
        ybottom = grconvertY(0, 'ndc', 'user'),
        ytop = grconvertY(1, 'ndc', 'user'),
        border = NA,
        col = ColorBG,
        xpd = TRUE
      )
      text(0.5, 0.6, ifelse(input$plot_option_switch3 == "hover", "Please move your mouse over the plots", "Please click on the plots"), col = ColorFont)
      text(0.5, 0.4, "to get an enlarged version of the plot!", col = ColorFont)
    }
    } else {
      plot(
        NULL,
        xlim = c(0, 1),
        ylim = c(0, 1),
        axes = FALSE,
        xlab = "",
        ylab = ""
      )
      rect(
        xleft = grconvertX(0, 'ndc', 'user'),
        xright = grconvertX(1, 'ndc', 'user'),
        ybottom = grconvertY(0, 'ndc', 'user'),
        ytop = grconvertY(1, 'ndc', 'user'),
        border = NA,
        col = ColorBG,
        xpd = TRUE
      )
      text(0.5, 0.6, ifelse(input$plot_option_switch3 == "hover", "Please move your mouse over the plots", "Please click on the plots"), col = ColorFont)
      text(0.5, 0.4, "to get an enlarged version of the plot!", col = ColorFont)

    }
  }, width = 400)

  output$prev.pal1 <- shiny::renderPlot({
    col <- c('white', colChoice[[shiny::req(input$select.pal1)]]$col, 'black')
    elaborator_draw_scheme_preview(x = col)
  })


  #### REACTIVE OBJECTS ####
  #### reactiveValues ####
  options(shiny.maxRequestSize = 50 * 1024 ^ 2)

  start <- shiny::reactiveValues(dat = FALSE)

  start.ai <- shiny::reactiveValues(dat = FALSE)

  method <- shiny::reactiveValues(val = "InQuRa")


  # new version of values$default
  statistical_test_results <- shiny::reactiveValues(var = NULL)

  values <- shiny::reactiveValues(default = 0)

  #### reactive ####
  output$flag <- shiny::reactive(start$dat)

  shiny::outputOptions(output, "flag", suspendWhenHidden = FALSE)

  output$ai <- shiny::reactive(start.ai$dat)

  shiny::outputOptions(output, "ai", suspendWhenHidden = FALSE)

  output$check <- shiny::reactive({
    length(input$trtcompar)
  })

  shiny::outputOptions(output, 'check', suspendWhenHidden = FALSE)

  #currently not in use:
  app_input <- shiny::reactive({
    NULL
  })

  data_param <- shiny::reactive({
    shiny::req(data_with_selected_factor_levels())
    ntreat <- length(unique(data_with_only_non_missings_over_visits()$TRTP))
    nvisit <- length(unique(data_with_only_non_missings_over_visits()$AVISIT))
    nlab <- length(unique(data_with_only_non_missings_over_visits()$LBTESTCD))
    tmp <- data_with_only_non_missings_over_visits()
    tmp <- subset(tmp,!(tmp$LBORNRLO == "" & tmp$LBORNRHI == ""))
    nlab2 <- length(unique(tmp$LBTESTCD))

    list(
      ntreat = ntreat,
      nvisit = nvisit,
      nlab = nlab,
      nlab2 = nlab2
    )
  })

  Summary_for_qualitative_trends <-  shiny::reactive({
    shiny::req(data_with_selected_factor_levels(), input$percent)
    dat1 <- data_with_selected_factor_levels()

    percent <- input$percent/100
    firstVisit <- dat1 %>%
      dplyr::pull(AVISIT)%>%
      levels() %>%
      .[1]
    Yall <- dat1 %>%
      tidyr::spread(AVISIT,LBORRES) %>%
      dplyr::select(
        c(LBTESTCD, LBORNRLO, LBORNRHI,
          SUBJIDN, TRTP, LBTESTCD, firstVisit
        )
      )
    lowquant <- highquant <- NULL
    Summa <- Yall %>%
      dplyr::group_by(LBTESTCD) %>%
      dplyr::summarise(
        lowquant = stats::quantile(!!rlang::sym(firstVisit), na.rm = TRUE, probs = 0.25),
        highquant = stats::quantile(!!rlang::sym(firstVisit), na.rm = TRUE, probs = 0.75),
        max = max(!!rlang::sym(firstVisit), na.rm = TRUE),
        min = min(!!rlang::sym(firstVisit), na.rm = TRUE),
        highref = mean(as.numeric(LBORNRHI),na.rm = TRUE),
        lowref = mean(as.numeric(LBORNRLO), na.rm = TRUE)
      ) %>%
      dplyr::mutate(
        InQuRa = percent * (highquant - lowquant),
        Range = percent * (max - min),
        refRange = percent * (highref - lowref)
      ) %>%
      dplyr::select(LBTESTCD, InQuRa, Range, refRange) %>%
      dplyr::rename(variable = LBTESTCD)
    Summa
  })

  trtcompar_val <- shiny::reactive({
    shiny::req(data_with_selected_factor_levels())
    choices  <- as.character(unique(data_with_selected_factor_levels()$AVISIT))
    choices
  })

  #### eventReactive ####
  tcomp <- shiny::eventReactive(c(input$go_select2), {
    input$trtcompar
  })

  box_col <- shiny::eventReactive(input$go, {
    shiny::req(input$select.visit)
    visits <- input$select.xvisit
    selected <- input$trtcompar
    b.col <- c(
      input$'id1-col', input$'id2-col', input$'id3-col', input$'id4-col', input$'id5-col', input$'id6-col',
      input$'id7-col', input$'id8-col', input$'id9-col', input$'id10-col', input$'id11-col', input$'id12-col',
      input$'id13-col', input$'id14-col', input$'id15-col', input$'id16-col', input$'id17-col', input$'id18-col',
      input$'id19-col', input$'id20-col'
    )

    if (!is.null(b.col)) {
      b.col[b.col == "Color1"] <- colBoxplot1
      b.col[b.col == "Color2"] <- colBoxplot2
      b.col[b.col == "Color3"] <- colBoxplot3
      b.col[b.col == "Color4"] <- colBoxplot4
    }

    if ({input$stattest != "none"})
      b.col[!(visits %in% selected)] <- elaborator_transform_transparent(b.col[!(visits %in% selected)], 70)
    b.col
  })

  border.col <-shiny::eventReactive(c(input$go_select2), {
    choices <- input$select.visit
    selected <- input$trtcompar
    col <- rep(elaborator_transform_transparent("black", alpha = 70), length(choices))
    col[choices %in% selected] <- "black"
    col
  })

  #### OBSERVERS ####

  #### observe ####
  shiny::observe({
    input$'id1-col'
    input$'id2-col'
    input$'id3-col'
    input$'id4-col'
    input$'id5-col'
    input$'id6-col'
    input$'id7-col'
    input$'id8-col'
    input$'id9-col'
    input$'id10-col'
    input$'id11-col'
    input$'id12-col'
    input$'id13-col'
    input$'id14-col'
    input$'id15-col'
    input$'id16-col'
    input$'id17-col'
    input$'id18-col'
    input$'id19-col'
    input$'id20-col'
    purrr::map(
      paste0("id", 1:data_param()$nvisit),
      ~ shiny::callModule(
        boxPlotColor,
        id = .x, c("Color1", "Color2", "Color3", "Color4"),
        paste0(
          "Select Color for '",
          input$select.visit[as.numeric(substr(.x,3,nchar(.x)))],
          "'"
        ),
        start_color = ifelse(is.null(input[[paste0("id",as.numeric(substr(.x,3,nchar(.x))),"-col")]]),
                             paste0("Color", 2),
                             input[[paste0("id",as.numeric(substr(.x, 3, nchar(.x))),"-col")]]
        ),
        number = as.numeric(substr(.x,3,nchar(.x)))
      )
    )
  })


  #### observeEvent ####

  shiny::observeEvent(input$apply_ref_plot, {
    if (input$apply_ref_plot >= 1) {

      output$inoutPlot <- shiny::renderPlot({

        dat <- shiny::isolate(data_with_only_non_missings_over_visits())

        dat <- subset(dat,!(dat$LBORNRLO == "" & dat$LBORNRHI == ""))

        cex <- shiny::isolate(input$cex.rvbp)
        crit <- shiny::isolate(input$criterion)

        elaborator_plot_ref_pattern(
          data = dat,
          fontsize = cex,
          criterion = crit,
          sorting_vector = levels(dat$LBTESTCD),
          abnormal_value_factor = shiny::isolate(input$abnormal_values_factor)
        )
      }, res = input$zoompx / 3)

      output$tab3 <- shiny::renderUI({
        shiny::req(data_param())

        hpx <- data_param()$ntreat

        wpx <- data_param()$nlab2
        zoompx <- input$zoompx
        # panelheight <- input$panelheight
        panelheight <- shiny::isolate(input$panelheight)

        shiny::wellPanel(style = paste0("background: ", ColorBG, ";overflow-x:scroll; max-height:", panelheight, "px"),
           shiny::plotOutput(
           outputId = 'inoutPlot',
           height = paste0(hpx * zoompx, 'px'),
           width = paste0(wpx * zoompx, 'px'),
           hover = clickOpts(
             "dist_hover3",
             clip = FALSE
           ),
           click = clickOpts(
             "dist_click3",
             clip = FALSE
           )
          )
        )
      })
    }
  })


  shiny::observeEvent(input$apply_qual_plot, {
    #requirements
    shiny::req(isolate(data_with_selected_factor_levels()))
    if(input$apply_qual_plot > 0) {
      #output of Qualitative trend plots
      output$trendPlot <- shiny::renderPlot({
        shiny::req(isolate(Summary_for_qualitative_trends()))

        dat <- shiny::isolate(data_with_only_non_missings_over_visits())

        cex <- shiny::isolate(input$cex.trend)
        Variab <- levels(dat$LBTESTCD)[levels(dat$LBTESTCD) %in% unique(dat$LBTESTCD)]

        meth <- shiny::isolate(input$method)
        Summa  <- shiny::isolate(Summary_for_qualitative_trends())
        elaborator_plot_qual_trends(
          dat1 = dat,
          Variab,
          fontsize = cex,
          method = meth,
          color_palette = c('white', colChoice[[shiny::req(isolate(input$select.pal1))]]$col, 'black'),
          Summa = Summa
        )
      }, res = isolate(input$zoompx) / 3)

      output$tab2 <- shiny::renderUI({
        shiny::req(shiny::isolate(data_param()))
        hpx <- shiny::isolate(data_param()$ntreat)
        wpx <- shiny::isolate(data_param()$nlab)

        zoompx <- shiny::isolate(input$zoompx)
        panelheight <- shiny::isolate(input$panelheight)

        shiny::wellPanel(
          style = paste0("background: ", ColorBG, ";overflow-x:scroll; max-height:", panelheight, "px"),
          shiny::plotOutput(
            outputId = 'trendPlot',
            height = paste0(hpx * zoompx, 'px'),
            width = paste0(wpx * zoompx, 'px'),
            hover = clickOpts(
               "dist_hover2",
               clip = FALSE
             ),
             click = clickOpts("dist_click2",
               clip = FALSE
             )
          )
        )
      })
    }
  })

  shiny::observeEvent(input$select.lab, {
    if(length(input$select.lab) <= length(input$arrange.lab)) {
      tmp <- input$arrange.lab[input$arrange.lab %in% input$select.lab]
    } else {
      tmp <- c(input$arrange.lab, input$select.lab[!input$select.lab %in% input$arrange.lab])
    }
    shiny::updateSelectizeInput(
      session,
      inputId = "arrange.lab",
      choices = tmp,
      selected = tmp
    )
  })
  #### Picker/Selectize Inputs ####

  ### bug fix filter update

  shiny::observeEvent(filtered_raw_data(), {

    choices_sel_lab <- unique(filtered_raw_data()$LBTESTCD)
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select.lab",
      choices = choices_sel_lab,
      selected = choices_sel_lab
    )

    choices_sel_visit <- unique(filtered_raw_data()$AVISIT)

    shiny::updateSelectizeInput(
      session,
      inputId = "select.visit",
      choices = choices_sel_visit,
      selected = choices_sel_visit
    )

    choices_sel_lab <- unique(filtered_raw_data()$LBTESTCD)

    shiny::updateSelectizeInput(
      session,
      inputId = "arrange.lab",
      choices = choices_sel_lab,
      selected = choices_sel_lab
    )

    choices_sel_treatments <- unique(filtered_raw_data()$TRTP)

    shiny::updateSelectizeInput(
      session,
      inputId = "select.treatments",
      choices = choices_sel_treatments,
      selected = choices_sel_treatments
    )
  })

 shiny::observeEvent(input$con_lin_options, {
   if (input$con_lin_options == 'custom_visits') {
    choices  <- input$select.visit
    selected <- c(choices[1], choices[length(choices)])
    shiny::updateCheckboxGroupInput(
      session,
      inputId = "custom_visits",
      choices = choices,
      selected = selected
    )
   }
 })

  shiny::observeEvent(input$select.visit, {
    choices  <- input$select.visit
    selected <- c(choices[1], choices[length(choices)])
    shiny::updateCheckboxGroupInput(
      session,
      inputId = "trtcompar",
      choices = choices,
      selected = selected
    )
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select.ai.first",
      choices = choices,
      selected = choices[1]
    )
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select.ai.last",
      choices = choices,
      selected = choices[length(choices)]
    )
  })

  #### QUALITATIVE TREND ####
  #### output of Qualitative trend plots ###
  # update when Update/Create button is clicked

  shiny::observeEvent(input$apply_quant_plot, {
    # requirements
    shiny::req(data_with_selected_factor_levels())
    # button need to be clicked at least once
    if (shiny::isolate(input$apply_quant_plot) > 0) {
      #select data  with non-missings for all visits
      dat <- shiny::isolate(data_with_only_non_missings_over_visits())

      #load statistical test values (saved in values$default)
      val <- shiny::isolate(values$default)
      if (!is.list(val)) {
        info <- NA
      } else {
        info <- shiny::isolate(values$default)
      }
      #replace values$default with newer version
      #load statistical test values (saved in statistical_test_resulst$var)

      if (shiny::isolate(input$go) != 0) {
        b.col <- shiny::isolate(box_col())
      } else {
        b.col <- c(colBoxplot2, colBoxplot2, colBoxplot2, colBoxplot2)
      }
      if (shiny::isolate(input$stattest) != "none") {
        bordcol <- shiny::isolate(border.col())
      } else {
        bordcol <- NULL
      }
      if(input$con_lin){
       lines_data <- shiny::isolate(quant_plot_data_lines())
      } else {
        lines_data <- NULL
      }
      #renderPlot created by elaborator_plot_quant_trends()-function
      output$compl <- shiny::renderPlot({
        elaborator_plot_quant_trends(
          shiny::isolate(data_with_only_non_missings_over_visits()),
          signtest = ifelse(shiny::isolate(input$stattest) == "signtest", TRUE, FALSE),
          Visit1 = shiny::isolate(input$trtcompar)[1],
          Visit2 = shiny::isolate(input$trtcompar)[-1],
          labcolumn = "LBTESTCD",
          cols = b.col,
          pcutoff = shiny::isolate(input$pcutoff),
          sameaxes = shiny::isolate(input$sameaxes),
          sortpoints = shiny::isolate(input$sortpoint),
          labelvis = NULL,
          infotest = shiny::isolate(statistical_test_results$var),
          sortinput = levels(shiny::isolate(dat$LBTESTCD)),
          bordercol = bordcol,
          add_points = shiny::isolate(input$add_points),
          connect_lines = shiny::isolate(input$con_lin),
          lin_data = lines_data,
          outliers = shiny::isolate(input$outlier),
          tolerated_percentage = shiny::isolate(input$select.toleratedPercentage),
          color_lines_options = shiny::isolate(input$con_lin_options),
          custom_visits = shiny::isolate(input$custom_visits)
        )
      }, res = shiny::isolate(input$zoompx) / 3
      )


      #Create a plot as y-label for graph
      output$treatment_label_panel <- shiny::renderPlot({
        elaborator_plot_quant_trends_treatment_label(
          dat1 = dat
        )
      })

      output$tab1 <- shiny::renderUI({
        shiny::req(isolate(data_param()))
        hpx <- shiny::isolate(data_param()$ntreat)
        wpx <- shiny::isolate(data_param()$nlab)
        zoompx <- shiny::isolate(input$zoompx)
        panelheight <- shiny::isolate(input$panelheight)
        shiny::fluidRow(
          shiny::column(12,
            shiny::wellPanel(style = paste0("background: ", ColorBG, ";overflow-x:scroll; max-height:", panelheight,"px"),
              shiny::plotOutput(
               outputId = 'compl',
               height = paste0(hpx * zoompx,'px'),
               width = paste0(wpx * zoompx, 'px'),
               hover = clickOpts(
                 "dist_hover",
                 clip = FALSE
               ),
               click = clickOpts("dist_click",
                 clip = FALSE
               )
              )
            )
          )
        )
      })
    }
  })

  #### Update Actionbuttons ####

  shiny::observeEvent(data_param(), {
    shiny::updateActionButton(
      session,
      inputId = "apply_qual_plot",
      label = paste0('Create/Update ', data_param()$nlab*data_param()$ntreat,' graphs')
    )
    shiny::updateActionButton(
      session,
      inputId = "apply_quant_plot",
      label = paste0('Create/Update ', data_param()$nlab*data_param()$ntreat,' graphs')
    )
    shiny::updateActionButton(
      session,
      inputId = "apply_ref_plot",
      label = paste0('Create/Update ', data_param()$nlab2*data_param()$ntreat,' graphs')
    )
  })

  shiny::observeEvent(input$link_to_pdf_view, {
    output$pdfview <- shiny::renderUI({
      tags$iframe(
        style = "height:500px; width:100%",
        src = "www/Seriation_methods_20191115.pdf"
      )
    })
  })

  shiny::observeEvent(raw_data_and_warnings()$data, {
    start$dat <- TRUE
  })

  shiny::observeEvent(input$go3, {
    if (shiny::isolate(input$orderinglab) == "auto") {
      if (!is.null(shiny::isolate(input$clusterMethod))) {
        if ((startsWith(shiny::isolate(input$clusterMethod), "OLO") | startsWith(shiny::isolate(input$clusterMethod), "GW"))) {
          start.ai$dat <- TRUE
        } else {
          start.ai$dat <- FALSE
        }
      } else {
        start.ai$dat <- FALSE
      }
    } else {
      start.ai$dat <- FALSE
    }
  })

  shiny::observeEvent(c(input$go_select2), {
    shiny::req(
      data_with_selected_factor_levels(),
      input$trtcompar,
      input$stattest,
      input$select.treatments,
      shiny::isolate(input$select.lab),
      input$select.visit
    )

    dat <- data_with_selected_factor_levels()
    T1 <- input$trtcompar[1]
    T2 <- input$trtcompar[-1]
    signtest <- input$stattest

    if (input$stattest == "signtest" | input$stattest == "ttest") {
      statistical_test_results$var <- elaborator_calculate_test_for_all_visits(
          elab_data = dat,
          Visit1 = T1,
          Visit2 = T2,
          sign_test = input$stattest,
          pcutoff = shiny::isolate(input$pcutoff)
      )

    } else {
      statistical_test_results$var <- NULL
    }

    # if (input$stattest == "signtest" && length(input$trtcompar) >= 2 && length(unique(dat$AVISIT)) >= 2) {
    #   values$default <- elaborator_derive_test_values(
    #     data = dat,
    #     signtest = TRUE,
    #     Visit1 = T1,
    #     Visit2 = T2,
    #     lab_column = "LBTESTCD"
    #   )
    #
    # } else if (input$stattest== "ttest" && length(input$trtcompar) >= 2 && length(unique(dat$AVISIT)) >= 2) {
    #   values$default <- elaborator_derive_test_values(
    #     data = dat,
    #     signtest = FALSE,
    #     Visit1 = T1,
    #     Visit2 = T2,
    #     lab_column = "LBTESTCD"
    #   )
    # } else {
    #   values$default <- NA
    # }
  })

  shiny::observeEvent(input$link_to_tab_info, {
    shinydashboard::updateTabItems(session, "sidebarmenu", "helptext")
  })

  shiny::observeEvent(input$link_to_structure_info, {
    shinydashboard::updateTabItems(session, "sidebarmenu", "datamanual")
  })

  # change color of the Create/Upload Plots Buttons
  output$cont1 <- shiny::renderUI({
    list(
      shiny::tags$head(
        tags$style(HTML('#apply_quant_plot{color: #ffffff; background-color:#47d2bc;
          border-color: #f78300}'))
      )
    )
  })

  output$cont1_text <- shiny::renderUI({
    HTML(paste0("<b style='color: #47d2bc; border-color: #f78300'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"))
  })

  observeEvent(input$apply_quant_plot, {
    output$cont1 <- shiny::renderUI({
      list(
        shiny::tags$head(
          tags$style(HTML('#apply_quant_plot{color: #ffffff; background-color:#e3e3e3;
            border-color: #ffffff}'))
        )
      )
    })
    output$cont1_text <- shiny::renderUI({
      HTML("")
    })
  })

  shiny::observeEvent(
    c(input$sameaxes,
      input$add_points,
      input$con_lin,
      input$go_select2,
      input$select.visit,
      input$select.treatments,
      input$select.lab,
      input$select.toleratedPercentage,
      input$go3,
      input$sortpoint,
      input$zoompx,
      input$con_lin_options,
      input$custom_visists,
      input$outlier
    ), {
        output$cont1 <- shiny::renderUI({
          list(
            shiny::tags$head(
              tags$style(HTML('#apply_quant_plot{color: #ffffff; background-color:#47d2bc;
            border-color: #f78300}'))
            )
          )
        })

        output$cont1_text <- shiny::renderUI({
          HTML(paste0("<b style='color: #47d2bc;border-color: #f78300'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"))
        })
      })

  output$cont2 <- shiny::renderUI({
    list(
      shiny::tags$head(
        tags$style(HTML('#apply_qual_plot{color: #ffffff; background-color:#47d2bc;
          border-color: #f78300}'))
      )
    )
  })

  output$cont2_text <- shiny::renderUI({
    HTML(paste0("<b style='color: #47d2bc; border-color: #f78300'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"))
  })

  observeEvent(input$apply_qual_plot, {
    output$cont2 <- shiny::renderUI({
      list(
        shiny::tags$head(
          tags$style(HTML('#apply_qual_plot{color: #ffffff; background-color:#e3e3e3;
            border-color: #ffffff}'))
        )
      )
    })
    output$cont2_text <- shiny::renderUI({
      HTML("")
    })
  })

  shiny::observeEvent(
    c(input$cex.trend, input$method, input$percent,
      input$select.visit, input$select.pal1,
      input$select.treatments, input$select.lab, input$select.toleratedPercentage,
      input$go3), {
        output$cont2 <- shiny::renderUI({
          list(
            shiny::tags$head(
              tags$style(HTML('#apply_qual_plot{color: #ffffff; background-color:#47d2bc;
            border-color: #f78300}'))
            )
          )
        })

        output$cont2_text <- shiny::renderUI({
          HTML(paste0("<b style='color: #47d2bc;'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"))
        })
      })

  output$cont3 <- shiny::renderUI({
    list(
      shiny::tags$head(
        tags$style(HTML('#apply_ref_plot{color: #ffffff; background-color:#47d2bc;
          border-color: #f78300}'))
      )
    )
  })

  output$cont3_text <- shiny::renderUI({
    HTML(paste0("<b style='color: #47d2bc;'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"))
  })

  observeEvent(input$apply_ref_plot, {
    output$cont3 <- shiny::renderUI({
      list(
        shiny::tags$head(
          tags$style(HTML('#apply_ref_plot{color: #ffffff; background-color:#e3e3e3;
            border-color: #ffffff}'))
        )
      )
    })
    output$cont3_text <- shiny::renderUI({
      HTML("")
    })
  })

  shiny::observeEvent(
    c(input$cex.rvbp, input$criterion, input$abnormal_values_factor,
      input$select.visit,
      input$select.treatments, input$select.lab, input$select.toleratedPercentage,
      input$go3), {
        output$cont3 <- shiny::renderUI({
          list(
            shiny::tags$head(
              tags$style(HTML('#apply_ref_plot{color: #ffffff; background-color:#47d2bc;
            border-color: #f78300}'))
            )
          )
        })

        output$cont3_text <- shiny::renderUI({
          HTML(paste0("<b style='color: #47d2bc;'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"))
        })
      })


  #### FILTER ####
  # Reset initial values if Remove Button is clicked
  shiny::observeEvent(input$removeBtn, {
    id_elab_m$myList <- list()
    id_elab_m$myList2 <- list()
  })

  # Delete UI Elements if Remove Button is clicked
  shiny::observeEvent(input$removeBtn, {
    for (i in 1:length(inserted_elab)) {
      removeUI(selector = paste0("#", inserted_elab[i]))
    }
  })

    output$filter_percentage <- shiny::renderUI({
      total_tmp <- dim(raw_data_and_warnings()$data)[1]
      value_tmp <- dim(filtered_raw_data())[1]
      shinyWidgets::progressBar(
        id = "filter_percentage",
        value = value_tmp,
        total = total_tmp,
        title = "",
        display_pct = TRUE
      )
    })

    output$pickerinput_filter <- shiny::renderUI({
    shiny::req(raw_data_and_warnings())

    dat <- raw_data_and_warnings()$data

    data_variables_tmp <- purrr::map(
      dat,
      function(x) attr(x, "label", exact = TRUE)
    )
    data_variables = names(data_variables_tmp)
    names(data_variables) = paste0(
      names(data_variables_tmp),
      ifelse(
        as.character(data_variables_tmp) == "NULL",
        "",
        paste0(" - ", as.character(data_variables_tmp))
      )
    )

    choices <- data_variables

    shinyWidgets::pickerInput(
      inputId = 'pickerinput_filter',
      label = 'Select filter variable(s) for elaborator data set',
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
 inserted_elab <- c()

  id_elab_nr <- c()
  id_elab_nr2 <- c()

  id_elab_m <- shiny::reactiveValues()
  id_elab_m$myList <- list()
  id_elab_m$myList2 <- list()
  inserted_elab_list <- shiny::reactive({
    list()
  })


   shiny::observeEvent(c(input$insertBtn), {

    shiny::req(raw_data_and_warnings()$data)

    elab_data <- raw_data_and_warnings()$data

    ins_elab <- inserted_elab_list()
    id_elab_nr <<- c()
    id_elab_nr2 <<- c()

    if (length(inserted_elab) > 0) {
      for (i in 1:length(inserted_elab)) {
        shiny::removeUI(
          ## pass in appropriate div id
          selector = paste0('#', inserted_elab[i])
        )
      }
    }

    inserted_elab <<- c()

    btn <- input$insertBtn

    pickerinput_filter <- input$pickerinput_filter

    if (length(pickerinput_filter) > 0) {
      for (i in 1: length(pickerinput_filter)) {
        id <- paste0(pickerinput_filter[i], btn)
        shiny::insertUI(
          selector = '#placeholder',
          ui = shiny::tags$div(
            if (!is.numeric(elab_data %>%
                           dplyr::pull(pickerinput_filter[i]))) {
              shinyWidgets::pickerInput(
                inputId = id,
                label = paste0(pickerinput_filter[i]),
                choices = elab_data %>%
                  dplyr::pull(pickerinput_filter[i]) %>%
                  unique,
                selected = elab_data %>%
                  dplyr::pull(pickerinput_filter[i]) %>%
                  unique,
                multiple = TRUE,
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
                elab_data %>%
                  dplyr::pull(pickerinput_filter[i])
              ) && !is.integer(
                elab_data %>%
                  dplyr::pull(pickerinput_filter[i])
                )
            ) {
              shiny::sliderInput(
                inputId = id,
                label = paste0(pickerinput_filter[i]),
                value = c(
                  elab_data %>%
                    dplyr::pull(pickerinput_filter[i]) %>%
                    base::min(na.rm = TRUE), elab_data %>%
                    dplyr::pull(pickerinput_filter[i]) %>%
                    base::max(na.rm = TRUE)
                ),
                min = elab_data %>%
                 dplyr::pull(pickerinput_filter[i]) %>%
                 base::min(na.rm = TRUE),
                max = elab_data %>%
                 dplyr::pull(pickerinput_filter[i]) %>%
                 base::max(na.rm = TRUE)
              )
            } else if (
                is.numeric(
                  elab_data %>%
                    dplyr::pull(pickerinput_filter[i])
                ) &&
                is.integer(
                  elab_data %>%
                    dplyr::pull(pickerinput_filter[i])
                )
              ) {
              shiny::sliderInput(
                inputId = id,
                label = paste0(pickerinput_filter[i]),
                value = c(elab_data %>%
                  dplyr::pull(pickerinput_filter[i]) %>%
                  base::min(na.rm = TRUE),elab_data %>%
                  dplyr::pull(pickerinput_filter[i]) %>%
                  base::max(na.rm = TRUE)
                ),
                min = elab_data %>%
                  dplyr::pull(pickerinput_filter[i]) %>%
                  base::min(na.rm = TRUE),
                max = elab_data %>%
                  dplyr::pull(pickerinput_filter[i]) %>%
                  base::max(na.rm = TRUE),
                step = 1,
                sep = "",
                ticks = FALSE
              )
            },
            id = id
          )
        )
        inserted_elab <<- c(id, inserted_elab)
        ins_elab[[pickerinput_filter[i]]]  <- elab_data %>%
          dplyr::pull(pickerinput_filter[i])
        id_elab_nr2 <<- c(id_elab_nr2, pickerinput_filter[[i]])
        id_elab_nr <<- c(id_elab_nr,id)
      }
    }

    id_elab_m$myList2 <- id_elab_nr2
    id_elab_m$myList <- id_elab_nr
  })

  shiny::observe({
    if (file.exists(here::here("data", "elaborator_demo.RData"))) {
      updatePrettyRadioButtons(
        session,
        inputId = "impswitch",
        label = 'Select file format',
        choices = c('*.RData file', '*.CSV file','Demo data'),
        prettyOptions = list(status = "warning")
      )
    }
  })
}
