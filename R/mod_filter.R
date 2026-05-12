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
    shiny::uiOutput(ns("filter_percentage")),
    shiny::uiOutput(ns("pickerinput_filter")),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::actionButton(
          inputId = ns("insertBtn"),
          label = "Add",
          icon = icon("plus")
        )
      ),
      shiny::column(
        4,
        shiny::actionButton(
          inputId = ns("removeBtn"),
          label = "Delete",
          icon = icon("minus")
        )
      )
    ),
    shiny::tags$div(id = ns("placeholder")),
    shiny::actionButton(
      inputId = ns("apply"),
      label = "Apply Filter Selection!",
      icon = icon("redo"),
      class = "redo-button"
    )
  )
}

#' filter Server Functions
#'
#' @noRd
mod_filter_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ns_upload <- shiny::NS("upload_1")
    rs <- session$userData$root
    if (is.null(rs)) {
      rs <- session
    }
    ns_box <- shiny::NS("boxplots_1")
    ns_qual <- shiny::NS("qualitative_1")
    ns_trees <- shiny::NS("trees_1")

    id_elab_m <- shiny::reactiveValues(myList = list(), myList2 = list())

    #### 2. filter by app filter-tab  ####
    # used function:
    # ---
    # purpose:
    # filter data set by filter-tab selection within elaborator app.
    #
    # reactivity triggers :
    # r$raw_data_and_warnings / <filter selection within app>
    raw_data_and_warnings <- shiny::reactive({
      shiny::req(r$raw_data_and_warnings)
      r$raw_data_and_warnings
    })

    filtered_raw_data <- shiny::reactive({
      shiny::req(raw_data_and_warnings()$data)
      elab_data <- raw_data_and_warnings()$data
      data <- elab_data
      if (length(id_elab_m$myList) != 0) {
        if (length(id_elab_m$myList) && !is.null(id_elab_m$myList2)) {
          data_filt <- data
          for (i in seq_along(id_elab_m$myList)) {
            if (
              elab_data %>%
              dplyr::pull(id_elab_m$myList2[i]) %>%
              is.numeric()
            ) {
              if (!is.null(input[[id_elab_m$myList[[i]]]])) {
                data_filt <- data_filt[
                  data_filt %>%
                    dplyr::pull(id_elab_m$myList2[i]) %>%
                    dplyr::between(
                      input[[id_elab_m$myList[[i]]]][1],
                      input[[id_elab_m$myList[[i]]]][2]
                    ),
                ]
              }
            } else {
              data_filt <- data_filt %>%
                dplyr::filter(
                  !!rlang::sym(id_elab_m$myList2[i]) %in%
                    c(input[[id_elab_m$myList[i]]])
                )
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
        visits = r$globals$select.visit,
        treat = r$globals$select.treatments,
        labparameter = r$globals$select.lab
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
      shiny::req(r$globals$select.toleratedPercentage)

      filtered_and_removed_visits <- elaborator_remove_visits_due_tolerated_missings(
        elab_data = filtered_and_reduced_raw_data(),
        tolerated_value = (r$globals$select.toleratedPercentage / 100)
      )
      filtered_and_removed_visits
    })

    ####    5. remove visits due to tolerated percentage missing:####
    # used function: ---
    #
    # purpose: remove visits due to tolerated percentage missing
    #
    #
    # reactivity triggers :
    # data_with_missing_flag()

    data_without_missing_visits <- shiny::reactive({
      shiny::req(data_with_missing_flag())
      filtered_and_removed_visits <- data_with_missing_flag() %>%
        dplyr::filter(.data$visit_removed == FALSE)
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
      shiny::req(
        r$globals$select.treatments,
        r$globals$select.visit,
        data_without_missing_visits(),
        raw_data_and_warnings()
      )
      filtered_data2 <- elaborator_change_class_required_variables(
        elab_data = data_without_missing_visits(),
        visit = r$globals$select.visit,
        treatment = r$globals$select.treatments,
        lab = unique(raw_data_and_warnings()$data$LBTESTCD)
      )
      filtered_data2
    })

    #### preprocess lines for quantitative trends####
    quant_plot_data_lines <- shiny::reactive({
      shiny::req(filtered_and_reduced_raw_data(), data_with_selected_factor_levels())
      tmp <- filtered_and_reduced_raw_data() %>%
        dplyr::group_by(
          .data$TRTP,
          .data$LBTESTCD
        ) %>%
        dplyr::select(.data$TRTP, .data$LBTESTCD, .data$SUBJIDN, .data$AVISIT, .data$LBORRES) %>%
        tidyr::pivot_wider(names_from = .data$AVISIT, values_from = .data$LBORRES) %>%
        dplyr::select(-.data$SUBJIDN)
      # Column names come from actual AVISIT values in the wide table; factor
      # levels can lag or differ after a new upload — only subset columns that exist.
      lv <- levels(data_with_selected_factor_levels()$AVISIT)
      visit_in_tmp <- setdiff(names(tmp), c("TRTP", "LBTESTCD"))
      ordered_visits <- c(intersect(lv, visit_in_tmp), setdiff(visit_in_tmp, lv))
      tmp <- tmp[, c("TRTP", "LBTESTCD", ordered_visits), drop = FALSE]
      tmp
    })

    #### AI Sorting ####
    ####    a1. prepare distance matrix (only if ai sorting is selected) ####
    prepare_dist_matrix_for_clustering <- shiny::eventReactive(c(r$globals$go3), {
      shiny::req(data_filtered_by_app_selection())
      ds <- data_filtered_by_app_selection()
      if (shiny::isolate(r$globals$orderinglab) == "auto") {
        first <- shiny::isolate(r$globals$select.ai.first)
        last <- shiny::isolate(r$globals$select.ai.last)
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
    lab_parameter_order_by_clustering <- shiny::eventReactive(r$globals$go3, {
      shiny::req(shiny::isolate(data_filtered_by_app_selection()))
      tmp2 <- shiny::isolate(prepare_dist_matrix_for_clustering())
      ds <- shiny::isolate(data_filtered_by_app_selection())

      if (r$globals$orderinglab == "asinp") {
        as.character(unique(ds$LBTESTCD))
      } else if (r$globals$orderinglab == "alphabetically") {
        sort(as.character(unique(ds$LBTESTCD)))
      } else if (r$globals$orderinglab == "auto") {
        shiny::req(prepare_dist_matrix_for_clustering())
        ord <- tmp2 %>%
          elaborator_calculate_spearman_distance() %>%
          seriation::seriate(method = r$globals$clusterMethod) %>%
          seriation::get_order()
        rownames(tmp2)[ord]
      } else if (r$globals$orderinglab == "manual") {
        r$globals$arrange.lab
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

    data_with_selected_factor_levels <- shiny::eventReactive(
      c(data_filtered_by_app_selection(), r$globals$go3),
      {
        tmp <- data_filtered_by_app_selection()
        if (shiny::isolate(r$globals$orderinglab) == "asinp") {
          lab_levels <- unique(raw_data_and_warnings()$data$LBTESTCD)
          lab_levels <- lab_levels[lab_levels %in% r$globals$select.lab]
          tmp$LBTESTCD <- factor(tmp$LBTESTCD, levels = lab_levels)
        } else if (shiny::isolate(r$globals$orderinglab) == "alphabetically") {
          lab_levels <- sort(unique(raw_data_and_warnings()$data$LBTESTCD))
          lab_levels <- lab_levels[lab_levels %in% r$globals$select.lab]
          tmp$LBTESTCD <- factor(tmp$LBTESTCD, levels = lab_levels)
        } else if (shiny::isolate(r$globals$orderinglab) == "auto") {
          # Do not isolate(): we need a reactive dependency so this runs *after*
          # lab_parameter_order_by_clustering() updates on the same go3 click.
          lab_levels <- lab_parameter_order_by_clustering()
          lab_levels <- c(
            lab_levels,
            as.character(unique(tmp$LBTESTCD)[which(
              !unique(tmp$LBTESTCD) %in% lab_levels
            )])
          )
          tmp$LBTESTCD <- factor(tmp$LBTESTCD, levels = lab_levels)
        } else if (shiny::isolate(r$globals$orderinglab) == "manual") {
          lab_levels <- r$globals$arrange.lab
          lab_levels <- lab_levels[lab_levels %in% r$globals$select.lab]
          tmp$LBTESTCD <- factor(tmp$LBTESTCD, levels = lab_levels)
        }
        tmp
      }
    )

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
      shiny::req(r$globals$select.visit)
      tmp <- data_with_selected_factor_levels() %>%
        dplyr::full_join(
          data_with_selected_factor_levels() %>%
            dplyr::group_by(.data$TRTP, .data$LBTESTCD) %>%
            dplyr::summarise(
              visits_non_missing = length(unique(.data$AVISIT)),
              .groups = "keep"
            ),
          by = c("TRTP", "LBTESTCD")
        )

      tmp2 <- tmp %>%
        dplyr::right_join(
          tmp %>%
            dplyr::group_by(.data$SUBJIDN, .data$LBTESTCD, .data$TRTP) %>%
            dplyr::summarise(
              # visits_non_missing is constant per (SUBJIDN, LBTESTCD, TRTP) in
              # theory, but duplicate join rows can repeat it — collapse to scalar.
              visits_expected = dplyr::first(
                dplyr::coalesce(as.numeric(.data$visits_non_missing), 0)
              ),
              non_missing_values = sum(!is.na(.data$LBORRES)),
              all_complete = .data$non_missing_values == .data$visits_expected,
              .groups = "keep"
            ) %>%
            dplyr::ungroup() %>%
            dplyr::select(.data$SUBJIDN, .data$LBTESTCD, .data$TRTP, .data$all_complete) %>%
            dplyr::distinct(),
          by = c("SUBJIDN", "LBTESTCD", "TRTP")
        ) %>%
        dplyr::filter(.data$all_complete == TRUE)
      tmp2
    })

    #### Reactive: layout dimensions for plot panels (data_param) ####
    data_param <- shiny::reactive({
      shiny::req(data_with_selected_factor_levels())
      ntreat <- length(unique(data_with_only_non_missings_over_visits()$TRTP))
      nvisit <- length(unique(data_with_only_non_missings_over_visits()$AVISIT))
      nlab <- length(unique(data_with_only_non_missings_over_visits()$LBTESTCD))
      tmp <- data_with_only_non_missings_over_visits()
      tmp <- subset(tmp, !(tmp$LBORNRLO == "" & tmp$LBORNRHI == ""))
      nlab2 <- length(unique(tmp$LBTESTCD))

      list(
        ntreat = ntreat,
        nvisit = nvisit,
        nlab = nlab,
        nlab2 = nlab2
      )
    })

    #### QUALITATIVE TREND ####
    #### Summary for qualitative trends (InQuRa / Range / refRange) ####
    Summary_for_qualitative_trends <- shiny::reactive({
      shiny::req(data_with_selected_factor_levels(), r$globals$percent)
      dat1 <- data_with_selected_factor_levels()

      percent <- r$globals$percent / 100
      lv <- levels(dplyr::pull(dat1, "AVISIT"))
      Yall <- dat1 %>%
        tidyr::spread(!!rlang::sym("AVISIT"), !!rlang::sym("LBORRES"))
      # After spread, columns are named by values present in data; factor levels
      # may not all exist as columns (e.g. after switching datasets).
      firstVisit <- lv[lv %in% names(Yall)][1L]
      if (is.na(firstVisit)) {
        meta <- c("LBTESTCD", "LBORNRLO", "LBORNRHI", "SUBJIDN", "TRTP")
        cand <- setdiff(names(Yall), meta)
        shiny::req(length(cand) > 0L)
        firstVisit <- cand[[1L]]
      }
      need_cols <- c("LBTESTCD", "LBORNRLO", "LBORNRHI", "SUBJIDN", "TRTP", firstVisit)
      Yall <- Yall %>%
        dplyr::select(dplyr::any_of(need_cols))
      Summa <- Yall %>%
        dplyr::group_by(.data$LBTESTCD) %>%
        dplyr::summarise(
          lowquant = stats::quantile(
            !!rlang::sym(firstVisit),
            na.rm = TRUE,
            probs = 0.25
          ),
          highquant = stats::quantile(
            !!rlang::sym(firstVisit),
            na.rm = TRUE,
            probs = 0.75
          ),
          max = max(!!rlang::sym(firstVisit), na.rm = TRUE),
          min = min(!!rlang::sym(firstVisit), na.rm = TRUE),
          highref = mean(as.numeric(.data$LBORNRHI), na.rm = TRUE),
          lowref = mean(as.numeric(.data$LBORNRLO), na.rm = TRUE)
        ) %>%
        dplyr::mutate(
          InQuRa = percent * (.data$highquant - .data$lowquant),
          Range = percent * (.data$max - .data$min),
          refRange = percent * (.data$highref - .data$lowref)
        ) %>%
        dplyr::select(variable = .data$LBTESTCD, .data$InQuRa, .data$Range, .data$refRange)
      Summa
    })

    #### Visit choices for treatment comparison (trtcompar_val) ####
    trtcompar_val <- shiny::reactive({
      shiny::req(data_with_selected_factor_levels())
      as.character(unique(data_with_selected_factor_levels()$AVISIT))
    })

    inserted_elab <- shiny::reactiveVal(character())

    #### FILTER ####
    # Reset initial values and remove dynamic filter UI when Remove is clicked;
    # build filter percentage, picker, and insert-variable UI.

    shiny::observeEvent(input$removeBtn, {
      id_elab_m$myList <- list()
      id_elab_m$myList2 <- list()
      for (i in seq_along(inserted_elab())) {
        shiny::removeUI(selector = paste0("#", inserted_elab()[i]))
      }
      inserted_elab(character())
    })

    output$filter_percentage <- shiny::renderUI({
      total_tmp <- dim(raw_data_and_warnings()$data)[1]
      value_tmp <- dim(filtered_raw_data())[1]
      shinyWidgets::progressBar(
        id = ns("filter_percentage"),
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
      data_variables <- names(data_variables_tmp)
      names(data_variables) <- paste0(
        names(data_variables_tmp),
        ifelse(
          as.character(data_variables_tmp) == "NULL",
          "",
          paste0(" - ", as.character(data_variables_tmp))
        )
      )

      choices <- data_variables

      shinyWidgets::pickerInput(
        inputId = ns("pickerinput_filter"),
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

    shiny::observeEvent(c(input$insertBtn), {
      shiny::req(raw_data_and_warnings()$data)

      elab_data <- raw_data_and_warnings()$data

      if (length(inserted_elab()) > 0) {
        for (i in seq_along(inserted_elab())) {
          shiny::removeUI(selector = paste0('#', inserted_elab()[i]))
        }
      }

      btn <- input$insertBtn
      pickerinput_filter <- input$pickerinput_filter

      id_elab_nr <- character()
      id_elab_nr2 <- character()
      new_inserted <- character()

      if (length(pickerinput_filter) > 0) {
        for (i in seq_along(pickerinput_filter)) {
          id <- paste0(pickerinput_filter[i], btn)
          shiny::insertUI(
            selector = paste0("#", ns("placeholder")),
            ui = shiny::tags$div(
              if (
                !is.numeric(
                  elab_data %>%
                  dplyr::pull(pickerinput_filter[i])
                )
              ) {
                shinyWidgets::pickerInput(
                  inputId = id,
                  label = paste0(pickerinput_filter[i]),
                  choices = elab_data %>%
                    dplyr::pull(pickerinput_filter[i]) %>%
                    unique(),
                  selected = elab_data %>%
                    dplyr::pull(pickerinput_filter[i]) %>%
                    unique(),
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
                ) &&
                !is.integer(
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
                      base::min(na.rm = TRUE),
                    elab_data %>%
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
                  value = c(
                    elab_data %>%
                      dplyr::pull(pickerinput_filter[i]) %>%
                      base::min(na.rm = TRUE),
                    elab_data %>%
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
          new_inserted <- c(id, new_inserted)
          id_elab_nr2 <- c(id_elab_nr2, pickerinput_filter[[i]])
          id_elab_nr <- c(id_elab_nr, id)
        }
      }

      id_elab_m$myList2 <- id_elab_nr2
      id_elab_m$myList <- id_elab_nr
      inserted_elab(new_inserted      )
    })

    #### Picker/Selectize Inputs ####

    ### bug fix filter update

    shiny::observeEvent(filtered_raw_data(), {
      choices_sel_lab <- unique(filtered_raw_data()$LBTESTCD)
      shinyWidgets::updatePickerInput(
        rs,
        inputId = ns_upload("select.lab"),
        choices = choices_sel_lab,
        selected = choices_sel_lab
      )

      choices_sel_visit <- unique(filtered_raw_data()$AVISIT)

      shiny::updateSelectizeInput(
        rs,
        inputId = ns_upload("select.visit"),
        choices = choices_sel_visit,
        selected = choices_sel_visit
      )

      shiny::updateSelectizeInput(
        rs,
        inputId = "arrange.lab",
        choices = choices_sel_lab,
        selected = choices_sel_lab
      )

      choices_sel_treatments <- unique(filtered_raw_data()$TRTP)

      shiny::updateSelectizeInput(
        rs,
        inputId = ns_upload("select.treatments"),
        choices = choices_sel_treatments,
        selected = choices_sel_treatments
      )
    })

    shiny::observeEvent(r$globals$select.lab, {
      if (length(r$globals$select.lab) <= length(r$globals$arrange.lab)) {
        tmp <- r$globals$arrange.lab[r$globals$arrange.lab %in% r$globals$select.lab]
      } else {
        tmp <- c(
          r$globals$arrange.lab,
          r$globals$select.lab[!r$globals$select.lab %in% r$globals$arrange.lab]
        )
      }
      shiny::updateSelectizeInput(
        rs,
        inputId = "arrange.lab",
        choices = tmp,
        selected = tmp
      )
    })

    shiny::observeEvent(r$globals$select.visit, {
      choices <- r$globals$select.visit
      shiny::req(choices)
      selected <- c(choices[1], choices[length(choices)])
      shiny::updateCheckboxGroupInput(
        rs,
        inputId = ns_box("trtcompar"),
        choices = choices,
        selected = selected
      )
      shinyWidgets::updatePickerInput(
        rs,
        inputId = "select.ai.first",
        choices = choices,
        selected = choices[1]
      )
      shinyWidgets::updatePickerInput(
        rs,
        inputId = "select.ai.last",
        choices = choices,
        selected = choices[length(choices)]
      )
    })

    #### Update Actionbuttons ####

    shiny::observeEvent(data_param(), {
      shiny::updateActionButton(
        rs,
        inputId = ns_qual("apply_qual_plot"),
        label = paste0(
          'Create/Update ',
          data_param()$nlab * data_param()$ntreat,
          ' graphs'
        )
      )
      shiny::updateActionButton(
        rs,
        inputId = ns_box("apply_quant_plot"),
        label = paste0(
          'Create/Update ',
          data_param()$nlab * data_param()$ntreat,
          ' graphs'
        )
      )
      shiny::updateActionButton(
        rs,
        inputId = ns_trees("apply_ref_plot"),
        label = paste0(
          'Create/Update ',
          data_param()$nlab2 * data_param()$ntreat,
          ' graphs'
        )
      )
    })

    shiny::observe({
      r$filtered_raw_data <- filtered_raw_data()
    })
    shiny::observe({
      r$filtered_and_reduced_raw_data <- filtered_and_reduced_raw_data()
    })
    shiny::observe({
      r$data_with_missing_flag <- data_with_missing_flag()
    })
    shiny::observe({
      r$data_without_missing_visits <- data_without_missing_visits()
    })
    shiny::observe({
      r$data_filtered_by_app_selection <- data_filtered_by_app_selection()
    })
    shiny::observe({
      r$prepare_dist_matrix_for_clustering <- prepare_dist_matrix_for_clustering()
    })
    shiny::observe({
      r$lab_parameter_order_by_clustering <- lab_parameter_order_by_clustering()
    })
    shiny::observe({
      r$data_with_selected_factor_levels <- data_with_selected_factor_levels()
    })
    shiny::observe({
      r$data_with_only_non_missings_over_visits <- data_with_only_non_missings_over_visits()
    })
    shiny::observe({
      r$quant_plot_data_lines <- quant_plot_data_lines()
    })
    shiny::observe({
      r$data_param <- data_param()
    })
    shiny::observe({
      r$Summary_for_qualitative_trends <- Summary_for_qualitative_trends()
    })
    shiny::observe({
      r$trtcompar_val <- trtcompar_val()
    })
  })
}
