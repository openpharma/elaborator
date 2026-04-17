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
    shiny::uiOutput("filter_percentage"),
    shiny::uiOutput("pickerinput_filter"),
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::actionButton(
          inputId = "insertBtn",
          label = "Add",
          icon = icon("plus")
        )
      ),
      shiny::column(
        4,
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
    #### 2. filter by app filter-tab  ####
    # used function:
    # ---
    # purpose:
    # filter data set by filter-tab selection within elaborator app.
    #
    # reactivity triggers :
    # raw_data_and_warnings() / <filter selection within app>
    raw_data_and_warnings <- shiny::reactive(r$raw_data_and_warnings)

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
        visits = input$select.visit,
        treat = input$select.treatments,
        labparameter = input$select.lab
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
        tolerated_value = (input$select.toleratedPercentage / 100)
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
      shiny::req(
        input$select.treatments,
        input$select.visit,
        data_without_missing_visits(),
        raw_data_and_warnings()
      )
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
          TRTP,
          LBTESTCD
        ) %>%
        dplyr::select(TRTP, LBTESTCD, SUBJIDN, AVISIT, LBORRES) %>%
        ## by group? map_group?
        tidyr::pivot_wider(names_from = AVISIT, values_from = LBORRES) %>%
        dplyr::select(-SUBJIDN)
      tmp <- tmp[, c(
        "TRTP",
        "LBTESTCD",
        levels(data_with_selected_factor_levels()$AVISIT)
      )]
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
      } else if (input$orderinglab == "alphabetically") {
        sort(as.character(unique(ds$LBTESTCD)))
      } else if (input$orderinglab == "auto") {
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

    data_with_selected_factor_levels <- shiny::eventReactive(
      c(data_filtered_by_app_selection(), input$go3),
      {
        tmp <- data_filtered_by_app_selection()
        #re-level the lab parameter vector for arrangement within app
        if (shiny::isolate(input$orderinglab) == "asinp") {
          lab_levels <- unique(raw_data_and_warnings()$data$LBTESTCD)
          lab_levels <- lab_levels[lab_levels %in% input$select.lab]
          tmp$LBTESTCD <- factor(tmp$LBTESTCD, levels = lab_levels)
        } else if (shiny::isolate(input$orderinglab) == "alphabetically") {
          lab_levels <- sort(unique(raw_data_and_warnings()$data$LBTESTCD))
          lab_levels <- lab_levels[lab_levels %in% input$select.lab]
          tmp$LBTESTCD <- factor(tmp$LBTESTCD, levels = lab_levels)
        } else if (shiny::isolate(input$orderinglab) == "auto") {
          lab_levels <- shiny::isolate(lab_parameter_order_by_clustering())
          lab_levels <- c(
            lab_levels,
            as.character(unique(tmp$LBTESTCD)[which(
              !unique(tmp$LBTESTCD) %in% lab_levels
            )])
          )
          tmp$LBTESTCD <- factor(tmp$LBTESTCD, levels = lab_levels)
        } else if (shiny::isolate(input$orderinglab) == "manual") {
          lab_levels <- input$arrange.lab
          lab_levels <- lab_levels[lab_levels %in% input$select.lab]
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
      shiny::req(input$select.visit)
      tmp <- data_with_selected_factor_levels() %>%
        dplyr::full_join(
          data_with_selected_factor_levels() %>%
            dplyr::group_by(TRTP, LBTESTCD) %>%
            dplyr::summarise(
              visits_non_missing = length(unique(AVISIT)),
              .groups = "keep"
            ),
          by = c("TRTP", "LBTESTCD")
        )

      tmp2 <- tmp %>%
        dplyr::right_join(
          tmp %>%
            dplyr::group_by(SUBJIDN, LBTESTCD, TRTP) %>%
            dplyr::summarise(
              non_missing_values = sum(!is.na(LBORRES)),
              all_complete = unique(ifelse(
                non_missing_values ==
                  ifelse(is.null(visits_non_missing), 0, visits_non_missing),
                TRUE,
                FALSE
              )),
              .groups = "keep"
            ) %>%
            dplyr::ungroup() %>%
            dplyr::select(SUBJIDN, LBTESTCD, TRTP, all_complete) %>%
            dplyr::distinct(),
          by = c("SUBJIDN", "LBTESTCD", "TRTP")
        ) %>%
        dplyr::filter(all_complete == TRUE)
      tmp2
    })
  })
}

## To be copied in the UI
# mod_filter_ui("filter_1")

## To be copied in the server
# mod_filter_server("filter_1")
