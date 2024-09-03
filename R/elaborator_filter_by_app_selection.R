#'

elaborator_filter_by_app_selection <- function(
    elab_data,
    visits,
    treat,
    labparameter
){
  AVISIT <- LBTESTCD <- TRTP <- NULL
  filtered_elab_data <- elab_data %>%
    dplyr::filter(
      AVISIT %in% visits &
      LBTESTCD %in% labparameter &
      TRTP %in% treat
    )

  #add complete cases filter
  # 1.step filter for complete cases by selection
  # filtered_elab_data_complete <- filtered_elab_data %>%
  #   dplyr::group_by(SUBJIDN, LBTESTCD, TRTP) %>%
  #   dplyr::summarise(visit_length_complete_cases = n(), .groups = "keep") %>%
  #   dplyr::filter(visit_length_complete_cases == length(visits))
  #
  # filtered_elab_data_test <- filtered_elab_data %>%
  #   dplyr::left_join(filtered_elab_data_complete, by = c("SUBJIDN","LBTESTCD","TRTP")) %>%
  #   dplyr::filter(!is.na(visit_length_complete_cases)) %>%
  #   dplyr::select(-visit_length_complete_cases)

  # # #2.step filter if complete cases appear in all treatment arms

  # filtered_elab_data_test2 <- filtered_elab_data_test %>%
  #   dplyr::left_join(
  #     filtered_elab_data_test %>%
  #       dplyr::select(LBTESTCD,TRTP) %>%
  #       unique() %>%
  #       dplyr::group_by(LBTESTCD) %>%
  #       dplyr::summarise(visit_length_complete_cases = n()) %>%
  #       dplyr::filter(visit_length_complete_cases == length(treat))
  #       ,
  #     by = "LBTESTCD"
  #   ) %>%
  #   dplyr::filter(!is.na(visit_length_complete_cases)) %>%
  #   dplyr::select(-visit_length_complete_cases)
  #
  # filtered_elab_data <- filtered_elab_data_test2
  filtered_elab_data
}
