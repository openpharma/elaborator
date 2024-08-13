elaborator_remove_visits_due_tolerated_missings <- function(
  elab_data,
  tolerated_value
) {
# create a variable with the percentage number of missing values for parameter/visit/treatment
tmp <- elab_data %>%
  dplyr::group_by(LBTESTCD,AVISIT,TRTP) %>%
  dplyr::summarise(
    number_patients = length(LBORRES),
    non_missing = sum(is.na(LBORRES)),
    percent_missing = non_missing/number_patients,
    .groups = "keep"
  ) %>%
  dplyr::ungroup()

# compare the calculated percentage with the tolerated value (selected within app)
# save the information as logical variable cause_visit_removed
tmp2 <- tmp %>%
  dplyr::mutate(
    cause_visit_removed =
      case_when(
        percent_missing <= tolerated_value ~ FALSE,
        percent_missing > tolerated_value ~ TRUE
      )
  )

# generate logical variable visit_removed to flag
# if a visit should be removed for all treatments for specific
# lab parameter.
# keep variable_cause_visit_removed to use this information in the app to
# determine which visit/treatment caused the removing visit.
tmp3 <- tmp2 %>%
  dplyr::filter(cause_visit_removed == TRUE) %>%
  dplyr::select(LBTESTCD, AVISIT) %>%
  distinct() %>%
  dplyr::left_join(tmp2, multiple = "all", by = c("LBTESTCD", "AVISIT")) %>%
  dplyr::mutate(visit_removed = TRUE) %>%
  dplyr::select(LBTESTCD,AVISIT,TRTP,cause_visit_removed,visit_removed) %>%
  dplyr::right_join(tmp, by = c("LBTESTCD", "AVISIT", "TRTP")) %>%
  dplyr::mutate(
    cause_visit_removed = ifelse(is.na(cause_visit_removed), FALSE, cause_visit_removed),
    visit_removed = ifelse(is.na(visit_removed), FALSE, visit_removed)
  )

  #merge the two new flag variables to data set
  tmp4 <- elab_data %>%
    dplyr::right_join(tmp3, by = c("LBTESTCD", "AVISIT", "TRTP"),multiple = "all")

return(tmp4)

}
