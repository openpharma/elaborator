#' create hover text info in elaborator app zoom panel(s)
#'
#' @param elab_data elaborator data
#' @param labparameter labparameter used for filtering
#' @param treat treatment used for filtering
#'
#' @keywords internal
#'
#' @return character string with text about number of visits in every
#'         treatment group and labparameter
#'

elaborator_create_hover_info_text <- function(
  elab_data,
  labparameter,
  treat,
  select.visit
) {

  ds_filtered <- elab_data %>% dplyr::filter(
    LBTESTCD == labparameter,
    TRTP == treat
  )

    df_filtered_text <- ds_filtered %>%
      dplyr::group_by(AVISIT) %>%
      dplyr::summarise(
        median_value = median(LBORRES),
        number_subjects = length(SUBJIDN),
        missing_values = sum(is.na(LBORRES))
      ) %>%
      dplyr::full_join(
        ds_filtered %>%
          dplyr::select(AVISIT,cause_visit_removed,visit_removed) %>%
          unique(),
        by = "AVISIT"
      ) %>%
      dplyr::mutate(
        number_subjects_text =
          paste0(
            ifelse(visit_removed,"<s style='color: grey'>",""), AVISIT, ": ", number_subjects - missing_values, ifelse(visit_removed, "</s> Visit removed",""), "<br>"
          )
      )

      # Wed May 22 12:59:49 2024 ------------------------------
      df_filtered_text <- df_filtered_text[order(match(df_filtered_text$AVISIT, select.visit)), ]

      length_visits <- ds_filtered %>%
        dplyr::filter(visit_removed == FALSE) %>% pull(AVISIT) %>%
        unique() %>% length()

      number_all_non_missings <- ds_filtered %>%
        dplyr::filter(visit_removed == FALSE) %>%
        dplyr::group_by(SUBJIDN,LBTESTCD,TRTP) %>%
        dplyr::summarise(
          number_visits = length_visits,
          non_missing_values = sum(!is.na(LBORRES)),
          all_complete = ifelse(non_missing_values == length_visits, TRUE,FALSE),
          .groups = "keep"
        ) %>%
        dplyr::ungroup() %>%
        dplyr::pull(all_complete) %>%
        sum()
    return(
      paste(
      "<p>
      <b> Number of subjects with non-missing ", labparameter," values for all ",
      ifelse(
        any(ds_filtered$visit_removed),paste0("(remaining)"), paste0("")
      )
      ," visits in ", treat,": ","<b style='font-size: 16px'>",number_all_non_missings,"</b>","</b>","<br>",

      "Number of subjects with non-missing lab values by visit:", "<br>",
      paste(df_filtered_text$number_subjects_text, collapse =""),
      "</p>"
      ,collapse ="")
    )
}
