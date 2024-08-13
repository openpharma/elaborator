elaborator_prepare_clustering_matrix <- function(
  elab_data,
  first_variable,
  last_variable
) {
  # Thu Feb 16 14:20:34 2023 ------------------------------
      ### need to be checked:
      # tmp <- full_join(
      #   elab_data %>%
      #     dplyr::filter(
      #     AVISIT == first_variable
      #   ) %>%
      #     dplyr::select(SUBJIDN,LBTESTCD,AVISIT,LBORRES) %>%
      #     dplyr::rename(first_variable = LBORRES),
      #   elab_data %>%
      #     dplyr::filter(
      #     AVISIT == last_variable
      #   ) %>%
      #     dplyr::select(SUBJIDN,LBTESTCD,AVISIT,LBORRES) %>%
      #     dplyr::rename(last_variable = LBORRES),
      #   by = c("SUBJIDN","LBTESTCD","AVISIT")
      # ) %>% dplyr::mutate(LBORRES_diff = first_variable-last_variable)
      # tmp %>%
      #     dplyr::select(LBTESTCD, AVISIT, SUBJIDN, LBORRES_diff) %>%
      #     tidyr::spread(key = LBTESTCD, value = LBORRES_diff) %>%
      #     dplyr::mutate(vari = paste0(SUBJIDN, "_", AVISIT)) %>%

      if (length(unique(elab_data$LBTESTCD)) > 1 && length(unique(elab_data$AVISIT)) > 1) {
        elab_data_tmp <- elab_data %>%
          dplyr::group_by(LBTESTCD,TRTP) %>%
          dplyr::summarise(median_value = median(LBORRES,na.rm = TRUE)) %>%
          dplyr::ungroup(LBTESTCD,TRTP)

        elab_data2 <- elab_data %>%
          right_join(elab_data_tmp, by = c('LBTESTCD','TRTP'))

        elab_data <- elab_data2 %>%
          dplyr::mutate(LBORRES = ifelse(is.na(LBORRES), median_value, LBORRES))
        elab_data_new <- elab_data %>%
          dplyr::group_by(SUBJIDN, LBTESTCD) %>%
          dplyr::mutate(n = n())
        elab_data_new <- elab_data_new %>%
          dplyr::arrange(SUBJIDN, LBTESTCD, AVISIT) %>%
          dplyr::filter(AVISIT %in% c(first_variable,last_variable))

        elab_data_new$LBORRES_diff <- c(0, diff(elab_data_new$LBORRES))
        elab_data_new_test <- elab_data_new %>%
          dplyr::mutate(LBORRES_diff = ifelse(AVISIT == first_variable, 0 , LBORRES_diff))

        tmp <- elab_data_new_test %>%
          dplyr::select(LBTESTCD, AVISIT, SUBJIDN, LBORRES_diff) %>%
          dplyr::filter(AVISIT == last_variable) %>%
          tidyr::spread(key = LBTESTCD, value = LBORRES_diff) %>%
          dplyr::mutate(vari = paste0(SUBJIDN, "_", AVISIT)) %>%
          dplyr::ungroup(SUBJIDN) %>%
          dplyr::select(-dplyr::one_of(c("SUBJIDN","AVISIT")))

        tmp <- tmp %>%
          dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(., median(., na.rm=TRUE))))

        tmp_nam <- tmp %>%
          dplyr::select(vari)
        tmp2 <- tmp %>%
          ungroup() %>%
          dplyr::select(-vari) %>%
          t()
        colnames(tmp2) <- t(tmp_nam$vari)

        tmp2
      }
}
