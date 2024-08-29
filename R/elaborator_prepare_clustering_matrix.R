#' Transform data in form for cluster analysis in app elaborator
#'
#' @param elab_data data set
#' @param first_variable character with variable name
#' @param last_variable character with variable name
#'
#' @return Transformed data frame
#'
#' @keywords internal

elaborator_prepare_clustering_matrix <- function(
  elab_data,
  first_variable,
  last_variable
) {
    LBTESTCD <- TRTP <- LBORRES <- median_value <- SUBJIDN <- AVISIT <- NULL
    LBORRES_diff <- vari <- NULL

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
          dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ tidyr::replace_na(., median(., na.rm=TRUE))))

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
