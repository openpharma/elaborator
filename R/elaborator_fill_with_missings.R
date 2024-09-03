#' Transform data and fill them with missing values in app elaborator
#'
#' @param elab_data data set
#'
#' @return Transformed data frame
#'
#' @keywords internal

elaborator_fill_with_missings <- function(
  elab_data
){
  SUBJIDN <- TRTP <- LBTESTCD <- LBORRES <- elaborator_treatment_low_quant <- elaborator_treatment_upp_quant <- NULL
  # create "empty" lab value data set for every subject, visit and lab parameter
  tmp_grid <- tidyr::crossing(
    SUBJIDN = unique(elab_data$SUBJIDN),
    AVISIT = unique(elab_data$AVISIT),
    LBTESTCD = unique(elab_data$LBTESTCD)
  )
  # assign treatment to every subject
  tmp_grid2 <- tmp_grid %>%
  dplyr::full_join(
    elab_data %>%
      dplyr::select(SUBJIDN,TRTP) %>%
      dplyr::distinct(),
    by = "SUBJIDN"
  )
  tmp_grid3 <- tmp_grid2 %>%
  dplyr::left_join(
    elab_data,
    by = c("SUBJIDN", "AVISIT", "LBTESTCD", "TRTP")
  )

  #add columns elaborator_treatment_min and max for
  #quantitative trend plots option 'use same axes'

  elab_data <- elab_data %>% right_join(
    elab_data %>% dplyr::group_by(
      LBTESTCD
    ) %>% dplyr::summarise(
      elaborator_treatment_min = min(LBORRES, na.rm = TRUE),
      elaborator_treatment_max = max(LBORRES, na.rm = TRUE),
      elaborator_treatment_low_quant = quantile(LBORRES, na.rm =TRUE, probs = 0.25),
      elaborator_treatment_upp_quant = quantile(LBORRES, na.rm =TRUE, probs = 0.75)
    ),
    by = c("LBTESTCD")
  )

  elab_data <- elab_data %>%
    dplyr::mutate(
      elaborator_treatment_low_outlier = elaborator_treatment_low_quant - (5*(elaborator_treatment_upp_quant-elaborator_treatment_low_quant)),
      elaborator_treatment_upp_outlier = elaborator_treatment_upp_quant + (5*(elaborator_treatment_upp_quant-elaborator_treatment_low_quant)),
    )
  return(elab_data)
}
