#' Performs t-test for changes of a laboratory parameter between two visits
#'
#' @description
#' This function is mostly useful for generating the quantitative trend analysis plots. It returns the output of a t-test result used for colorizing the background of the plots.
#'
#'@param data data set
#'@param treatment name of treatment group
#'@param lab_parameter name of laboratory parameter
#'@param Visit1 name of visit time point one
#'@param Visit2 name of visit time point two
#'@param lab_column column name of data which includes the laboratory parameter names
#'
#'@return A list of class "htest" with the results of the t-test.
#'
#'@keywords internal

elaborator_perform_ttest2 <- function(data, treatment, lab_parameter, Visit1 = "Randomization", Visit2 = "End of Treatment", lab_column){

  datasub <- data[data$TRTP == treatment & data[,lab_column] == lab_parameter,]

  tmp <- dplyr::full_join(datasub %>% dplyr::filter(AVISIT == Visit1) %>% dplyr::select(-AVISIT),
  datasub %>% dplyr::filter(AVISIT == Visit2) %>% dplyr::select(-AVISIT),
  by = c("LBTESTCD", "TRTP", "SUBJIDN")) %>%
    dplyr::mutate(differ = LBORRES.x - LBORRES.y)

  if(length(na.omit(tmp$differ))>=2) {
    testres <- stats::t.test(x = tmp$differ)
  } else {
    testres <- rep(NA,length(tmp$differ))
  }
}
