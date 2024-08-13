#' Derives p-values and estimates for sign test or t-test
#'
#' @description
#' This function is mostly useful for generating the quantitative trend analysis plots. It returns p-values and estimates for the sign test or the t-test used for colorizing the background of the plots.
#'
#'@param data data set
#'@param signtest logical, if true sign test, otherwise t-test for comparing Visit1 and Visit2 is performed
#'@param Visit1 name of visit time point one
#'@param Visit2 name of visit time point two, or vector of visit names for pairwise comparison to Visit1
#'@param lab_column column name of data which includes the laboratory parameter names
#'
#'@return A list with p-values and estimates of the selected statistical test. The p-values and estimates will be used to colorize the background of the quantitative trend analysis plots.
#' \itemize{
#'   \item p.value - p.value of statistical test.
#'   \item estimate - estimate of statistical test.
#' }
#'
#'@keywords internal

elaborator_derive_test_values2 <- function(
    data = data,
    signtest = TRUE,
    Visit1 = c("Randomization"),
    Visit2 = c("End of Treatment"),
    lab_column) {

  data <- data[, c(lab_column,"TRTP","SUBJIDN","AVISIT","LBORRES")]

  #if (length(levels(data$TRTP)) * length(levels(as.factor(as.character(data[, lab_column])))) > 1) {

  shiny::withProgress(message = 'performing statistical test(s) ...', value = 0, {
  shiny::incProgress(0, detail = paste(""))

  infotest <- rep(
    list(
      list(
        p.value = matrix(NA,nrow = length(levels(as.factor(as.character(data[, lab_column])))), ncol = length(levels(data$TRTP))),
        estimate = matrix(NA,nrow = length(levels(as.factor(as.character(data[, lab_column])))), ncol = length(levels(data$TRTP)))
      )
    ), length(Visit2)
  )
  total_number_tests <- length(infotest)*dim(infotest[[1]][1]$p.value)[1]*dim(infotest[[1]][1]$p.value)[2]

  if(signtest) {
     for(i in 1:length(Visit2)) {
      for(treat in levels(data$TRTP)) {
        for (labpara in levels(as.factor(as.character(data[, lab_column])))) {
          ttest <- elaborator_perform_binomial_test2(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)
           if (any(!is.na(ttest))) {
             infotest[[i]]$p.value[which(levels(as.factor(as.character(data[, lab_column]))) == labpara), which(levels(data$TRTP) == treat)] <- ttest$p.value
             infotest[[i]]$estimate[which(levels(as.factor(as.character(data[, lab_column]))) == labpara), which(levels(data$TRTP) == treat)] <- ttest$estimate
           } else {
             infotest[[i]]$p.value[which(levels(as.factor(as.character(data[, lab_column]))) == labpara), which(levels(data$TRTP) == treat)] <- NA
             infotest[[i]]$estimate[which(levels(as.factor(as.character(data[, lab_column]))) == labpara), which(levels(data$TRTP) == treat)] <- NA
           }
           shiny::incProgress(1/total_number_tests , detail = paste(""))
        }
      }
      rownames(infotest[[i]]$p.value) <- levels(as.factor(as.character(data[, lab_column])))
      colnames(infotest[[i]]$p.value) <-levels(data$TRTP)
      rownames(infotest[[i]]$estimate) <- levels(as.factor(as.character(data[, lab_column])))
      colnames(infotest[[i]]$estimate) <-levels(data$TRTP)
    }
  } else {
    for(i in 1:length(Visit2)) {
      for(treat in levels(data$TRTP)) {
        for (labpara in levels(as.factor(as.character(data[, lab_column])))) {
          ttest <- elaborator_perform_ttest2(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)
           if (any(!is.na(ttest))) {
             infotest[[i]]$p.value[which(levels(as.factor(as.character(data[, lab_column]))) == labpara), which(levels(data$TRTP) == treat)] <- ttest$p.value
             infotest[[i]]$estimate[which(levels(as.factor(as.character(data[, lab_column]))) == labpara), which(levels(data$TRTP) == treat)] <- ttest$estimate
           } else {
             infotest[[i]]$p.value[which(levels(as.factor(as.character(data[, lab_column]))) == labpara), which(levels(data$TRTP) == treat)] <- NA
             infotest[[i]]$estimate[which(levels(as.factor(as.character(data[, lab_column]))) == labpara), which(levels(data$TRTP) == treat)] <- NA
           }
           #shiny::incProgress(1/total_number_tests , detail = paste(""))
        }
      }
      rownames(infotest[[i]]$p.value) <- levels(as.factor(as.character(data[, lab_column])))
      colnames(infotest[[i]]$p.value) <-levels(data$TRTP)
      rownames(infotest[[i]]$estimate) <- levels(as.factor(as.character(data[, lab_column])))
      colnames(infotest[[i]]$estimate) <-levels(data$TRTP)
    }
  }

  shiny::incProgress(0, detail = paste("done!"))
  })
  infotest
}
