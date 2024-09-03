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

elaborator_derive_test_values <- function(
    data = data,
    signtest = TRUE,
    Visit1 = c("Randomization"),
    Visit2 = c("End of Treatment"),
    lab_column) {

  if (length(levels(data$TRTP))*length(levels(as.factor(as.character(data[, lab_column])))) > 1) {
  shiny::withProgress(message = 'performing statistical test(s) ...', value = 0, {
  shiny::incProgress(0, detail = paste(""))

  infotest <- rep(list(list(p.value = NULL, estimate = NULL)), length(Visit2))

  for(i in 1:length(Visit2)) {

    pval <- sapply(levels(data$TRTP), function(treat){
      sapply(levels(as.factor(as.character(data[, lab_column]))), function(labpara){

        if (any(!is.na(elaborator_perform_binomial_test(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)))) {
          res <- ifelse(signtest == TRUE,
            elaborator_perform_binomial_test(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)$p.value,
            elaborator_perform_ttest(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)$p.value
          )
        } else {res <- NA}
      })
    })
    pval2 <- matrix(pval,length(levels(as.factor(as.character(data[, lab_column])))), length(levels(data$TRTP)))
    rownames(pval2) <- rownames(pval)
    colnames(pval2) <- colnames(pval)
    shiny::incProgress(1/(length(Visit2))*0.5, detail = paste(""))

    esti <- sapply(levels(data$TRTP), function(treat){
      sapply(levels(as.factor(as.character(data[, lab_column]))), function(labpara){

        if (any(!is.na(elaborator_perform_binomial_test(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)))) {
          res <- ifelse(signtest == TRUE,
                        as.numeric(elaborator_perform_binomial_test(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)$estimate),
                        as.numeric(elaborator_perform_ttest(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)$estimate))
        } else {res <- NA}
      })
    })

    esti2 <- matrix(esti, length(as.character(unique(data$LBTESTCD))),
                   length(levels(data$TRTP)))
    rownames(esti2) <- rownames(esti)
    colnames(esti2) <- colnames(esti)

    infotest[[i]]$p.value <- pval2
    infotest[[i]]$estimate <- esti2

    shiny::incProgress(1/(length(Visit2))*0.5, detail = paste(""))
  }

  shiny::incProgress(0, detail = paste("done!"))
  })
  } else {

      infotest <- rep(list(list(p.value = NULL, estimate = NULL)), length(Visit2))

      for(i in 1:length(Visit2)) {

        pval <- sapply(levels(data$TRTP), function(treat){
          sapply(levels(as.factor(as.character(data[, lab_column]))), function(labpara){

            if (any(!is.na(elaborator_perform_binomial_test(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)))) {
              res <- ifelse(signtest == TRUE,
                            elaborator_perform_binomial_test(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)$p.value,
                            elaborator_perform_ttest(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)$p.value
              )
            } else {res <- NA}
          })
        })
        pval <- matrix(pval,length(levels(as.factor(as.character(data[, lab_column])))), length(levels(data$TRTP)))
        rownames(pval) <- levels(as.factor(as.character(data[, lab_column])))
        colnames(pval) <- levels(data$TRTP)

        esti <- sapply(levels(data$TRTP), function(treat){
          sapply(levels(as.factor(as.character(data[, lab_column]))), function(labpara){

            if (any(!is.na(elaborator_perform_binomial_test(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)))) {
              res <- ifelse(signtest == TRUE,
                            as.numeric(elaborator_perform_binomial_test(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)$estimate),
                            as.numeric(elaborator_perform_ttest(data = data, treatment = treat, lab_parameter = labpara, Visit1 = Visit1, Visit2 = Visit2[i], lab_column = lab_column)$estimate))
            } else {res <- NA}
          })
        })
        esti <- matrix(esti, length(as.character(unique(data$LBTESTCD))),
                       length(levels(data$TRTP)))
        rownames(esti) <- as.character(unique(data$LBTESTCD))
        colnames(esti) <- levels(data$TRTP)

        infotest[[i]]$p.value <- pval
        infotest[[i]]$estimate <- esti
      }
  }
  infotest
}
