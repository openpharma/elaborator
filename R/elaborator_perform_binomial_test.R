#' Performs binomial test for changes of a laboratory parameter between two visits
#'
#' @description
#' This function is mostly useful for generating the quantitative trend analysis plots. It returns the output of a binomial test result used for colorizing the background of the plots.
#'
#'@param data data set
#'@param treatment name of treatment group
#'@param lab_parameter name of laboratory parameter
#'@param Visit1 name of visit time point one
#'@param Visit2 name of visit time point two
#'@param lab_column column name of data which includes the laboratory parameter names
#'
#'@return A list of class "htest", with the results of the binomial test.
#'
#'@keywords internal

elaborator_perform_binomial_test <- function(
    data,
    treatment,
    lab_parameter,
    Visit1 = "Randomization",
    Visit2 = "End of Treatment",
    lab_column
){

  datasub <- data[data$TRTP == treatment & !is.na(data$TRTP) & data[,lab_column] == lab_parameter,]

  incr <- unlist(

    sapply(unique(datasub$SUBJIDN), function(z) {

      tmp1 <- datasub$SUBJIDN == z & datasub$AVISIT == Visit1
      tmp2 <- datasub$SUBJIDN == z & datasub$AVISIT %in% Visit2

      res <- datasub$LBORRES[tmp1] < datasub$LBORRES[tmp2]

      if (length(res) == 0 || is.na(res)) return(NA) else if (res == FALSE) {
        if (datasub$LBORRES[tmp1] ==
           datasub$LBORRES[tmp2]) return(NA)
      }
      return(res)
    }
    )
  )

  if (sum(is.na(incr)) < length(incr)) {
    testres <- stats::binom.test(x = sum(incr, na.rm = TRUE), n = sum(!is.na(incr)), p = 0.5)
  } else {testres <- NA}
}
