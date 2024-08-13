#' function to change (rdata) or set (csv data) the classes of the required variables in
#' elaborator
#'
#' @param elab_data elaborator_data (requires columns AVISIT,LBTESTCD,TRTP,SUBJIDN,LBORRES,LBORNRHI,LBORNRLO)
#' @param visit desired level order for AVISIT
#' @param treatment desired level order for TRTP
#' @param lab desired level order for LBTESTCD
#'
#' @return elab_data with changed classes (and factor levels)
#'
#' @keywords internal

elaborator_change_class_required_variables <- function(
  elab_data,
  visit,
  treatment,
  lab
) {

  #create factor variables:
  elab_data$AVISIT <- factor(elab_data$AVISIT, levels = visit)
  elab_data$LBTESTCD <- factor(elab_data$LBTESTCD, levels = lab)
  elab_data$TRTP <- factor(elab_data$TRTP, levels = treatment)

  #create character variables:
  elab_data$LBORNRHI <- as.character(elab_data$LBORNRHI)
  elab_data$LBORNRLO <- as.character(elab_data$LBORNRLO)

  #create numeric variables:
  elab_data$SUBJIDN <- as.numeric(elab_data$SUBJIDN)
  elab_data$LBORRES <- as.numeric(elab_data$LBORRES)

  return(elab_data)
}
