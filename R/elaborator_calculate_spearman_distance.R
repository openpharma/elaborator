#' Calculates the distance matrix based on Spearman's correlation coefficient
#'
#' @description
#' This function derives the Spearman correlation coefficients for a data matrix and transforms the resulting similarity matrix into a distance/dissimilarity matrix. No difference is made between positive and negative correlations, i.e., correlations of -1 and 1 will be transformed to a distance of 0; correlations of 0 will be transformed to a distance of 1. Missing values in the input data will be handled by casewise deletion.
#'
#'@param x data matrix for which a distance matrix based on the Spearman correlation coefficient is calculated
#'
#'@return A distance matrix of class "dist", which can be used for example for a seriation algorithm
#'
#'@keywords internal

elaborator_calculate_spearman_distance <- function(x){
  stats::as.dist(1 - abs(stats::cor(t(x), method = "spearman", use = "complete.obs")))
}
