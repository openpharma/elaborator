#' Generates boxplots for each treatment group and laboratory parameter
#'
#' @description
#' Generate boxplots showing the distribution of laboratory values at each visits of a study separately by treatment group and laboratory parameter according to the quantitative trend analysis.
#'
#'@param elab_data elaborator data
#'@param signtest logical, if true sign test, otherwise t-test for comparing Visit1 and Visit2 is performed
#'@param Visit1 name of visit time point one
#'@param Visit2 name of visit time point two, or vector of visit names for pairwise comparison to Visit1
#'@param sameaxes logical indicating if boxplot should have the same y-axis range for all treatment groups within a laboratory parameter
#'@param cols vector with colors of boxplots
#'@param labelvis vector of visit names used for labeling
#'@param labcolumn column name of dat1 which includes the laboratory parameter names
#'@param sortpoints logical indicating if dots in boxplots should be sorted from smallest to largest
#'@param pcutoff numeric for p-value threshold; if p-value of specified test is below this threshold, the background of the respective plot is colorized.
#'@param infotest list object from the perform test function
#'@param sortinput vector of laboratory parameter names in the order in which laboratory parameters are presented in the output
#'@param bordercol color of boxes which are not selected for hypothesis testing
#'@param add_points logical indicating if points showing patients' values should be added to the boxplots
#'@param connect_lines logical indicating if subject values should be connected between visits via lines
#'@param lin_data data set with connection line data
#'@param tolerated_percentage tolerated percentage value
#'@param color_lines_options option how the lines between visits should be colorized.
#'  Possible options are "first_last","each_visit",
#'  "custom_visits" or "all_grey". In the first three options
#'  decrease is colorized blue and an increase orange.
#'@param custom_visits if col_lines_options is "custom_visits" then two visits
#'  need to be selected. Otherwise all lines are grey.
#'@return No return value. Generates plots for the quantitative trends analysis.
#'
#'@keywords internal

elaborator_plot_quant_trends2 <- function(
  elab_data,
  signtest = TRUE,
  Visit1,
  Visit2,
  sameaxes,
  cols = NULL,
  labelvis = NULL,
  labcolumn,
  sortpoints = FALSE,
  pcutoff = 0.01,
  infotest = NULL,
  sortinput,
  bordercol = NULL,
  add_points = TRUE,
  connect_lines,
  lin_data = NULL,
  outliers,
  tolerated_percentage = 100,
  color_lines_options,
  custom_visits = NULL
) {


  num <- (length((levels(elab_data$TRTP)))*
    length(unique(elab_data$LBTESTCD)))

  graphics::par(
    mfrow = c(
      length((levels(elab_data$TRTP))),
      length(unique(elab_data$LBTESTCD))
    ),
    bty = "n",
    mar = c(1, 3, 1, 0),
    oma = c((max(nchar(levels(elab_data$AVISIT)))/3), 2, 3, 0),
    cex.main = 1.2,
    bg = "#E2F3F2"
  )
  #group data by treatment and labparameter
  grouped_data <- elab_data %>%
    dplyr::group_by(
      TRTP, LBTESTCD
    )

  if (connect_lines & !is.null(lin_data)) {

    grouped_lines_data <- lin_data %>%
      dplyr::group_by(
        TRTP, LBTESTCD
      )

    grouped_lines_data$LBTESTCD <- factor(grouped_lines_data$LBTESTCD,levels = levels(grouped_data$LBTESTCD))
    grouped_lines_data$TRTP <- factor(grouped_lines_data$TRTP,levels = levels(grouped_data$TRTP))

    combine <- dplyr::full_join(
      grouped_data %>% nest_legacy(.key = "raw"),
      grouped_lines_data %>% nest_legacy(.key = "differences"),
      by = c("TRTP", "LBTESTCD")
    ) %>%
    dplyr::group_by(
      TRTP, LBTESTCD
    )
  } else {
    grouped_lines_data <- NULL

    combine <- grouped_data %>%
      tidyr::nest_legacy(.key = "raw") %>%
      dplyr::group_by(
        TRTP, LBTESTCD
      )
  }

  if(!is.null(infotest)) {
    combine <- combine %>% dplyr::right_join(
      infotest %>% ungroup() %>%
        dplyr::group_by(TRTP,LBTESTCD) %>%
        tidyr::nest_legacy(.key = "test"),
      by = c("TRTP","LBTESTCD")
    )
  }

  if (num > 1) {
  shiny::withProgress(message = paste0('generating ', length(unique(elab_data$LBTESTCD))*length(unique(elab_data$TRTP)),' plots ...'), value = 0, {
    shiny::incProgress(0, detail = paste(""))
  combine %>% dplyr::group_walk(
    .f = ~ elaborator_boxplot(
      .,
      draw_points_logical = add_points,
      same_axes_per_treatment_logical = sameaxes,
      boxplot_color = cols,
      boxplot_border_color = bordercol,
      lines_data = connect_lines,
      number_plots = num,
      tol_percentage = tolerated_percentage,
      test_results_logical = !is.null(infotest),
      length_visit2_is_one_logical = (length(Visit2) == 1),
      incProgress = TRUE,
      outliers_logical = outliers,
      sort_points = sortpoints,
      col_lines_options = color_lines_options,
      custom_visit = custom_visits
    ), .keep = TRUE
  )

  #Progressbar end
  shiny::incProgress(1, detail = paste("done!"))
  })
  } else {
      combine %>% dplyr::group_walk(
    .f = ~ elaborator_boxplot(
      .,
      draw_points_logical = add_points,
      same_axes_per_treatment_logical = sameaxes,
      boxplot_color = cols,
      boxplot_border_color = bordercol,
      lines_data = connect_lines,
      number_plots = num,
      tol_percentage = tolerated_percentage,
      test_results_logical = !is.null(infotest),
      length_visit2_is_one_logical = (length(Visit2) == 1),
      incProgress = FALSE,
      outliers_logical = outliers,
      sort_points = sortpoints,
      col_lines_options = color_lines_options,
      custom_visit = custom_visits
    ), .keep = TRUE
  )
  }
}
