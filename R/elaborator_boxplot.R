#' Boxplot function used in app elaborator
#'
#' @param elab_data nested elaborator data with possibly up to three nests 'raw',
#'                  'test' and 'differences' grouped by treatment and labparameter
#' @param draw_points_logical a logical (TRUE/FALSE) if points should be drawn
#' @param same_axes_per_treatment_logical a logical (TRUE/FALSE) if same axes
#'                                        should be used within lab parameters
#' @param boxplot_color vector with color(s) for boxplots
#' @param boxplot_border_color vector with color(s) for boxplots border (used
#'                             to incidate which visits are used for statistical
#'                             tests)
#' @param lines_data data frame with connection lines between subjects lab values
#' @param number_plots number of plots (used in progress bar (if incProgress is TRUE))
#' @param tol_percentage tolerated percentage value
#' @param test_results_logical a logical (TRUE/FALSE) if statistical test is used
#' @param length_visit2_is_one_logical a logical (TRUE/FALSE) if statistical test
#'                                     is performed between one or more visits
#'                                     (this will influence the appearence of the background)
#' @param incProgress a logical (TRUE/FALSE) indicates if shiny progress bar is used
#' @param col_lines_options option how the lines between visits should be colorized.
#'                          Possible options are "first_last","each_visit",
#'                          "custom_visits" or "all_grey". In the first three options
#'                          decrease is colorized blue and an increase orange.
#' @param custom_visit if col_lines_options is "custom_visits" then two visits
#'                     need to be selected. Otherwise all lines are grey.
#'
#'@return No return value. Boxplots are generated, used in elaborator.
#'
#'@keywords internal

elaborator_boxplot <- function(
  elab_data,
  draw_points_logical,
  same_axes_per_treatment_logical,
  boxplot_color,
  boxplot_border_color,
  lines_data,
  number_plots,
  tol_percentage,
  test_results_logical,
  length_visit2_is_one_logical,
  sort_points,
  incProgress,
  outliers_logical,
  col_lines_options,
  custom_visit
) {

  raw <- elab_data$raw[[1]]
  if (same_axes_per_treatment_logical) {
    if (outliers_logical) {
      tmp_ylim <- c(unique(max(raw$elaborator_treatment_min,raw$elaborator_treatment_low_outlier)),unique(min(raw$elaborator_treatment_max,raw$elaborator_treatment_upp_outlier)))
    } else {
      tmp_ylim <- c(unique(raw$elaborator_treatment_min),unique(raw$elaborator_treatment_max))
    }
  } else {
    if (outliers_logical) {
      tmp_ylim_range <- range(raw$LBORRES)
      tmp_ylim_outlier_range <- (quantile(raw$LBORRES,prob=0.75) - quantile(raw$LBORRES,prob=0.25)) * 5
      tmp_ylim_outlier <- c(quantile(raw$LBORRES,prob=0.25)-tmp_ylim_outlier_range, quantile(raw$LBORRES,prob=0.75)+tmp_ylim_outlier_range)
      tmp_ylim <- c(
      max(tmp_ylim_outlier[1], tmp_ylim_range [1]),
      min(tmp_ylim_outlier[2], tmp_ylim_range [2])
      )
    } else {
      tmp_ylim <- range(raw$LBORRES)
    }
  }

  box_plot_object <- graphics::boxplot(
    formula = raw$LBORRES ~ raw$AVISIT,
    asp = 1,
    xaxt = "n",
    col = boxplot_color,
    border = boxplot_border_color,
    pars = list(boxwex = 0.75),
    lty = 1,
    staplewex = 0,
    outpch = ifelse(draw_points_logical,NA,16),
    ylim = tmp_ylim
  )
  box_plot_object

  #add median value labels into boxplots (in zoom panel only)
  if (!incProgress) {
    text(
      x = c(1:nlevels(raw$AVISIT)),
      y = box_plot_object$stats[3,] + (box_plot_object$stats[5,] - box_plot_object$stats[1,]) / 20,
      paste(round(box_plot_object$stats[3,],2)),
      cex = 0.9,
      col = "white"
    )
  }

  #visit label
  if (elab_data$TRTP == last(levels(elab_data$TRTP))) {
    graphics::text(
      as.numeric(raw$AVISIT),
      graphics::par("usr")[3],
      labels = raw$AVISIT,
      srt = 45,
      adj = c(1.1, 1.1),
      xpd = NA,
      cex = 0.8
    )
  }
  if (elab_data$LBTESTCD == levels(elab_data$LBTESTCD)[1]) {
    mtext(elab_data$TRTP, side = 2, line = 3, cex = 1.1)
  }
  if (elab_data$TRTP == levels(elab_data$TRTP)[1]) {
    graphics::mtext(unique(elab_data$LBTESTCD), 3, line = 1, cex = 1.1)
  }

  #outlier
  which.outlier <- raw %>%
    dplyr::pull(LBORRES) %>%
    dplyr::between(tmp_ylim[1],tmp_ylim[2])

  if (any(!which.outlier)) {
    index <- which(!which.outlier)
  }

  #normal ranges lines
  graphics::abline(
    h = c(raw$LBORNRLO,raw$LBORNRHI),
    col = "#57aefa",
    lty = 3,
    lwd = 0.8
  )


  # optional code depending on app selection:
  if (lines_data) {
    if (sort_points & draw_points_logical) {
      tmp <- elab_data$differences[[1]]
      index <- ((colSums(is.na(tmp)) / (colSums(!is.na(tmp))  + colSums(is.na(tmp))))  * 100) <= tol_percentage
      tmp <- tmp[,index]
      tmp <- tmp[complete.cases(tmp),]
      tmp <- tmp %>%
        dplyr::select(as.character(levels(raw$AVISIT))[as.character(levels(raw$AVISIT)) %in% as.character(unique(raw$AVISIT))])

      y <- t(tmp)
      x <- t(matrix(sort(as.numeric(raw$AVISIT) + sort(rep(seq(-0.25, 0.25, len = length(unique(raw$SUBJIDN))),length(unique(raw$AVISIT))))), dim(t(tmp))[2],dim(t(tmp))[1]))
      rank_order <- t(apply(y,1,rank))
      x_ordered <- t(sapply(1:nrow(rank_order), function(i) x[i,][rank_order[i,]]))

      if (col_lines_options == "first_last") {
        line_col <- apply(y, 2, function(x) { if(first(x)<last(x)){"#57aefa"} else if (first(x)>last(x)){"#fa5757"} else {"#bbbbbb"} })
        graphics::matlines(
          x = x_ordered,
          y = y,
          col = line_col
        )
      }
      if (col_lines_options == "all_grey") {
        graphics::matlines(
          x = x_ordered,
          y = y,
          col = "#bbbbbb"
        )
      }
      if (col_lines_options == "custom_visits") {
        custom <- custom_visit

        if (length(custom) == 2) {
          index_custom <- which(colnames(tmp) %in% custom)

          line_col <- apply(y[index_custom,], 2, function(x) { if(first(x)<last(x)){"#57aefa"} else if (first(x)>last(x)){"#fa5757"} else {"#bbbbbb"} })

          graphics::matlines(
             x = x_ordered,
            y = y,
            col = "#bbbbbb"
          )
        } else {
          graphics::matlines(
            x = x_ordered,
            y = y,
            col = "#bbbbbb"
          )
        }
        for (i in index_custom[1]:(index_custom[2]-1)) {
          graphics::matlines(
            x = x_ordered[i:(i+1),],
            y = y[i:(i+1),],
            col = line_col
          )
        }
      }

      if (col_lines_options == "each_visit") {
        for (i in 1:((dim(y)[1])-1)) {
          line_col <- apply(y[i:(i+1),], 2, function(x) { if(first(x)<last(x)){"#57aefa"} else if (first(x)>last(x)){"#fa5757"} else {"#bbbbbb"} })
          graphics::matlines(
            x = x_ordered[i:(i+1),],
            y = y[i:(i+1),],
            col = line_col
          )
        }
      }
    } else {
      tmp <- elab_data$differences[[1]]
      index <- ((colSums(is.na(tmp)) / (colSums(!is.na(tmp))  + colSums(is.na(tmp))))  * 100) <= tol_percentage
      tmp <- tmp[,index]
      tmp <- tmp[complete.cases(tmp),]

      if (col_lines_options == "first_last") {
        line_col <- apply(t(tmp), 2, function(x) { if(first(x)<last(x)){"#57aefa"} else if (first(x)>last(x)){"#fa5757"} else {"#bbbbbb"} })
        graphics::matlines(x = which(index), y = t(tmp), col = line_col)
      }

      if (col_lines_options == "custom_visits") {
        custom <- custom_visit
        if (length(custom) == 2) {
          index_custom <- which(colnames(tmp) %in% custom)

          line_col <- apply(t(tmp)[index_custom,], 2, function(x) { if(first(x)<last(x)){"#57aefa"} else if (first(x)>last(x)){"#fa5757"} else {"#bbbbbb"} })

          if ((index_custom[1]-1) != 0) {

            for (i in 1:(index_custom[1]-1)){
              graphics::matlines(
                x = which(index)[i:(i+1)],
                y = t(tmp)[i:(i+1),],
                col = "#bbbbbb"
              )
            }
          }

          if ((index_custom[2]) != length(index)) {
            for (i in ((index_custom[2])):((length(index))-1)){
              graphics::matlines(
                x = which(index)[i:(i+1)],
                y = t(tmp)[i:(i+1),],
                col = "#bbbbbb"
              )
            }
          }

          for (i in index_custom[1]:(last(index_custom)-1)) {
            graphics::matlines(
              x = which(index)[i:(i+1)],
              y = t(tmp)[i:(i+1),],
              col = line_col
            )
          }
        } else {
          graphics::matlines(x = which(index), y = t(tmp), col = "#bbbbbb")
        }
      }

      if (col_lines_options == "all_grey") {
          graphics::matlines(x = which(index), y = t(tmp), col = "#bbbbbb")
      }
      if (col_lines_options == "each_visit") {
        for (i in 1:((dim(t(tmp))[1])-1)) {
          line_col <- apply(t(tmp)[i:(i+1),], 2, function(x) { if(first(x)<last(x)){"#57aefa"} else if (first(x)>last(x)){"#fa5757"} else {"#bbbbbb"} })
          graphics::matlines(
            x = which(index)[i:(i+1)],
            y = t(tmp)[i:(i+1),],
            col = line_col
          )
        }
      }
    }
  }

  if (draw_points_logical) {
    x2 <- raw %>% arrange(AVISIT,LBORRES)
    if(sort_points) {
      graphics::points(
        x = sort(as.numeric(raw$AVISIT) + sort(rep(seq(-0.25,0.25,len=length(unique(raw$SUBJIDN))),length(unique(raw$AVISIT))))),
        y = x2$LBORRES,
        cex = 0.8,
        col = "#00000090",
        pch = 16,
        type="p"
      )
    } else {
      graphics::points(
        x = sort(as.numeric(raw$AVISIT)),
        y = x2$LBORRES,
        cex = 0.8,
        col = "#00000090",
        pch = 16,
        type="p"
      )
    }
  }

  if (test_results_logical) {
    tmp <- elab_data$test[[1]]
    index <- which(levels(raw$AVISIT) %in% tmp$AVISIT)
    index2 <- levels(raw$AVISIT)[levels(raw$AVISIT) %in% tmp$AVISIT]
    tmp <- tmp %>%
      dplyr::arrange(factor(AVISIT, levels = index2))
    for (k in index) {
      if (tmp$estimate_directions[which(index == k)] != "") {
        if(length_visit2_is_one_logical) {
          graphics::rect(
            1 - 0.5,
            graphics::par("usr")[3],
            length(levels(raw$AVISIT)) + 0.5,
            graphics::par("usr")[4], col = tmp$estimate_directions[which(index==k)],
            border = NA
          )
        } else {
          graphics::rect(
            k - 0.5,
            graphics::par("usr")[3],
            k + 0.5,
            graphics::par("usr")[4], col = tmp$estimate_directions[which(index==k)],
            border = NA
          )
        }
      }
    }
  }
  if(incProgress){
   shiny::incProgress(1/number_plots, detail = paste(""))
  }
}
