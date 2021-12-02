#' Generates boxplots for each treatment group and laboratory parameter
#'
#' @description
#' Generate boxplots showing the distribution of laboratory values at each visits of a study separately by treatment group and laboratory parameter according to the quantitative trend analysis.
#'
#'@param dat1 data set
#'@param signtest logical, if true sign test, otherwise t-test for comparing Visit1 and Visit2 is performed
#'@param Visit1 name of visit time point one
#'@param Visit2 name of visit time point two, or vector of visit names for pairwise comparison to Visit1
#'@param sameaxes logical indicating if boxplot should have the same y-axis range for all treatment groups within a laboratory parameter
#'@param ats numeric vector giving the locations where the boxplots should be drawn; defaults to 1:n where n is the number of boxplots. 
#'@param cols vector with colors of boxplots
#'@param labelvis vector of visit names used for labelling
#'@param labcolumn column name of dat1 which includes the laboratory parameter names
#'@param sortpoints logical indicating if dots in boxplots should be sorted from smallest to largest
#'@param pcutoff numeric for p-value threshold; if p-value of specified test is below this threshold, the background of the respective plot is colorized.
#'@param cexoutliers size of the outliers
#'@param infotest list object from the perform test function
#'@param sortinput vector of laboratory parameter names in the order in which laboratory parameters are presented in the output
#'@param bordercol color of boxes which are not selected for hypothesis testing
#'@param add_points logical indicating if points showing patients' values should be added to the boxplots
#'@param connect_lines logical indicating if subject values should be connected between visits via lines
#'
#'@return No return value. Generates plots for the quantitative trends analysis.
#'
#'@keywords internal

elaborator_plot_quant_trends <- function(
  dat1,
  signtest = TRUE,
  Visit1,
  Visit2,
  sameaxes = FALSE,
  ats = NULL,
  cols = NULL,
  labelvis = NULL,
  labcolumn,
  sortpoints = FALSE,
  pcutoff = 0.01,
  cexoutliers = 0.7,
  infotest = NULL,
  sortinput = as.character(unique(dat1[, labcolumn])),
  bordercol = NULL,
  add_points = TRUE,
  connect_lines = FALSE
  ) {
  PARAMCD <- . <- AVISIT <- SUBJIDN <- TRTP <- LBORRES <- NULL

  if (length(unique(dat1$PARAMCD))*length(unique(dat1$TRTP)) > 1) {
  shiny::withProgress(message = paste0('generating ', length(unique(dat1$PARAMCD))*length(unique(dat1$TRTP)),' Plots ...'), value = 0, {
    ColorBG <- "#E2F3F2"
    textcol <- arrowcol <- "#f78300"
    shiny::incProgress(0, detail = paste(""))
    if (length((unique(dat1$TRTP))) == 0 |
       length(unique(dat1[, labcolumn])) == 0){
      on_ex <- graphics::par("mfrow","bty","mar","oma","bg")
      on.exit(graphics::par(on_ex))
      graphics::par(
        mfrow = c(1,1),
        bty = "n",
        mar = c(1, 1, 1, 1),
        oma = c(0, 0, 0, 0),
        bg = ColorBG
      )
      graphics::plot(NULL, NULL, ylim = c(0, 1), xlim = c(0, 1), axes = FALSE, ylab = "", xlab = "")
      graphics::text(0.5 , 0.5 , paste0("No values for this Treatment"))
    } else {
      on_ex <- graphics::par("mfrow","bty","mar","oma","cex.main","bg")
      on.exit(graphics::par(on_ex))
      graphics::par(
        mfrow = c(
          length((unique(dat1$TRTP))),
          length(unique(dat1[, labcolumn]))
        ),
        bty = "n",
        mar = c(1, 3, 0, 0),
        oma = c((max(nchar(levels(dat1$AVISIT)))/3), 2, 3, 0),
        cex.main = 1.4,
        bg = ColorBG
      )

      sapply(levels(dat1$TRTP), function(treat) {

        dattreat <- dat1[dat1$TRTP == treat,]

        sapply(sortinput, function(labpara){

          ntreat <- length(unique(dattreat$TRTP))
          nvisit <- length(
            unique(
              dattreat %>%
                dplyr::filter(PARAMCD == labpara) %>%
                .$AVISIT
            )
          )
          nlab <- length(unique(dattreat$LBTESTCD))

          if (is.null(cols)) cols <- rep(
            c(colBoxplot1,
              colBoxplot2,
              colBoxplot3,
              colBoxplot4
            ),
            length = nvisit
          )

          if (is.null(labelvis)) labelvis <- levels(dat1$AVISIT)
          if (is.null(bordercol)) bordercol <- rep(c("black"), length = nvisit)

          mainlab <- ifelse(
            treat == levels(dat1$TRTP)[1],
            paste(
              strwrap(
                paste(strsplit(as.character(labpara), split = "/")[[1]], collapse = " / "), width = 16
              ),
            collapse = "\n"
            ),
            ""
          )

          ats_nr <- dattreat[dattreat[, labcolumn] == labpara,] %>%
            dplyr::pull(AVISIT) %>%
            unique() %>%
            length()

          ats <- 1 + (1.5 * (1:ats_nr - 1))

          labelvis <- dattreat[dattreat[, labcolumn] == labpara,] %>%
            droplevels() %>%
            dplyr::pull(AVISIT) %>%
            levels()

          out_lower_vec <- out_upper_vec <- vector()

          for (i in 1:length(unique(dattreat$AVISIT))) {
            quan <- stats::quantile(
              dat1$LBORRES[dat1[, labcolumn] == labpara & dat1$AVISIT == levels(as.factor(as.character((dat1$AVISIT))))[i]],
              probs = c(0.25, 0.75),
              na.rm = TRUE
            )

            out <- (quan[2] - quan[1]) * 5
            out_lower_vec[i] <- quan[1] - out
            out_upper_vec[i] <- quan[2] + out
          }

          out_lower <- min(out_lower_vec, na.rm = TRUE)
          out_upper <- max(out_upper_vec, na.rm = TRUE)

          if ((dattreat[dattreat[, labcolumn] == labpara,] %>%
              droplevels()) %>%
              dim() %>%
              .[1] == 0
          ) {
            graphics::plot(
              NULL,
              NULL,
              ylim = c(0, 1),
              xlim = c(0, 1),
              axes = FALSE,
              ylab = "",
              xlab = ""
            )
            graphics::text(
              0.5,
              0.5,
              paste0("No values for this Treatment")
            )
          } else {
            graphics::boxplot(
              formula = LBORRES ~ AVISIT,
              data = dattreat[dattreat[, labcolumn] == labpara,] %>%
                        droplevels(),
              asp = 1,
              xaxt = "n",
              yaxt = "n",
              col = cols,
              border = bordercol,
              at = ats,
              pars = list(boxwex = 1),
              lty = 1,
              staplewex = 0,
              outpch = 16,
              xlim = c(ats[1] - 1, ats[length(ats)] + 1),
              ylim = c(
                ifelse(
                  sameaxes == TRUE,
                  max(c(out_lower, min(dat1$LBORRES[dat1[, labcolumn] == labpara], na.rm = TRUE))),
                  min(dattreat[dattreat[, labcolumn] == labpara,]$LBORRES, na.rm = TRUE)
                ),
                ifelse(
                  sameaxes == TRUE,
                  c(min(c(out_upper, max(dat1$LBORRES[dat1[, labcolumn] == labpara], na.rm = TRUE)))),
                  max(dattreat[dattreat[, labcolumn] == labpara,]$LBORRES, na.rm = TRUE)
                )
              )
            )

            if (!any(is.na(infotest)) && !is.null(infotest)) {

              if (length(Visit2) == 1) {

                if (infotest[[1]]$p.value[labpara,treat] <= pcutoff &
                   !is.na(infotest[[1]]$p.value[labpara, treat]) &
                   !is.null(infotest[[1]]$p.value[labpara, treat])) {

                  if (signtest == TRUE) {
                    est_test <- infotest[[1]]$estimate[labpara,treat] < 0.5
                  } else if (signtest == FALSE) {
                    est_test <- infotest[[1]]$estimate[labpara,treat] > 0
                  }

                  graphics::rect(
                    graphics::par("usr")[1],
                    graphics::par("usr")[3],
                    graphics::par("usr")[2],
                    graphics::par("usr")[4],
                    col = ifelse(
                      est_test,
                      "#47d2bc",
                      "#ffeeaa"
                    ),
                    border = NA
                  )
                }
                if (is.na(infotest[[1]]$p.value[labpara, treat]) |
                   is.null(infotest[[1]]$p.value[labpara, treat])) {
                  graphics::rect(
                    graphics::par("usr")[1],
                    graphics::par("usr")[3],
                    graphics::par("usr")[2],
                    graphics::par("usr")[4],
                    col = "#A9A9A9",
                    border = NA
                  )
                }
              }

              if (length(Visit2) > 1) {

                for (j in Visit2) {
                  i <- which(Visit2 == j)
                  k <- which(
                    levels(
                      dat1 %>%
                        dplyr::filter(PARAMCD == labpara) %>%
                        .$AVISIT %>%
                        droplevels()
                    ) == j
                  )

                  if (infotest[[i]]$p.value[labpara,treat] <= pcutoff &
                     !is.na(infotest[[i]]$p.value[labpara,treat]) &
                     !is.null(infotest[[i]]$p.value[labpara,treat])) {

                    if (signtest == TRUE) {
                      est_test <- infotest[[i]]$estimate[labpara,treat] < 0.5
                    } else if (signtest == FALSE) {
                      est_test <- infotest[[i]]$estimate[labpara,treat] > 0
                    }

                    sigcol <- ifelse(est_test, "#47d2bc", "#ffeeaa")

                    if (length(k) > 0) {
                    graphics::rect(
                      ats[k] - 0.75,
                      graphics::par("usr")[3],
                      ats[k] + 0.75,
                      graphics::par("usr")[4], col = sigcol, border = NA
                    )
                    }
                  }

                  if (is.na(infotest[[i]]$p.value[labpara, treat]) |
                     is.null(infotest[[i]]$p.value[labpara, treat])) {
                    if (length(k) > 0) {
                    graphics::rect(ats[k] - 0.75,
                         graphics::par("usr")[3],
                         ats[k] + 0.75,
                         graphics::par("usr")[4], col = "#A9A9A9", border = NA)
                    }
                  }
                }
              }

              graphics::boxplot(
                formula = LBORRES ~ AVISIT,
                data = dattreat[dattreat[, labcolumn] == labpara,] %>%
                  droplevels(),
                asp = 1,
                xaxt = "n",
                yaxt = "n",
                col = cols,
                border = bordercol,
                at = ats,
                pars = list(boxwex = 1),
                lty = 1,
                staplewex = 0,
                outpch = 16,
                xlim = c(ats[1] - 1, ats[length(ats)] + 1),
                ylim = c(
                  ifelse(
                    sameaxes == TRUE,
                    max(c(out_lower, min(dat1$LBORRES[dat1[, labcolumn] == labpara], na.rm = TRUE))),
                    min(dattreat[dattreat[, labcolumn] == labpara,]$LBORRES, na.rm = TRUE)
                  ),
                  ifelse(
                    sameaxes == TRUE,
                    c(min(c(out_upper, max(dat1$LBORRES[dat1[, labcolumn] == labpara], na.rm = TRUE)))),
                    max(dattreat[dattreat[, labcolumn] == labpara,]$LBORRES, na.rm = TRUE))
                ), add = TRUE
              )
            }

            if (connect_lines == TRUE) {

              tmp <- dattreat[dattreat[, labcolumn] == labpara,] %>%
                dplyr::select(SUBJIDN, AVISIT, TRTP, LBORRES) %>%
                dplyr::group_by(SUBJIDN) %>%
                tidyr::spread(AVISIT,LBORRES) %>%
                dplyr::ungroup() %>%
                stats::na.omit() %>%
                dplyr::select(-c(SUBJIDN,TRTP))

              index <- t(apply(tmp, 1, diff))

              for(i in 2:dim(tmp)[2]) {
                apply(tmp[index[, i-1] < 0, (i - 1) : i], 1, function(x){unlist(x ,use.names = FALSE) %>%
                  graphics::lines(y = ., x = ats[(i-1):i], col = "#11c4d4")})
                apply(tmp[index[, i-1] > 0, (i - 1) : i], 1, function(x){unlist(x ,use.names = FALSE) %>%
                  graphics::lines(y = ., x = ats[(i-1):i], col = "#f78300")})
                apply(tmp[index[, i-1] == 0, (i - 1) : i], 1, function(x){unlist(x ,use.names = FALSE) %>%
                  graphics::lines(y = ., x = ats[(i-1):i], col = "darkgrey")})
              }
            }

            graphics::abline(
              h = unlist(unique(dattreat[dattreat[, labcolumn] == labpara, c("LBORNRLO", "LBORNRHI")])),
              col = "#f78300",
              lty = 3,
              lwd = 2
            )

            if (add_points == TRUE) {
              for (i in 1:nvisit) {

                nvals <- length(dattreat$LBORRES[dattreat$AVISIT == levels((dattreat$AVISIT))[i] & dattreat[,labcolumn] == labpara])

                if (nvals > 0) {
                  if (sortpoints == FALSE) {
                    graphics::points(
                      x = ats[i] + stats::runif(nvals,  min = -0.6, max = 0.6),
                      y = dattreat$LBORRES[dattreat$AVISIT == levels((dattreat$AVISIT))[i] & dattreat[,labcolumn]==labpara],
                      cex = 0.6,
                      col = bordercol[i]
                    )
                  } else {
                    graphics::points(
                      x = ats[i] + ((1:nvals)/nvals) - mean((1:nvals)/nvals),
                      y = sort(dattreat$LBORRES[dattreat$AVISIT == levels((dattreat$AVISIT))[i] & dattreat[,labcolumn]==labpara], na.last = TRUE),
                      cex = 0.4,
                      col = bordercol[i]
                    )
                  }

                  if (sameaxes == TRUE) {
                    vals <- dattreat$LBORRES[dattreat$AVISIT == levels((dattreat$AVISIT))[i] & dattreat[, labcolumn] == labpara]
                    nlow <- which(vals < out_lower)
                    nhig <- which(vals > out_upper)
                    if (length(nlow) > 0) {
                      graphics::arrows(x0 = ats[i], y0 = out_lower, x1 = ats[i], y1 = out_lower - ((out_upper-out_lower)/20), col = arrowcol, length = 0.05)
                      graphics::text(x = ats[i] - 0.2, y = out_lower, labels = round(sort(vals[nlow], decreasing = TRUE), digits = 1), pos = 4, cex = 0.7, col = textcol)
                    }
                    if (length(nhig) > 0) {
                      graphics::arrows(x0 = ats[i], y1 = out_upper, x1 = ats[i], y0 = out_upper - ((out_upper-out_lower)/20), col = arrowcol, length = 0.05)
                      graphics::text(x = ats[i] - 0.2, y = out_upper - ((out_upper-out_lower)/20*(length(nhig)/2)), labels = paste(round(sort(vals[nhig], decreasing = TRUE), digits = 1), collapse = "\n"), pos = 4, cex = cexoutliers, col = textcol)
                    }
                  }
                }
              }
            }

            graphics::mtext(mainlab, 3, line = 1, cex = 1.1)
            graphics::axis(side = 2, col = "lightgray")

            if (treat == levels(dat1$TRTP)[length(levels(dat1$TRTP))]) {
              graphics::text(ats, graphics::par("usr")[3], labels = labelvis, srt = 45, adj = c(1.1, 1.1), xpd = NA, cex = 1)
            }
          }
          if (labpara == sortinput[1]) {
            graphics::mtext(treat, side = 2, line = 3)
          }

          shiny::incProgress(1/(length(levels(dat1$TRTP)) * length(sortinput)), detail = paste(""))
        })

      })
      shiny::incProgress(0, detail = paste("done!"))

    }
  })
  } else {
      ColorBG <- "#E2F3F2"
      textcol <- arrowcol <- "#f78300"
      if (length((unique(dat1$TRTP))) == 0 |
         length(unique(dat1[, labcolumn])) == 0) {
        on_ex <- graphics::par("mfrow","bty","mar","oma","bg")
        on.exit(graphics::par(on_ex))
        graphics::par(
          mfrow = c(1,1),
          bty = "n",
          mar = c(1, 1, 1, 1),
          oma = c(0, 0, 0, 0),
          bg = ColorBG
        )
        graphics::plot(NULL, NULL, ylim = c(0, 1), xlim = c(0, 1), axes = FALSE, ylab = "", xlab = "")
        graphics::text(0.5 , 0.5 , paste0("No values for this Treatment"))
      } else {
        on_ex <- graphics::par("mfrow","bty","mar","oma","cex.main","bg")
        on.exit(graphics::par(on_ex))
        graphics::par(mfrow = c(length((unique(dat1$TRTP))),
                                length(unique(dat1[, labcolumn]))),
                      bty = "n",
                      mar = c(1, 3, 0, 0),
                      oma = c((max(nchar(levels(dat1$AVISIT)))/3), 2, 3, 0),
                      cex.main = 1.4,
                      bg = ColorBG)

        sapply(levels(dat1$TRTP), function(treat){

          dattreat <- dat1[dat1$TRTP == treat,]

          sapply(sortinput, function(labpara){

            ntreat <- length(unique(dattreat$TRTP))
            nvisit <- length(unique(dattreat %>%
                                      dplyr::filter(PARAMCD == labpara) %>%
                                      .$AVISIT))
            nlab <- length(unique(dattreat$LBTESTCD))

            if(is.null(cols)) cols <- rep(c(colBoxplot1, colBoxplot2, colBoxplot3, colBoxplot4),
                                          length = nvisit)
            if(is.null(labelvis)) labelvis <- levels(dat1$AVISIT)
            if(is.null(bordercol)) bordercol <- rep(c("black"), length = nvisit)

            mainlab <- ifelse(treat == levels(dat1$TRTP)[1],
                              paste(strwrap(paste(strsplit(as.character(labpara), split = "/")[[1]], collapse = " / "), width = 16),
                                    collapse = "\n"), "")

            ats_nr <- dattreat[dattreat[, labcolumn] == labpara,] %>%
              dplyr::pull(AVISIT) %>%
              unique() %>%
              length()

            ats <- 1 + (1.5 * (1:ats_nr - 1))

            labelvis <- dattreat[dattreat[, labcolumn] == labpara,] %>%
              droplevels() %>%
              dplyr::pull(AVISIT) %>%
              levels()

            out_lower_vec <- out_upper_vec <- vector()

            for(i in 1:length(unique(dattreat$AVISIT))){
              quan <- stats::quantile(
                dat1$LBORRES[dat1[, labcolumn] == labpara & dat1$AVISIT == levels(as.factor(as.character((dat1$AVISIT))))[i]],
                probs = c(0.25, 0.75),
                na.rm = TRUE
              )

              out <- (quan[2] - quan[1]) * 5
              out_lower_vec[i] <- quan[1] - out
              out_upper_vec[i] <- quan[2] + out
            }

            out_lower <- min(out_lower_vec, na.rm = TRUE)
            out_upper <- max(out_upper_vec, na.rm = TRUE)

            if((dattreat[dattreat[, labcolumn] == labpara,] %>%
                droplevels()) %>%
               dim() %>%
               .[1] == 0 ) {
              graphics::plot(NULL, NULL, ylim = c(0,1), xlim = c(0,1), axes = FALSE, ylab = "", xlab = "")
              graphics::text(0.5 , 0.5 , paste0("No values for this Treatment"))
            } else {
              graphics::boxplot(
                formula = LBORRES ~ AVISIT,
                data = dattreat[dattreat[, labcolumn] == labpara,] %>%
                  droplevels(),
                asp = 1,
                xaxt = "n",
                yaxt = "n",
                col = cols,
                border = bordercol,
                at = ats,
                pars = list(boxwex = 1),
                lty = 1,
                staplewex = 0,
                outpch = 16,
                xlim = c(ats[1] - 1, ats[length(ats)] + 1),
                ylim = c(
                  ifelse(sameaxes == TRUE,
                  max(c(out_lower, min(dat1$LBORRES[dat1[, labcolumn] == labpara], na.rm = TRUE))),
                  min(dattreat[dattreat[, labcolumn] == labpara,]$LBORRES, na.rm = TRUE)),
                  ifelse(
                    sameaxes == TRUE,
                    c(min(c(out_upper, max(dat1$LBORRES[dat1[, labcolumn] == labpara], na.rm = TRUE)))),
                    max(dattreat[dattreat[, labcolumn] == labpara,]$LBORRES, na.rm = TRUE)
                  )
                )
              )

              if (!any(is.na(infotest)) && !is.null(infotest)) {

                if (length(Visit2) == 1) {

                  if (infotest[[1]]$p.value[labpara,treat] <= pcutoff &
                     !is.na(infotest[[1]]$p.value[labpara, treat]) &
                     !is.null(infotest[[1]]$p.value[labpara, treat])) {

                    if (signtest == TRUE) {
                      est_test <- infotest[[1]]$estimate[labpara,treat] < 0.5
                    } else if (signtest == FALSE) {
                      est_test <- infotest[[1]]$estimate[labpara,treat] > 0
                    }

                    graphics::rect(
                      graphics::par("usr")[1],
                      graphics::par("usr")[3],
                      graphics::par("usr")[2],
                      graphics::par("usr")[4],
                      col = ifelse(
                        est_test,
                        "#47d2bc",
                        "#ffeeaa"
                      ),
                      border = NA
                    )
                  }
                  if (is.na(infotest[[1]]$p.value[labpara, treat]) |
                     is.null(infotest[[1]]$p.value[labpara, treat])) {
                    graphics::rect(
                      graphics::par("usr")[1],
                      graphics::par("usr")[3],
                      graphics::par("usr")[2],
                      graphics::par("usr")[4],
                      col = "#A9A9A9",
                      border = NA
                    )
                  }
                }

                if (length(Visit2) > 1) {

                  for (j in Visit2) {
                    i <- which(Visit2 == j)
                    k <- which(
                      levels(
                        dat1 %>%
                          dplyr::filter(PARAMCD == labpara) %>%
                          .$AVISIT %>%
                          droplevels()
                      ) == j
                    )

                    if (infotest[[i]]$p.value[labpara,treat] <= pcutoff &
                        !is.na(infotest[[i]]$p.value[labpara,treat]) &
                        !is.null(infotest[[i]]$p.value[labpara,treat])) {

                      if (signtest == TRUE) {
                        est_test <- infotest[[i]]$estimate[labpara,treat] < 0.5
                      } else if (signtest == FALSE) {
                        est_test <- infotest[[i]]$estimate[labpara,treat] > 0
                      }

                      sigcol <- ifelse(est_test, "#47d2bc", "#ffeeaa")

                      if (length(k) > 0) {
                        graphics::rect(
                          ats[k] - 0.75,
                          graphics::par("usr")[3],
                          ats[k] + 0.75,
                          graphics::par("usr")[4],
                          col = sigcol,
                          border = NA
                        )
                      }
                    }

                    if (is.na(infotest[[i]]$p.value[labpara, treat]) |
                        is.null(infotest[[i]]$p.value[labpara, treat])) {
                      if (length(k) > 0) {
                        graphics::rect(
                          ats[k] - 0.75,
                          graphics::par("usr")[3],
                          ats[k] + 0.75,
                          graphics::par("usr")[4],
                          col = "#A9A9A9",
                          border = NA
                        )
                      }
                    }
                  }
                }

                graphics::boxplot(
                  formula = LBORRES ~ AVISIT,
                  data = (
                    dattreat[dattreat[, labcolumn] == labpara,] %>%
                      droplevels()
                  ),
                  asp = 1,
                  xaxt = "n",
                  yaxt = "n",
                  col = cols,
                  border = bordercol,
                  at = ats,
                  pars = list(boxwex = 1),
                  lty = 1,
                  staplewex = 0,
                  outpch = 16,
                  xlim = c(ats[1] - 1, ats[length(ats)] + 1),
                  ylim = c(
                    ifelse(
                      sameaxes == TRUE,
                      max(c(out_lower, min(dat1$LBORRES[dat1[, labcolumn] == labpara], na.rm = TRUE))),
                      min(dattreat[dattreat[, labcolumn] == labpara,]$LBORRES, na.rm = TRUE)
                    ),
                    ifelse(
                      sameaxes == TRUE,
                      c(min(c(out_upper, max(dat1$LBORRES[dat1[, labcolumn] == labpara], na.rm = TRUE)))),
                      max(dattreat[dattreat[, labcolumn] == labpara,]$LBORRES, na.rm = TRUE)
                    )
                  ),
                  add = TRUE
                )
              }

              if (connect_lines == TRUE) {
                tmp <- dattreat[dattreat[, labcolumn] == labpara,] %>%
                  dplyr::select(SUBJIDN,AVISIT,TRTP,LBORRES) %>%
                  dplyr::group_by(SUBJIDN) %>%
                  tidyr::spread(AVISIT,LBORRES) %>%
                  dplyr::ungroup() %>% stats::na.omit() %>%
                  dplyr::select(-c(SUBJIDN,TRTP))

                index <- t(apply(tmp, 1, diff))

                for(i in 2:dim(tmp)[2]) {
                  apply(tmp[index[, i-1] < 0, (i - 1) : i], 1, function(x){unlist(x ,use.names = FALSE) %>%
                    graphics::lines(y = ., x = ats[(i-1):i], col = "#11c4d4")})
                  apply(tmp[index[, i-1]>=0, (i - 1) : i], 1, function(x){unlist(x ,use.names = FALSE) %>%
                    graphics::lines(y = ., x = ats[(i-1):i], col = "#f78300")})
                  apply(tmp[index[, i-1] == 0, (i - 1) : i], 1, function(x){unlist(x ,use.names = FALSE) %>%
                    graphics::lines(y = ., x = ats[(i-1):i], col = "darkgrey")})
                }
              }

              graphics::abline(
                h = unlist(unique(dattreat[dattreat[, labcolumn] == labpara, c("LBORNRLO", "LBORNRHI")])),
                col = "#f78300",
                lty = 3,
                lwd = 2
              )

              if (add_points == TRUE) {
                for(i in 1:nvisit) {

                  nvals <- length(dattreat$LBORRES[dattreat$AVISIT == levels((dattreat$AVISIT))[i] & dattreat[,labcolumn] == labpara])

                  if (nvals > 0) {
                    if (sortpoints == FALSE) {
                      graphics::points(
                        x = ats[i] + stats::runif(nvals,  min = -0.6, max = 0.6),
                        y = dattreat$LBORRES[dattreat$AVISIT == levels((dattreat$AVISIT))[i] & dattreat[,labcolumn]==labpara],
                        cex = 0.6,
                        col = bordercol[i]
                      )
                    } else {
                      graphics::points(
                        x = ats[i] + ((1:nvals)/nvals) - mean((1:nvals)/nvals),
                        y = sort(dattreat$LBORRES[dattreat$AVISIT == levels((dattreat$AVISIT))[i] & dattreat[,labcolumn]==labpara], na.last = TRUE),
                        cex = 0.4,
                        col = bordercol[i]
                      )
                    }

                    if (sameaxes == TRUE) {
                      vals <- dattreat$LBORRES[dattreat$AVISIT == levels((dattreat$AVISIT))[i] & dattreat[, labcolumn] == labpara]
                      nlow <- which(vals < out_lower)
                      nhig <- which(vals > out_upper)
                      if (length(nlow) > 0) {
                        graphics::arrows(
                          x0 = ats[i],
                          y0 = out_lower,
                          x1 = ats[i],
                          y1 = out_lower - ((out_upper-out_lower)/20),
                          col = arrowcol,
                          length = 0.05
                        )
                        graphics::text(
                          x = ats[i] - 0.2,
                          y = out_lower,
                          labels = round(sort(vals[nlow], decreasing = TRUE), digits = 1),
                          pos = 4,
                          cex = 0.7,
                          col = textcol
                        )
                      }
                      if (length(nhig) > 0) {
                        graphics::arrows(
                          x0 = ats[i],
                          y1 = out_upper,
                          x1 = ats[i],
                          y0 = out_upper - ((out_upper-out_lower)/20),
                          col = arrowcol,
                          length = 0.05
                        )
                        graphics::text(
                          x = ats[i] - 0.2,
                          y = out_upper - ((out_upper-out_lower)/20*(length(nhig)/2)),
                          labels = paste(round(sort(vals[nhig], decreasing = TRUE), digits = 1), collapse = "\n"),
                          pos = 4,
                          cex = cexoutliers,
                          col = textcol
                        )
                      }
                    }
                  }
                }
              }

              graphics::mtext(mainlab, 3, line = 1, cex = 1.1)
              graphics::axis(side = 2, col = "lightgray")

              if(treat == levels(dat1$TRTP)[length(levels(dat1$TRTP))]){
                graphics::text(
                  ats,
                  graphics::par("usr")[3],
                  labels = labelvis,
                  srt = 45,
                  adj = c(1.1, 1.1),
                  xpd = NA,
                  cex = 1
                )
              }
            }
            if (labpara == sortinput[1]) {
              graphics::mtext(treat, side = 2, line = 3)
            }
          })
        }
      )
    }
  }
}
