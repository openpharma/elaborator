#' Generates the reference-value based pattern analysis plots in the shiny app elaborator
#'
#' @description
#' Generates diagrams showing the frequency of normal and abnormal values at each visit of a study separately by treatment group and laboratory parameter according to the reference-value based pattern analysis.
#'
#' @param data data set
#' @param criterion criterion for defining whether abnormal values are defined as values below the lower limit of normal (less), above the upper limit of normal (greater) or either above the upper limit of normal or below the lower limit of normal (within)
#' @param fontsize font size of numbers printed inside the circles
#' @param sorting_vector vector of laboratory parameter names in the order in which laboratory parameters are presented in the output
#' @param abnormal_value_factor numeric which is multiplied with upper and/or lower limit of normal for defining abnormal values; can only take positive values.
#'
#' @return No return value, called to plot the reference-based value pattern.
#'
#' @keywords internal


elaborator_plot_ref_pattern <- function(
    data,
    criterion,
    fontsize = 0.6,
    sorting_vector = as.character(unique(data[, "LBTESTCD"])),
    abnormal_value_factor = 1
  ) {
  variable <- treat <- . <- subj <- upb <- upb2 <- lwb <- lwb2 <- difference_tmp <- dimension <- difference <- dimension2 <- NULL

  ColorBG <- "#E2F3F2"
  colRvbpPos <- "#2fb39f"
  colRvbpNeg <- "#f78300"

  if (length((unique(data$TRTP))) == 0 |
     length(unique(data[, "LBTESTCD"])) == 0) {

    # on_ex <- graphics::par("mfrow", "bty","mar","oma","bg")
    # on.exit(graphics::par(on_ex))
    graphics::par(mfrow = c(1, 1),
        bty = "n",
        mar = c(1, 1, 1, 1),
        oma = c(0, 0, 0, 0),
        bg = ColorBG)
    graphics::plot(NULL, NULL, ylim = c(0, 1), xlim = c(0, 1), axes = FALSE, ylab = "", xlab = "")
    graphics::rect(xleft = graphics::grconvertX(0, 'ndc', 'user'), xright = graphics::grconvertX(1,'ndc', 'user'),
         ybottom = graphics::grconvertY(0,'ndc', 'user'), ytop = graphics::grconvertY(1,'ndc', 'user'),
         border = NA, col = ColorBG, xpd = TRUE)
    graphics::text(0.5, 0.5, paste0("No values for this Treatment"))
  } else {

    if (length(unique(data$LBTESTCD))*length(unique(data$TRTP)) > 1) {
    shiny::withProgress(message = paste0('generating ', length(unique(data$LBTESTCD))*length(unique(data$TRTP)),' Plots ...'), value = 0, {
    shiny::incProgress(0, detail = paste(""))

    number <- function(p){
      as.vector((as.matrix(p) %*% as.matrix(2^((p %>% dim %>% .[2]-1):0))))
    }

    number2 <- function(p){
      res <- c()
      for(i in 1:nrow(p)){
        f <- 2^((length(p[i,]) - 1):0)
        res[i] <- sum(p[i, ] * f)
      }
      return(res)
    }

    D <- data.frame(subj = data$SUBJIDN,
                   treat = data$TRTP,
                   variable = data$LBTESTCD,
                   time = as.numeric(data$AVISIT),
                   value2 = data$LBORRES,
                   lwb  = as.numeric(data$LBORNRLO),
                   upb  = as.numeric(data$LBORNRHI))
    if(abnormal_value_factor >= 0 & abnormal_value_factor != 1) {
      D$lwb2 <- D$lwb * abnormal_value_factor
      D$upb2 <- D$upb * abnormal_value_factor
    }

    if (criterion == "within") {
      if (abnormal_value_factor == 1) {
        D$value = as.numeric(!((D$lwb <= D$value2) & (D$value2 <= D$upb)))
      } else {
        D$value = as.numeric(!((D$lwb2 <= D$value2) & (D$value2 <= D$upb2)))
      }
    } else if (criterion == "less") {
      if (abnormal_value_factor == 1) {
        D$value = as.numeric(!(D$lwb <= D$value2))
      } else {
        D$value = as.numeric(!(D$lwb2 <= D$value2))
      }
    } else if (criterion == "greater") {
      if (abnormal_value_factor == 1) {
        D$value = as.numeric(!(D$value2 <= D$upb))
      } else {
        D$value = as.numeric(!(D$value2 <= D$upb2))
      }
    }

    reducedData <- D[,c("subj", "treat", "variable", "time", "value")]

    Treats <- levels(reducedData$treat)

    reducedData_wide <- reshape2::dcast(reducedData, treat + variable + subj ~ time, drop = TRUE)


    reducedData_long <- reducedData_wide %>%
      dplyr::group_by(treat, variable) %>%
      dplyr::select(-subj) %>%
      tidyr::nest() %>%
      dplyr::mutate(difference_tmp = purrr::map(data, ~ .[, colSums(is.na(.)) != nrow(.)])) %>%
      dplyr::mutate(dimension = purrr::map(difference_tmp, ~ dim(.)[2])) %>%
      dplyr::mutate(difference = purrr::map(difference_tmp, ~ stats::na.omit(.))) %>%
      dplyr::select(-c(data, difference_tmp)) %>%
      dplyr::mutate(dimension2 = dimension %>%
               unlist) %>%
      #(1)
      dplyr::filter(dimension2 > 0) %>%
      dplyr::mutate(mz = purrr::map(difference, ~ number(.)))

    sortinput <- sorting_vector[sorting_vector %in% reducedData_long$variable]

    graphics::layout(matrix(1:(length(Treats) * length(sortinput)), length(Treats), length(sortinput)))

    # on_ex <- graphics::par("mfrow","mai","xaxs","yaxs","bg","fg","font","font.axis","font.lab","font.main","font.sub","ps","cex","family")
    # on.exit(graphics::par(on_ex))
    graphics::par(mfrow = c(length((unique(data$TRTP))), length(unique(sortinput))),
        mai = rep(0, 4), xaxs = "i", yaxs = "i",
        bg = ColorBG,
        fg = grDevices::rgb(140, 140, 140, maxColorValue = 255),
        font = 1, font.axis = 1, font.lab = 1, font.main = 1, font.sub = 1,
        ps = 5, cex = 1,
        family = "sans")

    for (treatments in Treats) {
      for (labvalues in sortinput) {

        dimension_tmp <- reducedData_long %>%
          dplyr::filter(variable == labvalues , treat == treatments) %>%
          dplyr::pull(dimension2)

        if (length(dimension_tmp) == 0) {
          graphics::plot(NULL, NULL, ylim = c(0, 1), xlim = c(0, 1), axes = FALSE, ylab = "", xlab = "")
          graphics::rect(xleft = graphics::grconvertX(0, 'ndc', 'user'), xright = graphics::grconvertX(1, 'ndc', 'user'),
               ybottom = graphics::grconvertY(0,'ndc','user'), ytop = graphics::grconvertY(1, 'ndc', 'user'),
               border = NA, col = ColorBG, xpd = TRUE)
          graphics::text(0.5, 0.5, paste0("No values for this Treatment"))

        } else {

          index <- 0:(2^dimension_tmp - 1)
          List_index <- lapply(index, function(i) elaborator_calculate_ref_pattern(i, dimension_tmp))

          CombinationMatrix <- matrix(unlist(List_index), 2^dimension_tmp, dimension_tmp, byrow = TRUE)
          colnames(CombinationMatrix) = paste("M", 1:dimension_tmp, sep = "")

          mg <- unlist(lapply(1:2^dimension_tmp, function(i) sum(CombinationMatrix[i,])))

          CombinationMatrix <- data.frame(CombinationMatrix, index)
          CombinationMatrix <- data.frame(CombinationMatrix[order(CombinationMatrix$index, decreasing = TRUE), ] , number = rep(0, 2^dimension_tmp))

          reducedData_long_tmp <- reducedData_long %>%
            dplyr::filter(variable == labvalues , treat == treatments)

          if (length(unlist(reducedData_long_tmp$mz)) > 0) {
            table_number <- table(unlist(reducedData_long_tmp$mz))
            q <- as.numeric(names(table_number))

            CombinationMatrix$number[elaborator_derive_equal_values(q, CombinationMatrix$index)] <- table_number
            #all
            elaborator_draw_ref_pattern(pattern_Matrix = CombinationMatrix, fontsize = fontsize, number_columns = dimension_tmp)

          } else {
            graphics::text((dimension_tmp - 1) / 2, (2^dimension_tmp + 1) / 2, "No Data Available!")
          }
        }
        if (treatments == Treats[1]) {
          graphics::text(0, (2^dimension_tmp + 1) - 1, labvalues, cex = 2)
        }

        if (labvalues == sortinput[1]) {
          graphics::text(-0.5, (2^dimension_tmp + 1) / 2, treatments, srt = 90, cex = 2)
        }


        shiny::incProgress(1/(length(sortinput) * length(Treats)), detail = paste(""))
      }
      shiny::incProgress(1/length(sortinput), detail = paste(""))
    }
    shiny::incProgress(0, detail = paste("done!"))
  })
  } else {

      number <- function(p) {
        as.vector((as.matrix(p) %*% as.matrix(2^((p %>% dim %>% .[2]-1):0))))
      }

      number2 <- function(p){
        res <- c()
        for(i in 1:nrow(p)){
          f <- 2^((length(p[i,]) - 1):0)
          res[i] <- sum(p[i, ] * f)
        }
        return(res)
      }

      D <- data.frame(subj = data$SUBJIDN,
                      treat = data$TRTP,
                      variable = data$LBTESTCD,
                      time = as.numeric(data$AVISIT),
                      value2 = data$LBORRES,
                      lwb  = as.numeric(data$LBORNRLO),
                      upb  = as.numeric(data$LBORNRHI))


     if(abnormal_value_factor >= 0 & abnormal_value_factor != 1) {
      D$lwb2 <- D$lwb * abnormal_value_factor
      D$upb2 <- D$upb * abnormal_value_factor
    }

    if (criterion == "within") {
      if (abnormal_value_factor == 1) {
        D$value = as.numeric(!((D$lwb <= D$value2) & (D$value2 <= D$upb)))
      } else {
        D$value = as.numeric(!((D$lwb2 <= D$value2) & (D$value2 <= D$upb2)))
      }
    } else if (criterion == "less") {
      if (abnormal_value_factor == 1) {
        D$value = as.numeric(!(D$lwb <= D$value2))
      } else {
        D$value = as.numeric(!(D$lwb2 <= D$value2))
      }
    } else if (criterion == "greater") {
      if (abnormal_value_factor == 1) {
        D$value = as.numeric(!(D$value2 <= D$upb))
      } else {
        D$value = as.numeric(!(D$value2 <= D$upb2))
      }
    }


      reducedData <- D[,c("subj", "treat", "variable", "time", "value")]

      Treats <- levels(reducedData$treat)

      reducedData_wide <- reshape2::dcast(reducedData, treat + variable + subj ~ time, drop = TRUE)

      reducedData_long <- reducedData_wide %>%
        dplyr::group_by(treat, variable) %>%
        dplyr::select(-subj) %>%
        tidyr::nest() %>%
        dplyr::mutate(difference_tmp = purrr::map(data, ~ .[, colSums(is.na(.)) != nrow(.)])) %>%
        dplyr::mutate(dimension = purrr::map(difference_tmp, ~ dim(.)[2])) %>%
        dplyr::mutate(difference = purrr::map(difference_tmp, ~ stats::na.omit(.))) %>%
        dplyr::select(-c(data, difference_tmp)) %>%
        dplyr::mutate(dimension2 = dimension %>%
                        unlist) %>%
        #(2)
        dplyr::filter(dimension2 > 0) %>%
        dplyr::mutate(mz = purrr::map(difference, ~ number(.)))

      sortinput <- sorting_vector[sorting_vector %in% reducedData_long$variable]

      graphics::layout(matrix(1:(length(Treats) * length(sortinput)), length(Treats), length(sortinput)))

      # on_ex <- graphics::par("mfrow","mai","xaxs","yaxs","bg","fg","font","font.axis","font.lab","font.main","font.sub","ps","cex","family")
      # on.exit(graphics::par(on_ex))
      graphics::par(mfrow = c(length((unique(data$TRTP))), length(unique(sortinput))),
                    mai = rep(0, 4), xaxs = "i", yaxs = "i",
                    bg = ColorBG,
                    fg = grDevices::rgb(140, 140, 140, maxColorValue = 255),
                    font = 1, font.axis = 1, font.lab = 1, font.main = 1, font.sub = 1,
                    ps = 5, cex = 1.2,
                    family = "sans")

      for (treatments in Treats) {
        for (labvalues in sortinput) {

          dimension_tmp <- reducedData_long %>%
            dplyr::filter(variable == labvalues , treat == treatments) %>%
            dplyr::pull(dimension2)
          if (length(dimension_tmp) == 0) {
            graphics::plot(NULL, NULL, ylim = c(0, 1), xlim = c(0, 1), axes = FALSE, ylab = "", xlab = "")
            graphics::rect(xleft = graphics::grconvertX(0, 'ndc', 'user'), xright = graphics::grconvertX(1, 'ndc', 'user'),
                           ybottom = graphics::grconvertY(0,'ndc','user'), ytop = graphics::grconvertY(1, 'ndc', 'user'),
                           border = NA, col = ColorBG, xpd = TRUE)
            graphics::text(0.5, 0.5, paste0("No values for this Treatment"))

          } else {

            index <- 0:(2^dimension_tmp - 1)
            List_index <- lapply(index, function(i) elaborator_calculate_ref_pattern(i, dimension_tmp))

            CombinationMatrix <- matrix(unlist(List_index), 2^dimension_tmp, dimension_tmp, byrow = TRUE)
            colnames(CombinationMatrix) = paste("M", 1:dimension_tmp, sep = "")

            mg <- unlist(lapply(1:2^dimension_tmp, function(i) sum(CombinationMatrix[i,])))

            CombinationMatrix <- data.frame(CombinationMatrix, index)
            CombinationMatrix <- data.frame(CombinationMatrix[order(CombinationMatrix$index, decreasing = TRUE), ] , number = rep(0, 2^dimension_tmp))

            reducedData_long_tmp <- reducedData_long %>%
              dplyr::filter(variable == labvalues , treat == treatments)

            if (length(unlist(reducedData_long_tmp$mz)) > 0) {
              table_number <- table(unlist(reducedData_long_tmp$mz))
              q <- as.numeric(names(table_number))

              CombinationMatrix$number[elaborator_derive_equal_values(q, CombinationMatrix$index)] <- table_number

              elaborator_draw_ref_pattern(pattern_Matrix = CombinationMatrix, fontsize = fontsize, number_columns = dimension_tmp)

              text(
                x = grconvertX(0, "npc", "user"),
                y = grconvertY(.2, "npc", "user"),
                labels = paste0("Upper limit normal:",
                  paste(
                    D %>%
                      dplyr::filter(treat == treatments & variable == labvalues) %>%
                      dplyr::pull(upb) %>%
                      unique(),
                    collapse = ", "
                  )
                ),
                adj = 0,
                cex = 2
              )

              text(
                x = grconvertX(0, "npc", "user"),
                y = grconvertY(.15, "npc", "user"),
                labels = paste0("Lower limit normal:",
                  paste(
                  D %>%
                    dplyr::filter(treat == treatments & variable == labvalues) %>%
                    dplyr::pull(lwb) %>%
                    unique(),
                  collapse = ", "
                  )
                ),
                adj = 0,
                cex = 2
              )

              if(abnormal_value_factor >= 0 & abnormal_value_factor != 1) {
                if (!criterion == "less") {
                  text(
                    x = grconvertX(0, "npc", "user"),
                    y = grconvertY(.1, "npc", "user"),
                    labels = paste0("Upper lower normal times factor (", abnormal_value_factor, ") : ",
                      paste(
                        D %>%
                          dplyr::filter(treat == treatments & variable == labvalues) %>%
                          dplyr::pull(upb2) %>%
                          unique() %>%
                          round(2),
                        collapse = ", "
                      )
                    ),
                    adj = 0,
                    col = "red",
                    cex = 2
                  )
                }
                if (!criterion == "greater") {
                  text(
                    x = grconvertX(0, "npc", "user"),
                    y = grconvertY(.05, "npc", "user"),
                      labels = paste0("Lower limit normal times factor (", abnormal_value_factor, ") : ",
                      paste(
                      D %>%
                        dplyr::filter(treat == treatments & variable == labvalues) %>%
                        dplyr::pull(lwb2) %>%
                        unique() %>%
                        round(2),
                      collapse = ", "
                      )
                    ),
                    adj = 0,
                    col = "red",
                    cex = 2
                  )
                }
              }
            } else {
              graphics::text((dimension_tmp - 1) / 2, (2^dimension_tmp + 1) / 2, "No Data Available!")
            }
          }
          if (treatments == Treats[1]){
            graphics::text(0, (2^dimension_tmp + 1) - 1, labvalues, cex = 2)
          }

          if (labvalues == sortinput[1]){
            graphics::text(-0.5, (2^dimension_tmp + 1) / 2, treatments, srt = 90, cex = 2)
          }
        }
      }
    }
  }
}
