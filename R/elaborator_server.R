#' Server part of the elaborator application
#'
#'@param id,input,output,session Internal parameters for {shiny}.
#'
#'@return No return value. Server part of the app, used in launch_elaborator-function.
#'
#'@keywords internal

elaborator_server <- function(input, output, session) {

  LBTESTCD <- TRTP <- AVISIT <- orderinglab <- median_value <- LBORRES_diff <- SUBJIDN <- tr <- data <- nonmissing <- tot <- percentage <- trt <- PARAMCD <- nonmiss <- nr_visits <- . <- LBORRES <- LBORNRLO <- LBORNRHI <- highref <- lowref <- InQuRa <- Range <- refRange <- LBORRES.y <- LBORRES.x <- vari <- js <- NULL

  colBoxplot4 <- "#004a8a"
  colBoxplot3 <- "#0075bc"
  colBoxplot2 <- "#00b4cb"
  colBoxplot1 <- "#2fb39f"

  colDecrease <- "#47d2bc"
  colIncrease <- "#ffeeaa"

  colLines <- "#f78300"

  colQualitative1 <- "#dff2fd"
  colQualitative2 <- "#c9e1f6"
  colQualitative3 <- "#b0d5f2"
  colQualitative4 <- "#95c7ed"
  colQualitative5 <- "#78b7e5"
  colQualitative6 <- "#57a7d9"
  colQualitative7 <- "#0092cd"
  colQualitative8 <- "#0082be"
  colQualitative9 <- "#0072a9"
  colQualitative10 <- "#00639b"
  colQualitative11 <- "#005c90"
  textcol <- "#f78300"
  arrowcol <- "#f78300"

  colRvbpPos <- "#2fb39f"
  colRvbpNeg <- "#f78300"

  ColorBG <- "#E2F3F2"
  ColorApp <- "#00b4cb"
  ColorPanel <- "#11c4d4"
  ColorHighlight <- "#f6ad82"
  ColorElements <- "#e3e3e3"
  ColorFont <- "#3c3c3b"

  colChoice <- list(
    'sequential orange' = list('col' = brewer.pal(9, 'Oranges'), 'gradient' = TRUE),
    'sequential blue'   = list('col' = brewer.pal(9, 'Blues'),   'gradient' = TRUE),
    'sequential green'  = list('col' = brewer.pal(9, 'Greens'),  'gradient' = TRUE),
    'sequential grey'   = list('col' = brewer.pal(9, 'Greys'),   'gradient' = TRUE),
    'sequential purple' = list('col' = brewer.pal(9, 'Purples'), 'gradient' = TRUE),
    'sequential red'    = list('col' = brewer.pal(9, 'Reds'),    'gradient' = TRUE),

    'sequential blue - green'  = list('col' = brewer.pal(9, 'BuGn'), 'gradient' = TRUE),
    'sequential blue - purple' = list('col' = brewer.pal(9, 'BuPu'), 'gradient' = TRUE),
    'sequential green - blue'  = list('col' = brewer.pal(9, 'GnBu'), 'gradient' = TRUE),
    'sequential orange - red'  = list('col' = brewer.pal(9, 'OrRd'), 'gradient' = TRUE),
    'sequential purple - blue' = list('col' = brewer.pal(9, 'PuBu'), 'gradient' = TRUE),

    'sequential purple - blue - green'= list('col' = brewer.pal(9, 'PuBuGn'),'gradient' = TRUE),
    'sequential purple - red'         = list('col' = brewer.pal(9, 'PuRd'),  'gradient' = TRUE),
    'sequential red - purple'         = list('col' = brewer.pal(9, 'RdPu'),  'gradient' = TRUE),

    'sequential yellow - green'         = list('col' = brewer.pal(9, 'YlGn'),  'gradient' = TRUE),
    'sequential yellow - green - blue'  = list('col' = brewer.pal(9, 'YlGnBu'),'gradient' = TRUE),
    'sequential yellow - orange - brown'= list('col' = brewer.pal(9, 'YlOrBr'),'gradient' = TRUE),
    'sequential yellow - orange - red'  = list('col' = brewer.pal(9, 'YlOrRd'),'gradient' = TRUE)
  )


  #### Import data ####
  output$impdata <- shiny::renderUI({
    if (input$impswitch == '*.RData file') {
      shiny::fileInput(
        inputId = 'file',
        label = 'Choose RData file',
        multiple = FALSE,
        accept = '.RData'
      )
    } else if (input$impswitch == '*.CSV file') {
      shiny::tagList(
        shiny::fixedRow(
          shiny::fileInput(
            inputId = 'csv_file',
            label = 'Choose CSV file',
            multiple = TRUE,
            accept = c('text/csv',
                       'text/comma-separated-values,text/plain',
                       '.csv'
            )
          ),
          shinyWidgets::prettyRadioButtons(
            inputId = 'sep',
            label = 'Select separator',
            inline = TRUE,
            choices = c('Comma' = ',',
                        'Semicolon' = ';',
                        'Tab' = '\t'),
            status ="warning",
            animation = "smooth",
            selected = ','
          ),
          shinyWidgets::prettyRadioButtons(
            inputId = 'quote',
            label = 'Select quote',
            inline = TRUE,
            choices = c(
              None = '',
              'Double Quote (")' = '"',
              "Single Quote (')" = "'"
            ),
            selected = '"',
            status ="warning",
            animation = "smooth"
          ),
          shinyWidgets::prettyRadioButtons(
            inputId = 'dec',
            label = 'Select decimal character',
            status ="warning",
            animation = "smooth",
            inline = TRUE,
            choices= c(
              'Point (.)' = '.',
              'Comma (,)' = ','
            ),
            selected = '.'
          )
        )
      )
    }
  })

  output$hover <- shiny::renderPlot({
    input$apply_quant_plot
    shiny::req(shiny::isolate(ds2()), input$dist_hover)

    if (input$dist_hover$coords_css$y > 0 & input$dist_hover$coords_css$x > 0) {
      y <- input$dist_hover$coords_css$y
      x <- input$dist_hover$coords_css$x
      if(!is.null(y) && !is.null(x)){
        dat <- shiny::isolate(ds2())
        val <- shiny::isolate(values$default)
        signtest2 <- shiny::isolate(input$stattest)
        if (signtest2 == "signtest") {
          signtest <- TRUE
        } else {
          signtest <- FALSE
        }
        sortin <- clust()
        sortin <- sortin[sortin %in% shiny::isolate(input$select.lab)]
        T1 <- shiny::isolate(input$trtcompar[1])
        T2 <- shiny::isolate(input$trtcompar[-1])

        dat_filt <- dat %>%
          dplyr::filter(
            TRTP == dat %>%
              dplyr::pull(TRTP) %>%
              levels() %>%
              .[ceiling(y / isolate(input$zoompx))], PARAMCD == sortin[ceiling(x / isolate(input$zoompx))]
          )

        dat_filt$TRTP <- factor(dat_filt$TRTP)
        cho <- shiny::isolate(input$trtcompar)
        sortpoint <- shiny::isolate(input$sortpoint)
        labelvis <- NULL
        sameax <- shiny::isolate(input$sameaxes)
        pval <- shiny::isolate(input$pcutoff)
        if (input$go != 0) {
          b.col <- box_col()
        } else {
          b.col <- c(colBoxplot2, colBoxplot2, colBoxplot2, colBoxplot2)
        }
        if (signtest2 != "none") {
          bordcol <- shiny::isolate(border.col())
        } else {
          bordcol <- NULL
        }
        add_points <- shiny::isolate(input$add_points)

        con_lin <- shiny::isolate(input$con_lin)

        if (!is.list(val) | length(T1) < 1 | length(T2) < 1) {
          info <- NA
        } else {
          info <- elaborator_derive_test_values(
            data = dat_filt,
            signtest = signtest,
            Visit1 = T1,
            Visit2 = T2,
            lab_column = "PARAMCD"
          )
        }

        elaborator_plot_quant_trends(
          dat1 = dat_filt,
          signtest = signtest,
          Visit1 = cho[1],
          Visit2 = cho[-1],
          labcolumn = "PARAMCD",
          cols = b.col,
          pcutoff = pval,
          sameaxes = sameax,
          sortpoints = sortpoint,
          labelvis = labelvis,
          cexoutliers = 0.5,
          infotest = info,
          sortinput = sortin[ceiling(x / input$zoompx)],
          bordercol = bordcol,
          add_points = add_points,
          connect_lines = con_lin
        )
      }
    } else {
      plot(
        NULL,
        xlim = c(0, 1),
        ylim = c(0, 1),
        axes = FALSE,
        xlab = "",
        ylab = ""
      )
      rect(
        xleft = grconvertX(0,'ndc','user'),
        xright = grconvertX(1, 'ndc', 'user'),
        ybottom = grconvertY(0,'ndc','user'),
        ytop = grconvertY(1, 'ndc', 'user'),
        border = NA,
        col = ColorBG,
        xpd = TRUE
      )
      text(0.5, 0.6, "Please move your mouse over the plots", col = ColorFont)
      text(0.5, 0.4, "to get an enlarged version of the plot!", col = ColorFont)
    }
  }, width = 400
  )

  output$hoverpanel <- shiny::renderUI({
    shiny::absolutePanel(
      id = "hoverpanel",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0("<div style='background-color:", ColorBG, "'>")),
      HTML('<button style = "background: #f6ad82; color:#ffffff", data-toggle="collapse" data-target="#demo" style="color:white;"> <i class="fa fa-search-plus"></i> Open/Close Zoom Panel</button>'),
      top = 70,
      left = "auto",
      right = 100,
      bottom = "auto",
      width = 400,
      height = "auto",
      tags$div(id = 'demo',
               class = "collapse",
               shiny::fluidRow(
                 shiny::column(2,
                               shiny::plotOutput('hover')
                 )
               )
      ),
      style = "z-index: 10;"
    )
  })

  output$dendro_1 <- shiny::renderPlot({
    shiny::req(pre_clust(), shiny::isolate(input$clusterMethod))
    if ((startsWith(shiny::isolate(input$clusterMethod), "OLO") | startsWith(shiny::isolate(input$clusterMethod), "GW"))) {
      tmp <- pre_clust()
      ser <- seriation::seriate(elaborator_calculate_spearman_distance(tmp), method = shiny::isolate(input$clusterMethod))
      asdendro <- stats::as.dendrogram(ser[[1]])
      dendro <- dendextend::assign_values_to_leaves_edgePar(dend = asdendro)
      graphics::rect(
        xleft = graphics::grconvertX(0, 'ndc', 'user'),
        xright = graphics::grconvertX(1,'ndc', 'user'),
        ybottom = graphics::grconvertY(0, 'ndc', 'user'), ytop = graphics::grconvertY(1,'ndc', 'user'),
        border = NA,
        col = ColorBG,
        xpd = TRUE
      )
      on_ex <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(on_ex))
      graphics::par(bg = ColorBG)
      graphics::plot(dendro, ylab = "Distance", horiz = FALSE)
    }
  })

  #### QUALITATIVE TRENDS ####
  output$dendro_2 <- shiny::renderPlot({
    if ((startsWith(shiny::isolate(input$clusterMethod), "OLO") | startsWith(shiny::isolate(input$clusterMethod), "GW"))) {
      shiny::req(pre_clust(), shiny::isolate(input$clusterMethod))
      tmp <- pre_clust()
      ser <- seriation::seriate(elaborator_calculate_spearman_distance(tmp), method = shiny::isolate(input$clusterMethod))
      asdendro <- stats::as.dendrogram(ser[[1]])
      dendro2 <- dendextend::assign_values_to_leaves_edgePar(dend = asdendro)

      graphics::rect(
        xleft = graphics::grconvertX(0,'ndc','user'),
        xright = graphics::grconvertX(1, 'ndc', 'user'),
        ybottom = graphics::grconvertY(0,'ndc','user'),
        ytop = graphics::grconvertY(1, 'ndc', 'user'),
        border = NA,
        col = ColorBG,
        xpd = TRUE
      )
      on_ex <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(on_ex))
      graphics::par(bg = ColorBG)
      graphics::plot(dendro2, ylab = "Distance", horiz = FALSE)
    }
  })

  output$legend <- shiny::renderPlot({
    on_ex <- graphics::par("mfrow","oma","mar")
    on.exit(graphics::par(on_ex))
    graphics::par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(0,0,0,0))
    graphics::plot(
      NULL,
      NULL,
      ylim = c(0,10),
      xlim = c(0,1),
      axes = FALSE,
      ylab = "",
      xlab = ""
    )
    leg.x <- 0.5
    leg.y <- seq(graphics::grconvertY(0, 'npc', 'user'), graphics::grconvertY(1, 'npc', 'user'), length.out = 12)
    leg.width <- 1
    graphics::rect(
      xleft = leg.x - 2,
      xright = leg.x + 2,
      ybottom = leg.y[-1],
      ytop = leg.y[-length(leg.y)],
      xpd = NA,
      col = c(c('white', colChoice[[shiny::req(input$select.pal1)]]$col, 'black')),
      border = TRUE
    )
    graphics::text(
      x = leg.x,
      y = leg.y[-1] - 0.5,
      labels = c(
        "0-5%",
        "5-10%",
        "10-15%",
        "15-20%",
        "20-25%",
        "25-30%",
        "30-35%",
        "35-40%",
        "40-45%",
        "45-50%",
        ">50%"
      ),
      col = c(
        'black','black','black','black','black','black',
        'white','white','white','white','white'
      )
    )
  }, width = 84.53)

  output$legendpanel <- shiny::renderUI({
    shiny::absolutePanel(
      id = "legendpanel",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      top = 240,
      left = "auto",
      right = 50,
      bottom = "auto",
      width = 84.53,
      height = "auto",

      HTML('<button style="background: #f6ad82; color:#ffffff", data-toggle="collapse" data-target="#demo_co">Open/Close</button>'),
      tags$div(id = 'demo_co',  class="collapse in",

               shiny::fluidRow(
                 shiny::column(2,
                               shiny::plotOutput(
                                 'legend'
                               )
                 )
               )

      ),
      style = "z-index: 10;"
    )
  })

  output$hoverpanel2 <- shiny::renderUI({

    nvi <- data_param()$nvisit

    shiny::absolutePanel(
      id = "hoverpanel2",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0("<div style='background-color:", ColorBG, "'>")),
      HTML('<button style="background: #f6ad82; color:#ffffff", data-toggle="collapse" data-target="#demo2"> <i class="fa fa-search-plus"></i> Open/Close Zoom Panel</button>'),
      top = 70,
      left = "auto",
      right = 100,
      bottom = "auto",
      width = nvi  * 100,
      height = "auto",
      tags$div(id = 'demo2',
               class = "collapse",
               shiny::fluidRow(
                 shiny::column(2,
                               shiny::plotOutput(
                                 'hover2'
                               )
                 )
               )
      ),
      style = "z-index: 10;"
    )
  })

  shiny::observe({
    output$hover2 <- shiny::renderPlot({
      input$apply_qual_plot
      shiny::req(ds2(), input$dist_hover2, Summa())
      if (input$dist_hover2$coords_css$y > 0 & input$dist_hover2$coords_css$x > 0) {
        dat <- ds2()
        Variab <- clust()

        Variab <- Variab[Variab %in% isolate(input$select.lab)]
        dat_filt <- dat %>%
          dplyr::filter(
            TRTP == dat %>%
              dplyr::pull(TRTP) %>%
              levels() %>%
              .[ceiling(input$dist_hover2$coords_css$y / input$zoompx)],
            PARAMCD == Variab[ceiling(input$dist_hover2$coords_css$x / input$zoompx)]
          )

        dat_filt$TRTP <- factor(dat_filt$TRTP)

        Summa  <- Summa()

        meth <- input$method
        suppressWarnings(
          elaborator_plot_qual_trends(
            dat1 = dat_filt,
            Variab[ceiling(input$dist_hover2$coords_css$x / input$zoompx)],
            fontsize = 2,
            method = meth,
            color_palette = c('white', colChoice[[shiny::req(input$select.pal1)]]$col, 'black'),
            Summa = Summa
          )
        )
      } else {
        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          axes = FALSE,
          xlab = "",
          ylab = ""
        )
        rect(
          xleft = grconvertX(0,'ndc','user'),
          xright = grconvertX(1, 'ndc', 'user'),
          ybottom = grconvertY(0,'ndc','user'),
          ytop = grconvertY(1, 'ndc', 'user'),
          border = NA,
          col = ColorBG,
          xpd = TRUE
        )
        text(0.5, 0.6, "Please move your mouse over the plots", col = ColorFont)
        text(0.5, 0.4, "to get an enlarged version of the plot!", col = ColorFont)
      }
    }, width = data_param()$nvisit * 100)
  })

  #### REFERENCE VALUE BASED PATTERN ####


  output$dendro_3 <- shiny::renderPlot({
    shiny::req(pre_clust(), shiny::isolate(input$clusterMethod))
    if ((startsWith(shiny::isolate(input$clusterMethod), "OLO") | startsWith(shiny::isolate(input$clusterMethod), "GW"))) {
      tmp <- pre_clust()
      ser <- seriation::seriate(
        elaborator_calculate_spearman_distance(tmp),
        method = shiny::isolate(input$clusterMethod)
      )
      asdendro <- stats::as.dendrogram(ser[[1]])
      dendro3 <- dendextend::assign_values_to_leaves_edgePar(dend = asdendro)

      graphics::rect(
        xleft = graphics::grconvertX(0,'ndc','user'),
        xright = graphics::grconvertX(1,'ndc','user'),
        ybottom = graphics::grconvertY(0,'ndc','user'),
        ytop = graphics::grconvertY(1,'ndc','user'),
        border = NA,
        col = ColorBG,
        xpd = TRUE
      )
      on_ex <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(on_ex))
      graphics::par(bg = ColorBG)
      graphics::plot(dendro3, ylab = "Distance", horiz = FALSE)
    }
  })


  output$hoverpanel3 <- shiny::renderUI({
    shiny::absolutePanel(
      id = "hoverpanel3",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0("<div style='background-color:", ColorBG, "'>")),
      HTML('<button style="background: #f6ad82; color:#ffffff", data-toggle="collapse" data-target="#demo3"> <i class="fa fa-search-plus"></i> Open/Close Zoom Panel</button>'),
      top = 70,
      left = "auto",
      right = 100,
      bottom = "auto",
      width = 400,
      height = "auto",
      tags$div(
        id = 'demo3',
        class = "collapse",
        shiny::fluidRow(
          shiny::column(2,
                        shiny::plotOutput('hover3')
          )
        )
      ),
      style = "z-index: 10;"
    )
  })

  output$hover3 <- shiny::renderPlot({
    input$apply_ref_plot
    shiny::req(ds2(), input$dist_hover3, input$abnormal_values_factor)
    if (input$dist_hover3$coords_css$y > 0 & input$dist_hover3$coords_css$x > 0
        & !is.na(input$abnormal_values_factor) & input$abnormal_values_factor >=0) {
      dat <- ds2()

      dat <-  subset(dat,!(dat$LBORNRLO == "" & dat$LBORNRHI == ""))

      dat$PARAMCD <- factor(dat$PARAMCD)

      sorti <- clust()
      sorti <- sorti[sorti %in% levels(dat$PARAMCD)]

      dat_filt <- dat %>%
        dplyr::filter(TRTP == dat %>%
                        dplyr::pull(TRTP) %>%
                        levels() %>%
                        .[ceiling(input$dist_hover3$coords_css$y / input$zoompx)], PARAMCD == sorti[ceiling(input$dist_hover3$coords_css$x / input$zoompx)])
      dat_filt$TRTP <- factor(dat_filt$TRTP)

      cex <- shiny::isolate(input$cex.rvbp)
      crit <- input$criterion

      elaborator_plot_ref_pattern(
        data = dat_filt,
        fontsize = 2,
        criterion = crit,
        sorting_vector = sorti[ceiling(input$dist_hover3$coords_css$x / input$zoompx)],
        abnormal_value_factor = shiny::isolate(input$abnormal_values_factor)
      )

    } else {
      plot(
        NULL,
        xlim = c(0, 1),
        ylim = c(0, 1),
        axes = FALSE,
        xlab = "",
        ylab = ""
      )
      rect(
        xleft = grconvertX(0, 'ndc', 'user'),
        xright = grconvertX(1, 'ndc', 'user'),
        ybottom = grconvertY(0, 'ndc', 'user'),
        ytop = grconvertY(1, 'ndc', 'user'),
        border = NA,
        col = ColorBG,
        xpd = TRUE
      )
      text(0.5, 0.6, "Please move your mouse over the plots", col = ColorFont)
      text(0.5, 0.4, "to get an enlarged version of the plot!", col = ColorFont)
    }
  }, width = 400)

  output$orderinglab <- shiny::renderUI({
    shiny::req(ds2())
    shinyWidgets::prettyRadioButtons(
      inputId = "orderinglab",
      label = "",
      choices = c(
        "As in input" = "asinp",
        "AI sorted" = "auto",
        "Alphabetically" = "alphabetically"
      ),
      selected = orderinglab$val,
      status = "warning"
    )
  })

  output$prev.pal1 <- shiny::renderPlot({
    col <- c('white', colChoice[[shiny::req(input$select.pal1)]]$col, 'black')
    elaborator_draw_scheme_preview(x = col)
  })


  #### REACTIVE OBJECTS ####
  #### reactiveValues ####
  options(shiny.maxRequestSize = 50 * 1024 ^ 2)

  start <- shiny::reactiveValues(dat = FALSE)

  start.ai <- shiny::reactiveValues(dat = FALSE)

  method <- shiny::reactiveValues(val = "InQuRa")

  values <- shiny::reactiveValues(default = 0)

  #### reactive ####
  output$flag <- shiny::reactive(start$dat)

  shiny::outputOptions(output, "flag", suspendWhenHidden = FALSE)

  output$ai <- shiny::reactive(start.ai$dat)

  shiny::outputOptions(output, "ai", suspendWhenHidden = FALSE)

  output$check <- shiny::reactive({
    length(input$trtcompar)
  })

  shiny::outputOptions(output, 'check', suspendWhenHidden = FALSE)

  df <- shiny::reactive({
    input$impswitch
    # need a non-empty data path
    if (!is.null(input$file$datapath) || !is.null(input$csv_file$datapath)) {

      required_elaborator_vars <- c("SUBJIDN", "AVISIT", "TRTP", "LBTESTCD", "LBORRES", "LBORNRLO", "LBORNRHI")

      if (input$impswitch == '*.RData file') {

        if (!is.null(input$file$datapath)) {
          # error message if selected data have a different format than rdata
          if (utils::tail(strsplit(input$file$datapath, ".", fixed = TRUE)[[1]], n = 1) != "RData") {
            elaborator_data <- NULL
            error_message <- paste0(
              "Wrong data format. <br> You have selected a ",
              utils::tail(strsplit(input$file$datapath, ".", fixed = TRUE)[[1]], n = 1),
              " file. <br> Please select a .RData file <br> or choose another file format."
            )
            return(
              list(
                data = elaborator_data,
                message = error_message
              )
            )
          } else {
            elaborator_data <- get(load(input$file$datapath))

            # error message if required variables are missing
            if (!all(required_elaborator_vars %in% names(elaborator_data))) {
              error_message <- paste0("The following required variable(s) <br> is/are missing: <br>",
                                      paste(required_elaborator_vars[which(!required_elaborator_vars %in% names(elaborator_data))], collapse = ", "),
                                      ".<br> Please check the data manual <br> for further information."
              )
              elaborator_data <- NULL
            } else {
              elaborator_data$LBTESTCD <- as.factor(elaborator_data$LBTESTCD)
              if ("SUBJIDN" %in% names(elaborator_data) && !("SUBJID" %in% names(elaborator_data))) {
                elaborator_data$SUBJID <- as.character(elaborator_data$SUBJIDN)
              }
              if ("SUBJID" %in% names(elaborator_data) && !("SUBJIDN" %in% names(elaborator_data))) {
                elaborator_data$SUBJIDN <- as.numeric(elaborator_data$SUBJID)
              }
              error_message <- NULL
            }
          }
        } else {
          elaborator_data <- NULL
          error_message <- NULL
        }
      } else if (input$impswitch == '*.CSV file') {

        if (!is.null(input$csv_file$datapath)) {
          # error message if selected data have a different format than csv
          if (utils::tail(strsplit(input$csv_file$datapath, ".", fixed = TRUE)[[1]], n = 1) != "csv") {
            elaborator_data <- NULL
            error_message <- paste0(
              "Wrong data format. <br> You have selected a ",
              utils::tail(strsplit(input$csv_file$datapath, ".", fixed = TRUE)[[1]], n = 1),
              " file. <br> Please select a .csv file <br> or choose another file format."
            )
          } else {
            elaborator_data <- utils::read.csv(
              input$csv_file$datapath,
              row.names = NULL,
              header = TRUE,
              na.strings = c('NA','.',''),
              sep = input$sep,
              quote = input$quote,
              dec = input$dec
            )

            if ("LBORRES" %in% names(elaborator_data)) {
              if (!is.numeric(elaborator_data$LBORRES)) {
                elaborator_data <- NULL
                error_message <- "Non numeric lab parameter. <br> Select another decimal character!"
                return(list(data = elaborator_data,
                            message = error_message))
              }
            }
            # error message if required variables are missing
            if (!all(required_elaborator_vars %in% names(elaborator_data))) {

              if (all(required_elaborator_vars %in% (strsplit(names(elaborator_data), ".", fixed = TRUE)[[1]]))) {
                error_message <- paste0(
                  "Please change separator and/or quote <br> input as in csv data set. <br>",
                  "For further information <br> check the data manual."
                )
                elaborator_data <- NULL
              } else {
                error_message <- paste0("The following required variable(s) <br> is/are missing: <br>",
                                        paste(required_elaborator_vars[which(!required_elaborator_vars %in% names(elaborator_data))], collapse = ", <br>"),
                                        ". <br> Try to change separator and/or quoute <br> input as in csv data set.
              <br> For further information <br> check the data manual."
                )
                elaborator_data <- NULL
              }
            } else {

              if("LBORNRHI" %in% colnames(elaborator_data)) {
                elaborator_data$LBORNRHI <- as.character(elaborator_data$LBORNRHI)
                elaborator_data$LBORNRLO <- as.character(elaborator_data$LBORNRLO)
                elaborator_data$LBTESTCD <- as.factor(elaborator_data$LBTESTCD)
                if ("SUBJIDN" %in% names(elaborator_data) && !("SUBJID" %in% names(elaborator_data))) {
                  elaborator_data$SUBJID <- as.character(elaborator_data$SUBJIDN)
                }
                if ("SUBJID" %in% names(elaborator_data) && !("SUBJIDN" %in% names(elaborator_data))) {
                  elaborator_data$SUBJIDN <- as.numeric(elaborator_data$SUBJID)
                }
                error_message <- NULL
              } else {
                elaborator_data <- NULL
              }
            }
            elaborator_data
          }
        } else {
          elaborator_data <- NULL
          error_message <- NULL
        }
      }
    } else {
      elaborator_data <- NULL
      error_message <- NULL
    }
    list(data = elaborator_data,
         message = error_message
    )
  })


  output$err_message <- renderText({
    if(!is.null(df()$message)) {
      str1 <- df()$message
      paste(str1)
    }
  })

  ds <- shiny::reactive({
    shiny::req(df()$data, input$select.toleratedPercentage)
    dat1 <- df()$data
    tolPerc <- input$select.toleratedPercentage
    shiny::withProgress(message = 'removing empty groups ...', value = 0, {
      shiny::incProgress(0, detail = paste(""))
      nest_dat1 <- dat1 %>%
        dplyr::group_by(AVISIT, TRTP, LBTESTCD) %>%
        tidyr::nest_legacy()

      shiny::incProgress(0.09, detail = paste(""))

      nonmissing <- unlist(
        lapply(
          nest_dat1 %>%
            pull(data),
          function(x){sum(!is.na(x$LBORRES))}
        )
      )

      shiny::incProgress(0.11, detail = paste(""))

      nonMissingVisits <- cbind(
        nest_dat1 %>%
          select(-data),
        nonmissing
      )

      shiny::incProgress(0.13, detail = paste(""))

      tmp1.2 <- dat1 %>%
        dplyr::select(SUBJIDN,TRTP) %>%
        dplyr::distinct() %>%
        dplyr::group_by(TRTP) %>%
        dplyr::summarise(tot = n()) %>%
        dplyr::full_join(dat1, by ="TRTP") %>%
        dplyr::full_join(
          nonMissingVisits,
          by = c("AVISIT","TRTP","LBTESTCD")
        ) %>%
        dplyr::mutate(percentage = nonmissing/tot)

      shiny::incProgress(0.15, detail = paste(""))

      tmp2.2 <- tmp1.2 %>%
        dplyr::group_by(AVISIT, LBTESTCD) %>%
        dplyr::summarise(tr = min(percentage))

      tmp3.2 <- tmp2.2 %>%
        dplyr::full_join(tmp1.2, by = c("AVISIT","LBTESTCD")) %>%
        dplyr::filter(tr >= tolPerc) %>%
        dplyr::select(colnames(dat1)) %>%
        dplyr::ungroup()

      tmp3.2 <- tmp3.2 %>%
        dplyr::mutate(LBTESTCD = forcats::fct_explicit_na(LBTESTCD, "NA"))

      countVisits.2 <- tmp3.2 %>%
        dplyr::group_by(LBTESTCD) %>%
        dplyr::group_modify(~ data.frame(nr_visits = length(unique(.x$AVISIT))))

      shiny::incProgress(0.19, detail = paste(""))

      tmp3.2_gb <- tmp3.2 %>%
        dplyr::group_by(SUBJIDN, LBTESTCD) %>%
        tidyr::nest_legacy()

      shiny::incProgress(0.95, detail = paste(""))

      nonmiss <- unlist(
        lapply(
          tmp3.2_gb %>%
            dplyr::pull(data),
          function(x){sum(!is.na(x$LBORRES))}
        )
      )

      nonMissing.2 <- cbind(tmp3.2_gb %>%
                              dplyr::select(SUBJIDN, LBTESTCD), nonmiss)

      shiny::incProgress(0.99, detail = paste(""))

      res.2 <- nonMissing.2 %>%
        dplyr::full_join(countVisits.2, by = c("LBTESTCD")) %>%
        dplyr::filter(nr_visits == nonmiss) %>%
        dplyr::ungroup() %>%
        dplyr::select(SUBJIDN, LBTESTCD)

      shiny::incProgress(0, detail = paste("done!"))
    })
    as.data.frame(
      tmp1.2 %>%
        dplyr::right_join(res.2, by = c("SUBJIDN","LBTESTCD")) %>%
        dplyr::select(colnames(tmp3.2))
    )
  })


  ds2 <- shiny::reactive({
    shiny::req(input$select.treatments, input$select.lab, input$select.visit, ds(), df()$data)
    df <- ds()

    choices.trt <- input$select.treatments
    choices.lab <- input$select.lab
    choices.visit <- input$select.visit
    ds <- df %>%
      dplyr::filter(LBTESTCD %in% c(choices.lab) & TRTP %in% c(choices.trt) & AVISIT %in% c(choices.visit))
    if(dim(ds)[1] > 0) {
      ds$AVISIT <- factor(ds$AVISIT)

      if(shiny::isolate(input$orderinglab) == "asinp") {
        ds$LBTESTCD <- factor(ds$LBTESTCD,levels = unique((ds$LBTESTCD)))
        ds$PARAMCD <- factor(ds$LBTESTCD,levels = unique((ds$LBTESTCD)))
      } else if (shiny::isolate(input$orderinglab) =="alphabetically"){
        ds$LBTESTCD <- factor(ds$LBTESTCD,levels = sort(levels(ds$LBTESTCD)))
        ds$PARAMCD <- factor(ds$LBTESTCD,levels = sort(levels(ds$LBTESTCD)))
      } else if (shiny::isolate(input$orderinglab) =="auto"){
        ds$LBTESTCD <- factor(ds$LBTESTCD,levels = levels(ds$LBTESTCD))
        ds$PARAMCD <- factor(ds$LBTESTCD,levels = levels(ds$LBTESTCD))
      }
      ds$TRTP <- factor((ds$TRTP))
      ds$TRTP <- ds$TRTP %>%
        forcats::fct_relevel(input$select.treatments)
      ds$AVISIT <- ds$AVISIT %>%
        forcats::fct_relevel(input$select.visit)
      ds
    }
  })

  data_param <- shiny::reactive({
    shiny::req(ds2())
    ntreat <- length(unique(ds2()$TRTP))
    nvisit <- length(unique(ds2()$AVISIT))
    nlab <- length(unique(ds2()$PARAMCD))
    tmp <- ds2()
    tmp <- subset(tmp,!(tmp$LBORNRLO == "" & tmp$LBORNRHI == ""))
    nlab2 <- length(unique(tmp$PARAMCD))
    list(ntreat = ntreat, nvisit = nvisit, nlab = nlab, nlab2 = nlab2)
  })

  Summa <-  shiny::reactive({
    shiny::req(ds2(), input$percent)
    dat1 <- ds2()

    percent <- input$percent/100
    firstVisit <- dat1 %>%
      dplyr::pull(AVISIT)%>%
      levels() %>%
      .[1]
    Yall <- dat1 %>%
      tidyr::spread(AVISIT,LBORRES) %>%
      dplyr::select(
        c(PARAMCD, LBORNRLO, LBORNRHI,
          SUBJIDN, TRTP, LBTESTCD, firstVisit
        )
      )
    lowquant <- highquant <- NULL
    Summa <- Yall %>%
      dplyr::group_by(PARAMCD) %>%
      dplyr::summarise(
        lowquant = stats::quantile(!!rlang::sym(firstVisit), na.rm = TRUE, probs = 0.25),
        highquant = stats::quantile(!!rlang::sym(firstVisit), na.rm = TRUE, probs = 0.75),
        max = max(!!rlang::sym(firstVisit), na.rm = TRUE),
        min = min(!!rlang::sym(firstVisit), na.rm = TRUE),
        highref = mean(as.numeric(LBORNRHI),na.rm = TRUE),
        lowref = mean(as.numeric(LBORNRLO), na.rm = TRUE)
      ) %>%
      dplyr::mutate(
        InQuRa = percent * (highquant - lowquant),
        Range = percent * (max - min),
        refRange = percent * (highref - lowref)
      ) %>%
      dplyr::select(PARAMCD, InQuRa, Range, refRange) %>%
      dplyr::rename(variable = PARAMCD)
    Summa
  })

  trtcompar_val <- shiny::reactive({
    shiny::req(ds2())
    choices  <- as.character(unique(ds2()$AVISIT))
    choices
  })

  #### eventReactive ####
  tcomp <- shiny::eventReactive(c(input$go_select2), {
    input$trtcompar
  })

  box_col <- shiny::eventReactive(input$go, {
    shiny::req(input$select.visit)
    visits <- input$select.visit
    selected <- input$trtcompar
    b.col <- c(
      input$'id1-col', input$'id2-col', input$'id3-col', input$'id4-col', input$'id5-col', input$'id6-col',
      input$'id7-col', input$'id8-col', input$'id9-col', input$'id10-col', input$'id11-col', input$'id12-col',
      input$'id13-col', input$'id14-col', input$'id15-col', input$'id16-col', input$'id17-col', input$'id18-col',
      input$'id19-col', input$'id20-col'
    )

    if (!is.null(b.col)) {
      b.col[b.col == "Color1"] <- colBoxplot1
      b.col[b.col == "Color2"] <- colBoxplot2
      b.col[b.col == "Color3"] <- colBoxplot3
      b.col[b.col == "Color4"] <- colBoxplot4
    }

    if ({input$stattest != "none"})
      b.col[!(visits %in% selected)] <- elaborator_transform_transparent(b.col[!(visits %in% selected)], 70)
    b.col
  })

  pre_clust <- shiny::eventReactive(c(input$go3, ds2()), {
    shiny::req(ds2())
    ds <- ds2()
    if (shiny::isolate(input$orderinglab) == "auto") {
      first <- shiny::isolate(input$select.ai.first)
      last <- shiny::isolate(input$select.ai.last)

      shiny::validate(
        shiny::need(first != last, "Please select different Timepoints for Seriation! The first timepoint must differ from second timepoint.")
      )

      shiny::validate(
        shiny::need(sum(ds$AVISIT == first) == sum(ds$AVISIT == last), "The selected timepoints have a different number of observations! Please use other timepoints or try to adjust the percentage of tolerated missing values.")
      )

      if (length(unique(ds$PARAMCD)) > 1 && length(unique(ds$AVISIT)) > 1) {
        ds_tmp <- ds %>%
          dplyr::group_by(PARAMCD,TRTP) %>%
          dplyr::summarise(median_value = median(LBORRES,na.rm = TRUE)) %>%
          dplyr::ungroup(PARAMCD,TRTP)

        ds2 <- ds %>%
          right_join(ds_tmp, by = c('PARAMCD','TRTP'))

        ds <- ds2 %>%
          dplyr::mutate(LBORRES = ifelse(is.na(LBORRES), median_value, LBORRES))

        ds_new <- ds %>%
          dplyr::group_by(SUBJIDN, PARAMCD) %>%
          dplyr::mutate(n = n())

        ds_new <- ds_new %>%
          dplyr::arrange(SUBJIDN, PARAMCD, AVISIT) %>%
          dplyr::filter(AVISIT %in% c(first,last))

        ds_new$LBORRES_diff <- c(0, diff(ds_new$LBORRES))
        ds_new_test <- ds_new %>%
          dplyr::mutate(LBORRES_diff = ifelse(AVISIT == first, 0 , LBORRES_diff))

        tmp <- ds_new_test %>%
          dplyr::select(PARAMCD, AVISIT, SUBJIDN, LBORRES_diff) %>%
          dplyr::filter(AVISIT == last) %>%
          tidyr::spread(key = PARAMCD, value = LBORRES_diff) %>%
          dplyr::mutate(vari = paste0(SUBJIDN, "_", AVISIT)) %>%
          dplyr::select(-dplyr::one_of(c("SUBJIDN","AVISIT"))) %>%
          dplyr::ungroup(SUBJIDN)

        tmp_nam <- tmp %>%
          dplyr::select(vari)
        tmp2 <- tmp %>%
          ungroup() %>%
          dplyr::select(-vari,-SUBJIDN) %>%
          t()
        colnames(tmp2) <- t(tmp_nam$vari)

        tmp2
      }
    } else {
      NULL
    }
  })

  clust <- shiny::eventReactive(c(input$go3, ds2()), {
    shiny::req(ds2())
    tmp2 <- pre_clust()
    ds <- ds2()
    if (input$orderinglab == "asinp") {
      as.character(unique(ds$PARAMCD))
    }
    else if (input$orderinglab == "alphabetically") {
      sort(as.character(unique(ds$PARAMCD)))}
    else if (input$orderinglab == "auto") {

      shiny::req(pre_clust())
      tmp2 %>%
        elaborator_calculate_spearman_distance() %>%
        seriation::seriate(method = input$clusterMethod) %>%
        seriation::get_order() %>%
        rownames(tmp2)[.]
    } else {
      as.character(unique(ds$PARAMCD))
    }
  })

  border.col <-shiny::eventReactive(c(input$go_select2), {
    choices <- input$select.visit
    selected <- input$trtcompar
    col <- rep(elaborator_transform_transparent("black", alpha = 70), length(choices))
    col[choices %in% selected] <- "black"
    col
  })

  #### OBSERVERS ####

  #### observe ####
  shiny::observe({
    input$'id1-col'
    input$'id2-col'
    input$'id3-col'
    input$'id4-col'
    input$'id5-col'
    input$'id6-col'
    input$'id7-col'
    input$'id8-col'
    input$'id9-col'
    input$'id10-col'
    input$'id11-col'
    input$'id12-col'
    input$'id13-col'
    input$'id14-col'
    input$'id15-col'
    input$'id16-col'
    input$'id17-col'
    input$'id18-col'
    input$'id19-col'
    input$'id20-col'
    purrr::map(
      paste0("id", 1:data_param()$nvisit),
      ~ shiny::callModule(
        boxPlotColor,
        id = .x, c("Color1", "Color2", "Color3", "Color4"),
        paste0(
          "Select Color for '",
          input$select.visit[as.numeric(substr(.x,3,nchar(.x)))],
          "'"
        ),
        start_color = ifelse(is.null(input[[paste0("id",as.numeric(substr(.x,3,nchar(.x))),"-col")]]),
                             paste0("Color", 2),
                             input[[paste0("id",as.numeric(substr(.x, 3, nchar(.x))),"-col")]]
        ),
        number = as.numeric(substr(.x,3,nchar(.x)))
      )
    )
  })


  #### observeEvent ####

  shiny::observeEvent(input$apply_ref_plot, {
    if (input$apply_ref_plot >= 1) {
      output$inoutPlot <- shiny::renderPlot({
        dat <- shiny::isolate(ds2())

        dat <- subset(dat,!(dat$LBORNRLO == "" & dat$LBORNRHI == ""))

        dat$PARAMCD <- dat$PARAMCD %>%
          factor()

        cex <- shiny::isolate(input$cex.rvbp)
        crit <- shiny::isolate(input$criterion)
        sorti <- shiny::isolate(clust())
        sorti <- sorti[sorti %in% levels(dat$PARAMCD)]

        elaborator_plot_ref_pattern(
          data = dat,
          fontsize = cex,
          criterion = crit,
          sorting_vector = sorti,
          abnormal_value_factor = shiny::isolate(input$abnormal_values_factor)
        )
      }, res = input$zoompx / 3)

      output$tab3 <- shiny::renderUI({
        shiny::req(data_param())

        hpx <- data_param()$ntreat

        wpx <- data_param()$nlab2
        zoompx <- input$zoompx
        panelheight <- input$panelheight

        shiny::wellPanel(style = paste0("background: ", ColorBG, ";overflow-x:scroll; max-height:", panelheight, "px"),
                         shiny::plotOutput(
                           outputId = 'inoutPlot',
                           height = paste0(hpx * zoompx, 'px'),
                           width = paste0(wpx * zoompx, 'px'),
                           hover = clickOpts("dist_hover3", clip = FALSE)
                         )
        )
      })
    }
  })

  shiny::observeEvent(input$apply_qual_plot, {
    shiny::req(isolate(ds2()))
    if(input$apply_qual_plot > 0) {
      output$trendPlot <- shiny::renderPlot({
        shiny::req(isolate(Summa()))
        dat <- shiny::isolate(ds2())
        cex <- shiny::isolate(input$cex.trend)
        Variab <- shiny::isolate(clust())
        Variab <- Variab[Variab %in% shiny::isolate(input$select.lab)]
        meth <- shiny::isolate(input$method)
        Summa  <- shiny::isolate(Summa())
        elaborator_plot_qual_trends(
          dat1 = dat,
          Variab,
          fontsize = cex,
          method = meth,
          color_palette = c('white', colChoice[[shiny::req(isolate(input$select.pal1))]]$col, 'black'),
          Summa = Summa
        )
      }, res = isolate(input$zoompx) / 3)

      output$tab2 <- shiny::renderUI({
        shiny::req(shiny::isolate(data_param()))
        hpx <- shiny::isolate(data_param()$ntreat)
        wpx <- shiny::isolate(data_param()$nlab)

        zoompx <- shiny::isolate(input$zoompx)
        panelheight <- shiny::isolate(input$panelheight)

        shiny::wellPanel(
          style = paste0("background: ", ColorBG, ";overflow-x:scroll; max-height:", panelheight, "px"),
          shiny::plotOutput(
            outputId = 'trendPlot',
            height = paste0(hpx * zoompx, 'px'),
            width = paste0(wpx * zoompx, 'px'),
            hover = clickOpts("dist_hover2", clip = FALSE)
          )
        )
      })
    }
  })


  #### Picker/Selectize Inputs ####
  shiny::observeEvent(df()$data, {

    if (is.factor(df()$data$LBTESTCD)) {
      choices_sel_lab <- levels(df()$data$LBTESTCD)
    } else {
      choices_sel_lab <- unique(df()$data$LBTESTCD)
    }
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select.lab",
      choices = choices_sel_lab,
      selected = choices_sel_lab
    )
    if (is.factor(df()$data$AVISIT)) {
      choices_sel_visit <- levels(df()$data$AVISIT)
    } else {
      choices_sel_visit <- unique(df()$data$AVISIT)
    }
    shiny::updateSelectizeInput(
      session,
      inputId = "select.visit",
      choices = choices_sel_visit,
      selected = choices_sel_visit
    )
    if (is.factor(df()$data$TRTP)) {
      choices_sel_treatments <- levels(df()$data$TRTP)
    } else {
      choices_sel_treatments <- unique(df()$data$TRTP)
    }
    shiny::updateSelectizeInput(
      session,
      inputId = "select.treatments",
      choices = choices_sel_treatments,
      selected = choices_sel_treatments
    )
  })

  shiny::observeEvent(input$select.visit, {
    choices  <- input$select.visit
    selected <- c(choices[1], choices[length(choices)])
    shiny::updateCheckboxGroupInput(
      session,
      inputId = "trtcompar",
      choices = choices,
      selected = selected
    )
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select.ai.first",
      choices = choices,
      selected = choices[1]
    )
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select.ai.last",
      choices = choices,
      selected = choices[length(choices)]
    )
  })

  shiny::observeEvent(input$apply_quant_plot, {
    shiny::req(ds2())
    if (input$apply_quant_plot > 0) {
      output$compl <- shiny::renderPlot({
        dat <- shiny::isolate(ds2())
        val <- shiny::isolate(values$default)
        if (!is.list(val)) {
          info <- NA
        } else {
          info <- shiny::isolate(values$default)
        }
        cho <- shiny::isolate(input$trtcompar)
        signtest2 <- shiny::isolate(input$stattest)
        if (signtest2 == "signtest") {
          signtest <- TRUE
        } else {
          signtest <- FALSE
        }
        sortpoint <- shiny::isolate(input$sortpoint)
        labelvis <- NULL
        sameax <- shiny::isolate(input$sameaxes)

        pval <- shiny::isolate(input$pcutoff)

        if (input$go != 0) {
          b.col <- shiny::isolate(box_col())
        } else {
          b.col <- c(colBoxplot2, colBoxplot2, colBoxplot2, colBoxplot2)
        }
        if (signtest2 != "none") {
          bordcol <- shiny::isolate(border.col())
        } else {
          bordcol <- NULL
        }
        sortin <- shiny::isolate(clust())

        sortin <- sortin[sortin %in% isolate(input$select.lab)]
        con_lin <- shiny::isolate(input$con_lin)

        add_points <- shiny::isolate(input$add_points)

        elaborator_plot_quant_trends(
          dat1 = dat,
          signtest = signtest,
          Visit1 = cho[1],
          Visit2 = cho[-1],
          labcolumn = "PARAMCD",
          cols = b.col,
          pcutoff = pval,
          sameaxes = sameax,
          sortpoints = sortpoint,
          labelvis = labelvis,
          cexoutliers = 0.5,
          infotest = info,
          sortinput = sortin,
          bordercol = bordcol,
          add_points = add_points,
          connect_lines = con_lin
        )
      }, res = isolate(input$zoompx) / 3
      )

      output$tab1 <- shiny::renderUI({
        shiny::req(isolate(data_param()))
        hpx <- shiny::isolate(data_param()$ntreat)
        wpx <- shiny::isolate(data_param()$nlab)
        zoompx <- shiny::isolate(input$zoompx)
        panelheight <- shiny::isolate(input$panelheight)
        shiny::wellPanel(style = paste0("background: ", ColorBG, ";overflow-x:scroll; max-height:", panelheight,"px"),
                         shiny::plotOutput(
                           outputId = 'compl',
                           height = paste0(hpx * zoompx,'px'),
                           width = paste0(wpx * zoompx, 'px'),
                           hover = clickOpts(
                             "dist_hover",
                             clip = FALSE
                           )
                         )
        )
      })
    }
  })

  #### Update Actionbuttons ####

  shiny::observeEvent(data_param(), {
    shiny::updateActionButton(
      session,
      inputId = "apply_qual_plot",
      label = paste0('Create/Update ', data_param()$nlab*data_param()$ntreat,' Plots')
    )
    shiny::updateActionButton(
      session,
      inputId = "apply_quant_plot",
      label = paste0('Create/Update ', data_param()$nlab*data_param()$ntreat,' Plots')
    )
    shiny::updateActionButton(
      session,
      inputId = "apply_ref_plot",
      label = paste0('Create/Update ', data_param()$nlab2*data_param()$ntreat,' Plots')
    )
  })

  shiny::observeEvent(input$link_to_pdf_view, {
    output$pdfview <- shiny::renderUI({
      tags$iframe(
        style = "height:500px; width:100%",
        src = "www/Seriation_methods_20191115.pdf"
      )
    })
  })

  shiny::observeEvent(df()$data, {
    start$dat <- TRUE
  })

  shiny::observeEvent(input$go3, {
    if (shiny::isolate(input$orderinglab) == "auto") {
      if (!is.null(shiny::isolate(input$clusterMethod))) {
        if ((startsWith(shiny::isolate(input$clusterMethod), "OLO") | startsWith(shiny::isolate(input$clusterMethod), "GW"))) {
          start.ai$dat <- TRUE
        } else {
          start.ai$dat <- FALSE
        }
      } else {
        start.ai$dat <- FALSE
      }
    } else {
      start.ai$dat <- FALSE
    }
  })

  shiny::observeEvent(c(input$go_select2), {
    shiny::req(
      ds2(),
      input$trtcompar,
      input$stattest,
      input$select.treatments,
      shiny::isolate(input$select.lab),
      input$select.visit
    )

    dat <- ds2()
    T1 <- input$trtcompar[1]
    T2 <- input$trtcompar[-1]
    signtest <- input$stattest

    if (input$stattest == "signtest" && length(input$trtcompar) >= 2 && length(unique(dat$AVISIT)) >= 2) {
      values$default <- elaborator_derive_test_values(
        data = dat,
        signtest = TRUE,
        Visit1 = T1,
        Visit2 = T2,
        lab_column = "PARAMCD"
      )
    } else if (input$stattest== "ttest" && length(input$trtcompar) >= 2 && length(unique(dat$AVISIT)) >= 2) {
      values$default <- elaborator_derive_test_values(
        data = dat,
        signtest = FALSE,
        Visit1 = T1,
        Visit2 = T2,
        lab_column = "PARAMCD"
      )
    } else {
      values$default <- NA
    }
  })

  shiny::observeEvent(input$link_to_tab_info, {
    shinydashboard::updateTabItems(session, "sidebarmenu", "helptext")
  })

  shiny::observeEvent(input$link_to_structure_info, {
    shinydashboard::updateTabItems(session, "sidebarmenu", "datamanual")
  })

  # change color of the Create/Upload Plots Buttons
  output$cont1 <- shiny::renderUI({
    list(
      shiny::tags$head(
        tags$style(HTML('#apply_quant_plot{color: #ffffff; background-color:#47d2bc;
          border-color: #f78300}'))
      )
    )
  })

  output$cont1_text <- shiny::renderUI({
    HTML(paste0("<b style='color: #47d2bc; border-color: #f78300'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"))
  })

  observeEvent(input$apply_quant_plot, {
    output$cont1 <- shiny::renderUI({
      list(
        shiny::tags$head(
          tags$style(HTML('#apply_quant_plot{color: #ffffff; background-color:#e3e3e3;
            border-color: #ffffff}'))
        )
      )
    })
    output$cont1_text <- shiny::renderUI({
      HTML("")
    })
  })

  shiny::observeEvent(
    c(input$sameaxes, input$add_points, input$sortpoint,
      input$con_lin, input$go_select2, input$select.visit,
      input$select.treatments, input$select.lab, input$select.toleratedPercentage,
      input$go3), {
        output$cont1 <- shiny::renderUI({
          list(
            shiny::tags$head(
              tags$style(HTML('#apply_quant_plot{color: #ffffff; background-color:#47d2bc;
            border-color: #f78300}'))
            )
          )
        })

        output$cont1_text <- shiny::renderUI({
          HTML(paste0("<b style='color: #47d2bc;border-color: #f78300'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"))
        })
      })

  output$cont2 <- shiny::renderUI({
    list(
      shiny::tags$head(
        tags$style(HTML('#apply_qual_plot{color: #ffffff; background-color:#47d2bc;
          border-color: #f78300}'))
      )
    )
  })

  output$cont2_text <- shiny::renderUI({
    HTML(paste0("<b style='color: #47d2bc; border-color: #f78300'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"))
  })

  observeEvent(input$apply_qual_plot, {
    output$cont2 <- shiny::renderUI({
      list(
        shiny::tags$head(
          tags$style(HTML('#apply_qual_plot{color: #ffffff; background-color:#e3e3e3;
            border-color: #ffffff}'))
        )
      )
    })
    output$cont2_text <- shiny::renderUI({
      HTML("")
    })
  })

  shiny::observeEvent(
    c(input$cex.trend, input$method, input$percent,
      input$select.visit, input$select.pal1,
      input$select.treatments, input$select.lab, input$select.toleratedPercentage,
      input$go3), {
        output$cont2 <- shiny::renderUI({
          list(
            shiny::tags$head(
              tags$style(HTML('#apply_qual_plot{color: #ffffff; background-color:#47d2bc;
            border-color: #f78300}'))
            )
          )
        })

        output$cont2_text <- shiny::renderUI({
          HTML(paste0("<b style='color: #47d2bc;'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"))
        })
      })

  output$cont3 <- shiny::renderUI({
    list(
      shiny::tags$head(
        tags$style(HTML('#apply_ref_plot{color: #ffffff; background-color:#47d2bc;
          border-color: #f78300}'))
      )
    )
  })

  output$cont3_text <- shiny::renderUI({
    HTML(paste0("<b style='color: #47d2bc;'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"))
  })

  observeEvent(input$apply_ref_plot, {
    output$cont3 <- shiny::renderUI({
      list(
        shiny::tags$head(
          tags$style(HTML('#apply_ref_plot{color: #ffffff; background-color:#e3e3e3;
            border-color: #ffffff}'))
        )
      )
    })
    output$cont3_text <- shiny::renderUI({
      HTML("")
    })
  })

  shiny::observeEvent(
    c(input$cex.rvbp, input$criterion, input$abnormal_values_factor,
      input$select.visit,
      input$select.treatments, input$select.lab, input$select.toleratedPercentage,
      input$go3), {
        output$cont3 <- shiny::renderUI({
          list(
            shiny::tags$head(
              tags$style(HTML('#apply_ref_plot{color: #ffffff; background-color:#47d2bc;
            border-color: #f78300}'))
            )
          )
        })

        output$cont3_text <- shiny::renderUI({
          HTML(paste0("<b style='color: #47d2bc;'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"))
        })
      })
}
