#' boxplots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_boxplots_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidPage(
      shiny::conditionalPanel(
        condition = "output.flag == true",
        ns = ns,
        shinydashboard::box(
          width = NULL,
          title = span(shiny::tagList('', icon("cogs"))),
          solidHeader = TRUE,
          background = 'black',
          collapsible = TRUE,
          collapsed = FALSE,
          shiny::fluidRow(
            bsplus::use_bs_popover(),
            bsplus::use_bs_tooltip(),
            shiny::column(
              3,
              shiny::checkboxInput(
                inputId = ns("sameaxes"),
                label = tagList(
                  "Use same scales within lab parameter",
                  bsplus::bs_embed_tooltip(
                    tag = bsplus::shiny_iconlink("question"),
                    title = "Define whether the scales are the same among all treatment groups.
                        Using the same scales among all
                        treatment groups enables a much better comparison between treatment groups.
                        Otherwise, each plot will have its own scale.",
                    placement = "top"
                  )
                ),
                value = FALSE
              ),
              shiny::checkboxInput(
                inputId = ns("outlier"),
                label = tagList(
                  "Use outlier corrected scale",
                  bsplus::bs_embed_tooltip(
                    tag = bsplus::shiny_iconlink("question"),
                    title = "Define whether the scales are outlier corrected or not. Outlier correction
                        uses the five times interquartile range as a definition of outliers.",
                    placement = "top"
                  )
                ),
                value = FALSE
              ),
              bsplus::use_bs_popover(),
              bsplus::use_bs_tooltip(),
              shiny::checkboxInput(
                inputId = ns("add_points"),
                label = tagList(
                  "Patient-specific values",
                  bsplus::bs_embed_tooltip(
                    tag = bsplus::shiny_iconlink("question"),
                    title = "Tick box for plotting patient-specific lab values as single points sorted from smallest to largest.
                        ",
                    placement = "top"
                  )
                ),
                value = FALSE
              ),
              shiny::conditionalPanel(
                condition = "input.add_points == true",
                ns = ns,
                shiny::checkboxInput(
                  inputId = ns("sortpoint"),
                  label = "Sort patient-specific values",
                  value = FALSE
                )
              ),
              shiny::checkboxInput(
                inputId = ns("con_lin"),
                label = tagList(
                  "Draw connection lines",
                  bsplus::bs_embed_tooltip(
                    tag = bsplus::shiny_iconlink("question"),
                    title = "Tick box for plotting connection lines between patient measurements.
                          If the option 'First/last visit' is selected, the colors indicating increasing or decreasing lab
                          values from first to last visit.
                          If 'Each visit' is selected, the colors indicating increase/decrease between each visit for a single subject.
                          The 'Custom visit' option can be used to select two visits for the increase/decrease indication.
                          If more or less then 2 visits are selected, all lines appear grey.
                          This is also the case for the last option 'All grey'.
                          ",
                    placement = "right"
                  )
                ),
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.con_lin == true",
                ns = ns,
                prettyRadioButtons(
                  inputId = ns("con_lin_options"),
                  label = "",
                  choices = c(
                    "First/last visit" = "first_last",
                    "Each visit" = "each_visit",
                    "Custom visits" = "custom_visits",
                    "All grey" = "all_grey"
                  ),
                  selected = "first_last",
                  status = "warning",
                  inline = TRUE
                )
              ),
                conditionalPanel(
                  condition = "input.con_lin_options == 'custom_visits'",
                  ns = ns,
                  shiny::checkboxGroupInput(
                  inputId = ns("custom_visits"),
                  label = "",
                  choices = NULL,
                  selected = NULL,
                  inline = TRUE
                ),
                conditionalPanel(
                  condition = "input.custom_visits.length != 2",
                  ns = ns,
                  class = "color-red",
                  "Please select exactly two visits"
                )
              )
            ),
            shinydashboard::box(
              background = 'black',
              shiny::column(
                4,
                bsplus::use_bs_popover(),
                bsplus::use_bs_tooltip(),
                bsplus::bs_embed_tooltip(
                  tag = h5(span(shiny::tagList(
                    "Test for explorative trend detection",
                    icon("question")
                  ))),
                  title = "Explore whether there are any trends over time (comparison of test results between treatment groups is only recommended for balanced designs). Choose the approproate statistical test. The statistical test aims to assess whether patient-specific changes in laboratory values occur.",
                  placement = "bottom",
                  expanded = TRUE
                ),
                shinyWidgets::prettyRadioButtons(
                  inputId = ns("stattest"),
                  label = "",
                  choices = c(
                    "None" = "none",
                    "Sign test" = "signtest",
                    "t-test" = "ttest"
                  ),
                  selected = "none",
                  status = "warning"
                ),
                conditionalPanel(
                  condition = "input.trtcompar.length > 1 | input.stattest == 'none'",
                  ns = ns,
                  shiny::actionButton(
                    inputId = ns("go_select2"),
                    label = "Update!",
                    icon = icon("redo")
                  )
                )
              ),
              shiny::conditionalPanel(
                condition = "input.stattest != 'none'",
                ns = ns,
                shiny::column(
                  4,
                  bsplus::use_bs_popover(),
                  bsplus::use_bs_tooltip(),
                  bsplus::bs_embed_tooltip(
                    tag = h5(span(shiny::tagList(
                      "Visits to compare",
                      icon("question")
                    ))),
                    title = "Select which visits you want to test for the existence of a trend. If more than two visits are selected, the first selection is tested against any of the others (pairwise testing).",
                    placement = "top",
                    expanded = TRUE
                  ),
                  shiny::checkboxGroupInput(
                    inputId = ns("trtcompar"),
                    label = "",
                    choices = NULL,
                    selected = NULL
                  ),
                  shiny::conditionalPanel(
                    condition = "output.check <2",
                    ns = ns,
                    class = "color-red",
                    shiny::helpText(
                      "Please select at least 2 visits!"
                    )
                  )
                ),
                shiny::column(
                  3,
                  bsplus::use_bs_popover(),
                  bsplus::use_bs_tooltip(),
                  bsplus::bs_embed_tooltip(
                    tag = h5(span(shiny::tagList(
                      "p-value cutoff",
                      icon("question")
                    ))),
                    title = "Statistical tests are performed for each lab parameter and treatment group. Backgrounds are colored if the respective p-value lies below this p-value threshold.",
                    placement = "top",
                    expanded = TRUE
                  ),
                  shiny::sliderInput(
                    inputId = ns("pcutoff"),
                    label = tags$div(tags$h5(" ")),
                    min = 0,
                    max = 0.2,
                    value = 0.01,
                    step = 0.005
                  )
                )
              )
            ),
            shiny::column(
              2,
              shiny::helpText(
                class = "color-white",
                "You can minimize/maximize this window with the -/+ button on the top right of the panel"
              ),
              shiny::conditionalPanel(
                condition = "input.stattest != 'none'",
                ns = ns,
                bsplus::use_bs_popover(),
                bsplus::use_bs_tooltip(),
                bsplus::bs_embed_tooltip(
                  tag = h5(span(shiny::tagList(
                    tags$i(
                      class = "fa-solid fa-square decrease"
                    ),
                    "Decrease"
                  ))),
                  title = "Statistical test indicates a decrease in values.",
                  placement = "top",
                  expanded = TRUE
                ),
                bsplus::bs_embed_tooltip(
                  tag = h5(span(shiny::tagList(
                    tags$i(
                      class = "fa-solid fa-square increase"
                    ),
                    "Increase"
                  ))),
                  title = "Statistical test indicates an increase in values.",
                  placement = "top",
                  expanded = TRUE
                ),
                bsplus::bs_embed_tooltip(
                  tag = h5(span(shiny::tagList(
                    tags$i(
                      class = "fa-solid fa-square missing"
                    ),
                    "Missing"
                  ))),
                  title = "Statistical test indicates missing values.",
                  placement = "top",
                  expanded = TRUE
                )
              )
            )
          )
        )
      ),
      shiny::conditionalPanel(
        condition = "output.ai == true",
        ns = ns,
        shinydashboard::box(
          width = NULL,
          title = span(shiny::tagList(
            '',
            icon("sort-alpha-down"),
            'Dendrogram - (Click on the + symbol to open)'
          )),
          solidHeader = TRUE,
          background = 'black',
          collapsible = TRUE,
          collapsed = TRUE,
          height = "100%",
          shiny::fluidRow(
            shiny::column(
              12,
              shiny::plotOutput(
                outputId = ns("dendro_1"),
                height = "450px"
              )
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::conditionalPanel(
            condition = "output.flag == false",
            ns = ns,
            shiny::HTML(
              "<img src = 'www/BAY_eLaborator_Logo.svg'
                  alt = 'Graphic cannot be displayed'
                  width = '682'
                  height = '286'>"
            ),
            h2(
              "is a novel concept for generating knowledge and gaining insights into laboratory data. You will be able to efficiently and easily explore your laboratory data
                  from different perspectives."
            ),
            br(),
            tags$div(
              HTML(
                paste(
                  "<i class='fa fa-file-upload'></i>&emsp;",
                  tags$span(
                    class = "larger-font",
                    "Upload your",
                    tags$span(
                      class = "color-orange",
                      "laboratory data"
                    ),
                    " by using the 'Data Upload'-tab in the task bar on the left.
                        Select the file format and click
                        the 'Browse...'-button.",
                    sep = ""
                  )
                )
              )
            ),
            tags$div(
              HTML(
                paste(
                  "<i class= 'fa fa-file'></i>&emsp;",
                  tags$span(
                    class = "larger-font",
                    "Click the 'Data Manual'-tab for the required format and structure for laboratory data file."
                  )
                )
              )
            ),
            tags$div(
              HTML(
                paste(
                  "<i class='fa fa-info'></i>&emsp;",
                  tags$span(
                    class = "larger-font",
                    " If you want to access information on the elaborator, click the 'Information'-tab.",
                    sep = ""
                  )
                )
              )
            )
          ),
          shiny::conditionalPanel(
            condition = "output.flag == true",
            ns = ns,
            shiny::fluidRow(
              shiny::column(
                2,
                shiny::actionButton(
                  inputId = ns("apply_quant_plot"),
                  label = paste0('Create Plots'),
                  icon = icon("object-group")
                ),
                shiny::uiOutput(ns("cont1"))
              ),
              shiny::column(
                5,
                offset = 2,
                shiny::uiOutput(ns("cont1_text"))
              )
            )
          ),
          shiny::uiOutput(ns("tab1"), width = 'auto'),
          shiny::conditionalPanel(
            condition = "output.flag == true",
            ns = ns,
            shiny::uiOutput(ns("hoverpanel"))
          )
        )
      )
    )
  )
}

mod_boxplots_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$con_lin_options, {
      if (!identical(input$con_lin_options, "custom_visits")) {
        return(invisible(NULL))
      }
      choices <- r$globals$select.visit
      shiny::req(choices)
      selected <- c(choices[1], choices[length(choices)])
      shiny::updateCheckboxGroupInput(
        session,
        inputId = ns("custom_visits"),
        choices = choices,
        selected = selected
      )
    })

  ####    3. dendrogram output ####
  output$dendro_1 <- shiny::renderPlot({
    shiny::req(
      r$prepare_dist_matrix_for_clustering,
      shiny::isolate(r$globals$clusterMethod)
    )
    if (
      (startsWith(shiny::isolate(r$globals$clusterMethod), "OLO") |
        startsWith(shiny::isolate(r$globals$clusterMethod), "GW"))
    ) {
      tmp <- r$prepare_dist_matrix_for_clustering
      ser <- seriation::seriate(
        elaborator_calculate_spearman_distance(tmp),
        method = shiny::isolate(r$globals$clusterMethod)
      )
      asdendro <- stats::as.dendrogram(ser[[1]])
      dendro <- dendextend::assign_values_to_leaves_edgePar(dend = asdendro)
      graphics::rect(
        xleft = graphics::grconvertX(0, 'ndc', 'user'),
        xright = graphics::grconvertX(1, 'ndc', 'user'),
        ybottom = graphics::grconvertY(0, 'ndc', 'user'),
        ytop = graphics::grconvertY(1, 'ndc', 'user'),
        border = NA,
        col = r$theme$ColorBG,
        xpd = TRUE
      )
      on_ex <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(on_ex))
      graphics::par(bg = r$theme$ColorBG)
      graphics::plot(dendro, ylab = "Distance", horiz = FALSE)
    }
  })

  #### QUALITATIVE TRENDS ####
  output$hover <- shiny::renderPlot(
    {
      input$apply_quant_plot
      shiny::req(
        shiny::isolate(r$data_with_selected_factor_levels),
        input$plot_option_switch
      )

      # switch between hover or click options for zoom panel
      if (input$plot_option_switch == "hover") {
        plot_coords <- input$dist_hover
      } else if (input$plot_option_switch == "click") {
        plot_coords <- input$dist_click
      }

      if (
        !is.null(plot_coords$coords_css$y) & !is.null(plot_coords$coords_css$x)
      ) {
        if (plot_coords$coords_css$y > 0 & plot_coords$coords_css$x > 0) {
          y <- plot_coords$coords_css$y
          x <- plot_coords$coords_css$x
          if (!is.null(y) && !is.null(x)) {
            #use only subjects with non missing values for all visits
            dat <- r$data_with_only_non_missings_over_visits

            #load statistical test values (saved in r$values$default)

            val <- shiny::isolate(r$values$default)
            if (!is.list(val)) {
              info <- NA
            } else {
              info <- shiny::isolate(r$values$default)
            }
            #replace r$values$default with newer version
            #load statistical test values (saved in statistical_test_resulst$var)
            if (r$globals$go != 0) {
              b.col <- shiny::isolate(box_col())
            } else {
              b.col <- c(r$theme$colBoxplot2, r$theme$colBoxplot2, r$theme$colBoxplot2, r$theme$colBoxplot2)
            }
            if (shiny::isolate(input$stattest) != "none") {
              bordcol <- shiny::isolate(border.col())
            } else {
              bordcol <- NULL
            }

            sortin <- levels(dat$LBTESTCD)[
              levels(dat$LBTESTCD) %in% unique(dat$LBTESTCD)
            ]
            dat_filt <- dat[
              dat$TRTP == levels(dat$TRTP)[ceiling(y / r$globals$zoompx)] &
                dat$LBTESTCD == sortin[ceiling(x / r$globals$zoompx)],
            ]
            dat_filt$TRTP <- factor(dat_filt$TRTP)
            dat_filt$LBTESTCD <- factor(dat_filt$LBTESTCD)

            if (input$con_lin) {
              lines_data <- r$quant_plot_data_lines %>%
                dplyr::filter(
                  TRTP ==
                    dat %>%
                      dplyr::pull(TRTP) %>%
                      levels() %>%
                      .[ceiling(y / isolate(r$globals$zoompx))],
                  LBTESTCD == sortin[ceiling(x / isolate(r$globals$zoompx))]
                )
            } else {
              lines_data <- NULL
            }
            if (!is.null(r$statistical_test_results$var)) {
              infotest <- r$statistical_test_results$var %>%
                dplyr::filter(
                  TRTP ==
                    dat %>%
                      dplyr::pull(TRTP) %>%
                      levels() %>%
                      .[ceiling(y / isolate(r$globals$zoompx))],
                  LBTESTCD == sortin[ceiling(x / isolate(r$globals$zoompx))]
                )
            } else {
              infotest <- NULL
            }

            elaborator_plot_quant_trends(
              dat_filt,
              #shiny::isolate(r$data_with_only_non_missings_over_visits),
              signtest = ifelse(
                shiny::isolate(input$stattest) == "signtest",
                TRUE,
                FALSE
              ),
              Visit1 = shiny::isolate(input$trtcompar)[1],
              Visit2 = shiny::isolate(input$trtcompar)[-1],
              labcolumn = "LBTESTCD",
              cols = b.col,
              pcutoff = shiny::isolate(input$pcutoff),
              sameaxes = shiny::isolate(input$sameaxes),
              sortpoints = shiny::isolate(input$sortpoint),
              labelvis = NULL,
              infotest = infotest,
              sortinput = levels(dat_filt$LBTESTCD),
              bordercol = bordcol,
              add_points = shiny::isolate(input$add_points),
              connect_lines = shiny::isolate(input$con_lin),
              lin_data = lines_data,
              outliers = shiny::isolate(input$outlier),
              tolerated_percentage = shiny::isolate(
                r$globals$select.toleratedPercentage
              ),
              color_lines_options = shiny::isolate(input$con_lin_options),
              custom_visits = shiny::isolate(input$custom_visits)
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
            xleft = grconvertX(0, 'ndc', 'user'),
            xright = grconvertX(1, 'ndc', 'user'),
            ybottom = grconvertY(0, 'ndc', 'user'),
            ytop = grconvertY(1, 'ndc', 'user'),
            border = NA,
            col = r$theme$ColorBG,
            xpd = TRUE
          )
          text(
            0.5,
            0.6,
            ifelse(
              input$plot_option_switch == "hover",
              "Please move your mouse over the plots",
              "Please click on the plots"
            ),
            col = r$theme$ColorFont
          )
          text(
            0.5,
            0.4,
            "to get an enlarged version of the plot!",
            col = r$theme$ColorFont
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
          xleft = grconvertX(0, 'ndc', 'user'),
          xright = grconvertX(1, 'ndc', 'user'),
          ybottom = grconvertY(0, 'ndc', 'user'),
          ytop = grconvertY(1, 'ndc', 'user'),
          border = NA,
          col = r$theme$ColorBG,
          xpd = TRUE
        )
        text(
          0.5,
          0.6,
          ifelse(
            input$plot_option_switch == "hover",
            "Please move your mouse over the plots",
            "Please click on the plots"
          ),
          col = r$theme$ColorFont
        )
        text(
          0.5,
          0.4,
          "to get an enlarged version of the plot!",
          col = r$theme$ColorFont
        )
      }
    },
    width = 400
  )

  output$hoverpanel <- shiny::renderUI({
    shiny::absolutePanel(
      id = ns("hoverpanel"),
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0("<div style='background-color:", r$theme$ColorBG, "'>")),
      shiny::tags$button(
        class = "btn",
        style = "background: #f6ad82; color:#ffffff",
        `data-toggle` = "collapse",
        `data-target` = paste0("#", ns("demo_quant")),
        shiny::HTML(
          "<i class=\"fa-solid fa-search-plus\"></i> Open/Close Zoom Panel"
        )
      ),
      top = 70,
      left = "auto",
      right = 100,
      bottom = "auto",
      width = 400,
      height = "auto",
      tags$div(
        id = ns("demo_quant"),
        class = "collapse",
        shiny::fluidRow(
          shiny::column(
            2,
            shiny::plotOutput(
              outputId = ns("hover"),
              height = "400px"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            12,
            offset = 4,
            shiny::radioButtons(
              inputId = ns("plot_option_switch"),
              label = NULL,
              choices = c("hover", "click"),
              selected = c("hover"),
              inline = TRUE
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(12, shiny::uiOutput(ns("hover_info_text")))
        )
      ),
      style = "z-index: 99999;"
    )
  })

  output$hover_info_text <- shiny::renderUI({
    input$apply_quant_plot
    shiny::req(
      shiny::isolate(r$data_with_missing_flag),
      input$plot_option_switch
    )

    # switch between hover or click options for zoom panel
    if (input$plot_option_switch == "hover") {
      plot_coords <- input$dist_hover
    } else if (input$plot_option_switch == "click") {
      plot_coords <- input$dist_click
    }

    if (
      !is.null(plot_coords$coords_css$y) & !is.null(plot_coords$coords_css$x)
    ) {
      if (plot_coords$coords_css$y > 0 & plot_coords$coords_css$x > 0) {
        y <- plot_coords$coords_css$y
        x <- plot_coords$coords_css$x
        if (!is.null(y) && !is.null(x)) {
          dat <- shiny::isolate(r$data_with_only_non_missings_over_visits)
          #dat <- shiny::isolate(r$data_with_selected_factor_levels)

          sortin <- levels(dat$LBTESTCD)[
            levels(dat$LBTESTCD) %in% unique(dat$LBTESTCD)
          ]
          #sortin <- levels(dat$LBTESTCD)
          val <- shiny::isolate(r$values$default)
          hover_treatment <- dat %>%
            dplyr::pull(TRTP) %>%
            levels() %>%
            .[ceiling(y / isolate(r$globals$zoompx))]

          hover_labparameter <- sortin[ceiling(x / isolate(r$globals$zoompx))]

          text <- elaborator_create_hover_info_text(
            elab_data = r$data_with_missing_flag,
            labparameter = hover_labparameter,
            treat = hover_treatment,
            select.visit = r$globals$select.visit
          )

          HTML(
            text
          )
        }
      }
    } else {
      HTML("")
    }
  })
  box_col <- shiny::eventReactive(r$globals$go, {
    shiny::req(r$globals$select.visit)
    visits <- r$globals$select.visit
    selected <- input$trtcompar
    b.col <- c(
      r$globals[["id1-col"]],
      r$globals[["id2-col"]],
      r$globals[["id3-col"]],
      r$globals[["id4-col"]],
      r$globals[["id5-col"]],
      r$globals[["id6-col"]],
      r$globals[["id7-col"]],
      r$globals[["id8-col"]],
      r$globals[["id9-col"]],
      r$globals[["id10-col"]],
      r$globals[["id11-col"]],
      r$globals[["id12-col"]],
      r$globals[["id13-col"]],
      r$globals[["id14-col"]],
      r$globals[["id15-col"]],
      r$globals[["id16-col"]],
      r$globals[["id17-col"]],
      r$globals[["id18-col"]],
      r$globals[["id19-col"]],
      r$globals[["id20-col"]]
    )

    if (!is.null(b.col)) {
      b.col[b.col == "Color1"] <- r$theme$colBoxplot1
      b.col[b.col == "Color2"] <- r$theme$colBoxplot2
      b.col[b.col == "Color3"] <- r$theme$colBoxplot3
      b.col[b.col == "Color4"] <- r$theme$colBoxplot4
    }

    if (
      {
        input$stattest != "none"
      }
    ) {
      b.col[!(visits %in% selected)] <- elaborator_transform_transparent(
        b.col[!(visits %in% selected)],
        70
      )
    }
    b.col
  })

  border.col <- shiny::eventReactive(c(input$go_select2), {
    choices <- r$globals$select.visit
    selected <- input$trtcompar
    col <- rep(
      elaborator_transform_transparent("black", alpha = 70),
      length(choices)
    )
    col[choices %in% selected] <- "black"
    col
  })
  shiny::observeEvent(input$apply_quant_plot, {
    # requirements
    shiny::req(r$data_with_selected_factor_levels)
    # button need to be clicked at least once
    if (shiny::isolate(input$apply_quant_plot) > 0) {
      #select data  with non-missings for all visits
      dat <- shiny::isolate(r$data_with_only_non_missings_over_visits)

      #load statistical test values (saved in r$values$default)
      val <- shiny::isolate(r$values$default)
      if (!is.list(val)) {
        info <- NA
      } else {
        info <- shiny::isolate(r$values$default)
      }
      #replace r$values$default with newer version
      #load statistical test values (saved in statistical_test_resulst$var)

      if (shiny::isolate(r$globals$go) != 0) {
        b.col <- shiny::isolate(box_col())
      } else {
        b.col <- c(r$theme$colBoxplot2, r$theme$colBoxplot2, r$theme$colBoxplot2, r$theme$colBoxplot2)
      }
      if (shiny::isolate(input$stattest) != "none") {
        bordcol <- shiny::isolate(border.col())
      } else {
        bordcol <- NULL
      }
      if (input$con_lin) {
        lines_data <- r$quant_plot_data_lines
      } else {
        lines_data <- NULL
      }
      #renderPlot created by elablorator_plot_quant_trends()-function
      output$compl <- shiny::renderPlot(
        {
          elaborator_plot_quant_trends(
            shiny::isolate(r$data_with_only_non_missings_over_visits),
            signtest = ifelse(
              shiny::isolate(input$stattest) == "signtest",
              TRUE,
              FALSE
            ),
            Visit1 = shiny::isolate(input$trtcompar)[1],
            Visit2 = shiny::isolate(input$trtcompar)[-1],
            labcolumn = "LBTESTCD",
            cols = b.col,
            pcutoff = shiny::isolate(input$pcutoff),
            sameaxes = shiny::isolate(input$sameaxes),
            sortpoints = shiny::isolate(input$sortpoint),
            labelvis = NULL,
            infotest = shiny::isolate(r$statistical_test_results$var),
            sortinput = levels(shiny::isolate(dat$LBTESTCD)),
            bordercol = bordcol,
            add_points = shiny::isolate(input$add_points),
            connect_lines = shiny::isolate(input$con_lin),
            lin_data = lines_data,
            outliers = shiny::isolate(input$outlier),
            tolerated_percentage = shiny::isolate(
              r$globals$select.toleratedPercentage
            ),
            color_lines_options = shiny::isolate(input$con_lin_options),
            custom_visits = shiny::isolate(input$custom_visits)
          )
        },
        res = shiny::isolate(r$globals$zoompx) / 3
      )

      #Create a plot as y-label for graph
      output$treatment_label_panel <- shiny::renderPlot({
        elaborator_plot_quant_trends_treatment_label(
          dat1 = dat
        )
      })

      output$tab1 <- shiny::renderUI({
        shiny::req(r$data_param)
        hpx <- r$data_param$ntreat
        wpx <- r$data_param$nlab
        zoompx <- shiny::isolate(r$globals$zoompx)
        panelheight <- shiny::isolate(r$globals$panelheight)
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::wellPanel(
              style = paste0(
                "background: ",
                r$theme$ColorBG,
                ";overflow-x:scroll; max-height:",
                panelheight,
                "px"
              ),
              shiny::plotOutput(
                outputId = ns("compl"),
                height = paste0(hpx * zoompx, 'px'),
                width = paste0(wpx * zoompx, 'px'),
                hover = shiny::clickOpts(
                  session$ns("dist_hover"),
                  clip = FALSE
                ),
                click = shiny::clickOpts(session$ns("dist_click"), clip = FALSE)
              )
            )
          )
        )
      })
    }
  })
  shiny::observeEvent(c(input$go_select2), {
    shiny::req(
      r$data_with_selected_factor_levels,
      input$trtcompar,
      input$stattest,
      r$globals$select.treatments,
      shiny::isolate(r$globals$select.lab),
      r$globals$select.visit
    )

    dat <- r$data_with_selected_factor_levels
    T1 <- input$trtcompar[1]
    T2 <- input$trtcompar[-1]
    signtest <- input$stattest

    if (input$stattest == "signtest" | input$stattest == "ttest") {
      r$statistical_test_results$var <- elaborator_calculate_test_for_all_visits(
        elab_data = dat,
        Visit1 = T1,
        Visit2 = T2,
        sign_test = input$stattest,
        pcutoff = shiny::isolate(input$pcutoff)
      )
    } else {
      r$statistical_test_results$var <- NULL
    }

    # if (input$stattest == "signtest" && length(input$trtcompar) >= 2 && length(unique(dat$AVISIT)) >= 2) {
    #   r$values$default <- elaborator_derive_test_values(
    #     data = dat,
    #     signtest = TRUE,
    #     Visit1 = T1,
    #     Visit2 = T2,
    #     lab_column = "LBTESTCD"
    #   )
    #
    # } else if (input$stattest== "ttest" && length(input$trtcompar) >= 2 && length(unique(dat$AVISIT)) >= 2) {
    #   r$values$default <- elaborator_derive_test_values(
    #     data = dat,
    #     signtest = FALSE,
    #     Visit1 = T1,
    #     Visit2 = T2,
    #     lab_column = "LBTESTCD"
    #   )
    # } else {
    #   r$values$default <- NA
    # }
  })
  # change color of the Create/Upload Plots Buttons
  output$cont1 <- shiny::renderUI({
    aq <- ns("apply_quant_plot")
    list(
      shiny::tags$head(
        tags$style(HTML(
          paste0(
            '#', aq, '{color: #ffffff; background-color:#47d2bc;',
            'border-color: #f78300}'
          )
        ))
      )
    )
  })

  output$cont1_text <- shiny::renderUI({
    HTML(paste0(
      "<b style='color: #47d2bc; border-color: #f78300'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"
    ))
  })

  shiny::observeEvent(input$apply_quant_plot, {
    aq <- ns("apply_quant_plot")
    output$cont1 <- shiny::renderUI({
      list(
        shiny::tags$head(
          tags$style(HTML(
            paste0(
              '#', aq, '{color: #ffffff; background-color:#e3e3e3;',
              'border-color: #ffffff}'
            )
          ))
        )
      )
    })
    output$cont1_text <- shiny::renderUI({
      HTML("")
    })
  })

  shiny::observeEvent(
    c(
      input$sameaxes,
      input$add_points,
      input$con_lin,
      input$go_select2,
      r$globals$select.visit,
      r$globals$select.treatments,
      r$globals$select.lab,
      r$globals$select.toleratedPercentage,
      r$globals$go3,
      input$sortpoint,
      r$globals$zoompx,
      input$con_lin_options,
      input$custom_visits,
      input$outlier
    ),
    {
      aq <- ns("apply_quant_plot")
      output$cont1 <- shiny::renderUI({
        list(
          shiny::tags$head(
            tags$style(HTML(
              paste0(
                '#', aq, '{color: #ffffff; background-color:#47d2bc;',
                'border-color: #f78300}'
              )
            ))
          )
        )
      })

      output$cont1_text <- shiny::renderUI({
        HTML(paste0(
          "<b style='color: #47d2bc;border-color: #f78300'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"
        ))
      })
    }
  )
  })
}
