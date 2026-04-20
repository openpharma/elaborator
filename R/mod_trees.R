#' trees UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_trees_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::box(
      width = NULL,
      title = span(shiny::tagList('', icon("cogs"))),
      background = 'black',
      solidHeader = TRUE,
      collapsible = TRUE,
      shiny::column(
        2,
        bsplus::use_bs_popover(),
        bsplus::use_bs_tooltip(),
        bsplus::bs_embed_tooltip(
          tag = h5(span(shiny::tagList(
            "Font size",
            icon("question")
          ))),
          title = "Adapt font size. Set font size to 0 to suppress any text.",
          placement = "top",
          expanded = TRUE
        ),
        shiny::sliderInput(
          inputId = ns("cex.rvbp"),
          label = '',
          min = 0,
          max = 5,
          value = 0,
          step = 0.5
        )
      ),
      shiny::column(
        2,
        bsplus::use_bs_popover(),
        bsplus::use_bs_tooltip(),
        bsplus::bs_embed_tooltip(
          tag = h5(span(shiny::tagList(
            "Definition of abnormal values",
            icon("question")
          ))),
          title = "Select how to define abnormal values based on the upper limit of normal (ULN) and lower limit of normal (LLN).",
          placement = "top",
          expanded = TRUE
        ),
        shinyWidgets::prettyRadioButtons(
          inputId = ns("criterion"),
          label = tags$div(tags$h5("")),
          choices = c(
            "above ULN OR below LLN" = "within",
            "above ULN" = "greater",
            "below LLN" = "less"
          ),
          selected = "within",
          status = "warning"
        )
      ),
      shiny::column(
        2,
        bsplus::use_bs_popover(),
        bsplus::use_bs_tooltip(),
        bsplus::bs_embed_tooltip(
          tag = h5(span(shiny::tagList(
            "Factor multiplied with ULN or LLN",
            icon("question")
          ))),
          title = "Define abnormal values in terms of ULN or LLN multiplied with a positive value. E.g. the factor 2
                  means that abnormal values are defined as values above 2xULN and/or below 2xLLN.",
          placement = "top",
          expanded = TRUE
        ),
        shiny::numericInput(
          inputId = ns("abnormal_values_factor"),
          label = "",
          value = 1,
          min = 0,
          step = 0.1
        )
      ),
      shiny::column(
        width = 2,
        offset = 4,
        shiny::helpText(
          class = "color-white",
          "You can minimize/maximize this window with the -/+ button on the top right of the panel"
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
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::plotOutput(
              outputId = ns("dendro_3"),
              height = "250px"
            )
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::conditionalPanel(
          condition = "input.abnormal_values_factor >= 0 && input.abnormal_values_factor != undefined && output.flag == true",
          ns = ns,
          shiny::fluidRow(
            shiny::column(
              2,
              shiny::actionButton(
                inputId = ns("apply_ref_plot"),
                label = paste0('Create Plots'),
                icon = icon("object-group")
              ),
              shiny::uiOutput(ns("cont3"))
            ),
            shiny::column(
              5,
              offset = 2,
              shiny::uiOutput(ns("cont3_text"))
            )
          )
        ),
        shiny::conditionalPanel(
          condition = "input.abnormal_values_factor < 0 || input.abnormal_values_factor == undefined",
          ns = ns,
          class = "color-orange",
          "Please enter a non-negative numeric percentage value."
        ),
        shiny::uiOutput(ns("tab3"), width = 'auto'),
        shiny::uiOutput(ns("hoverpanel3"))
      )
    )
  )
}

mod_trees_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
  output$hover_info_text3 <- shiny::renderUI({
    input$apply_ref_plot
    #shiny::req(shiny::isolate(r$data_with_missing_flag), input$plot_option_switch3)
    shiny::req(
      shiny::isolate(r$data_with_missing_flag),
      input$plot_option_switch3
    )

    # switch between hover or click options for zoom panel
    if (input$plot_option_switch3 == "hover") {
      plot_coords <- input$dist_hover3
    } else if (input$plot_option_switch3 == "click") {
      plot_coords <- input$dist_click3
    }

    if (
      !is.null(plot_coords$coords_css$y) & !is.null(plot_coords$coords_css$x)
    ) {
      if (plot_coords$coords_css$y > 0 & plot_coords$coords_css$x > 0) {
        y <- plot_coords$coords_css$y
        x <- plot_coords$coords_css$x
        if (!is.null(y) && !is.null(x)) {
          dat <- shiny::isolate(r$data_with_only_non_missings_over_visits)
          sortin <- levels(dat$LBTESTCD)[
            levels(dat$LBTESTCD) %in% unique(dat$LBTESTCD)
          ]
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

  #### REFERENCE VALUE BASED PATTERN ####
  output$dendro_3 <- shiny::renderPlot({
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
      dendro3 <- dendextend::assign_values_to_leaves_edgePar(dend = asdendro)

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
      graphics::plot(dendro3, ylab = "Distance", horiz = FALSE)
    }
  })

  output$hoverpanel3 <- shiny::renderUI({
    shiny::absolutePanel(
      id = ns("hoverpanel3"),
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      HTML(paste0("<div style='background-color:", r$theme$ColorBG, "'>")),
      shiny::tags$button(
        class = "btn",
        style = "background: #f6ad82; color:#ffffff",
        `data-toggle` = "collapse",
        `data-target` = paste0("#", ns("demo_rvbp")),
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
      shiny::tags$div(
        id = ns("demo_rvbp"),
        class = "collapse",
        shiny::fluidRow(
          shiny::column(
            2,
            shiny::plotOutput(outputId = ns("hover3"), height = "400px")
          )
        ),
        shiny::fluidRow(
          shiny::column(
            12,
            offset = 4,
            shiny::radioButtons(
              inputId = ns("plot_option_switch3"),
              label = NULL,
              choices = c("hover", "click"),
              selected = c("hover"),
              inline = TRUE
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(12, shiny::uiOutput(ns("hover_info_text3")))
        )
      ),
      style = "z-index: 10;"
    )
  })

  output$hover3 <- shiny::renderPlot(
    {
      input$apply_ref_plot
      shiny::req(
        r$data_with_selected_factor_levels,
        input$abnormal_values_factor,
        input$plot_option_switch3
      )

      # switch between hover or click options for zoom panel
      if (input$plot_option_switch3 == "hover") {
        plot_coords <- input$dist_hover3
      } else if (input$plot_option_switch3 == "click") {
        plot_coords <- input$dist_click3
      }

      if (
        !is.null(plot_coords$coords_css$y) & !is.null(plot_coords$coords_css$x)
      ) {
        if (
          plot_coords$coords_css$y > 0 &
            plot_coords$coords_css$x > 0 &
            !is.na(input$abnormal_values_factor) &
            input$abnormal_values_factor >= 0
        ) {
          dat <- r$data_with_only_non_missings_over_visits

          dat <- subset(dat, !(dat$LBORNRLO == "" & dat$LBORNRHI == ""))

          dat$LBTESTCD <- factor(dat$LBTESTCD)

          sorti <- levels(dat$LBTESTCD)[
            levels(dat$LBTESTCD) %in% unique(dat$LBTESTCD)
          ]
          dat_filt <- dat[
            dat$TRTP ==
              levels(dat$TRTP)[ceiling(
                plot_coords$coords_css$y / r$globals$zoompx
              )] &
              dat$LBTESTCD ==
                sorti[ceiling(plot_coords$coords_css$x / r$globals$zoompx)],
          ]

          dat_filt$TRTP <- factor(dat_filt$TRTP)

          cex <- shiny::isolate(input$cex.rvbp)
          crit <- input$criterion

          elaborator_plot_ref_pattern(
            data = dat_filt,
            fontsize = 2,
            criterion = crit,
            sorting_vector = sorti[ceiling(
              plot_coords$coords_css$x / r$globals$zoompx
            )],
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
            col = r$theme$ColorBG,
            xpd = TRUE
          )
          text(
            0.5,
            0.6,
            ifelse(
              input$plot_option_switch3 == "hover",
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
            input$plot_option_switch3 == "hover",
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
  shiny::observeEvent(input$apply_ref_plot, {
    if (input$apply_ref_plot >= 1) {
      output$inoutPlot <- shiny::renderPlot(
        {
          dat <- shiny::isolate(r$data_with_only_non_missings_over_visits)

          dat <- subset(dat, !(dat$LBORNRLO == "" & dat$LBORNRHI == ""))

          cex <- shiny::isolate(input$cex.rvbp)
          crit <- shiny::isolate(input$criterion)

          elaborator_plot_ref_pattern(
            data = dat,
            fontsize = cex,
            criterion = crit,
            sorting_vector = levels(dat$LBTESTCD),
            abnormal_value_factor = shiny::isolate(input$abnormal_values_factor)
          )
        },
        res = r$globals$zoompx / 3
      )

      output$tab3 <- shiny::renderUI({
        shiny::req(r$data_param)

        hpx <- r$data_param$ntreat

        wpx <- r$data_param$nlab2
        zoompx <- r$globals$zoompx
        # panelheight <- r$globals$panelheight
        panelheight <- shiny::isolate(r$globals$panelheight)

        shiny::wellPanel(
          style = paste0(
            "background: ",
            r$theme$ColorBG,
            ";overflow-x:scroll; max-height:",
            panelheight,
            "px"
          ),
          shiny::plotOutput(
            outputId = ns("inoutPlot"),
            height = paste0(hpx * zoompx, 'px'),
            width = paste0(wpx * zoompx, 'px'),
            hover = shiny::clickOpts(
              session$ns("dist_hover3"),
              clip = FALSE
            ),
            click = shiny::clickOpts(
              session$ns("dist_click3"),
              clip = FALSE
            )
          )
        )
      })
    }
  })
  output$cont3 <- shiny::renderUI({
    aq <- ns("apply_ref_plot")
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

  output$cont3_text <- shiny::renderUI({
    HTML(paste0(
      "<b style='color: #47d2bc;'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"
    ))
  })

  shiny::observeEvent(input$apply_ref_plot, {
    aq <- ns("apply_ref_plot")
    output$cont3 <- shiny::renderUI({
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
    output$cont3_text <- shiny::renderUI({
      HTML("")
    })
  })

  shiny::observeEvent(
    c(
      input$cex.rvbp,
      input$criterion,
      input$abnormal_values_factor,
      r$globals$select.visit,
      r$globals$select.treatments,
      r$globals$select.lab,
      r$globals$select.toleratedPercentage,
      r$globals$go3
    ),
    {
      aq <- ns("apply_ref_plot")
      output$cont3 <- shiny::renderUI({
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

      output$cont3_text <- shiny::renderUI({
        HTML(paste0(
          "<b style='color: #47d2bc;'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"
        ))
      })
    }
  )
  })
}
