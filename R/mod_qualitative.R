#' qualitative UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_qualitative_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::conditionalPanel(
      condition = "output.flag == false",
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
      shinydashboard::box(
        width = NULL,
        title = span(shiny::tagList(' ', icon("cogs"))),
        background = 'black',
        solidHeader = TRUE,
        collapsible = TRUE,
        shiny::fluidRow(
          shiny::column(
            2,
            bsplus::bs_embed_tooltip(
              tag = h5(span(shiny::tagList(
                "Font size",
                icon("question")
              ))),
              title = "Adapt font size. Set font size to 0 to exclude any text.",
              placement = "top",
              expanded = TRUE
            ),
            shiny::sliderInput(
              inputId = ns("cex.trend"),
              label = '',
              min = 0,
              max = 5,
              value = 0,
              step = 0.5
            )
          ),
          shiny::column(
            2,
            bsplus::bs_embed_tooltip(
              tag = h5(span(shiny::tagList(
                "Choose method for defining stability",
                icon("question")
              ))),
              title = "You can specify a tolerated difference in which a change in two adjacent lab values are considered stable ('='). This tolerated difference can be derived as a (small) percentage of the interquartile range (IQR), the range or the reference range. The IQR and the range is evaluated at the first visit across all treatment groups.",
              placement = "bottom",
              expanded = TRUE
            ),
            shinyWidgets::prettyRadioButtons(
              inputId = ns("method"),
              label = ' ',
              choices = c(
                'Interquartile Range' = 'InQuRa',
                'Range' = 'Range',
                'Reference Range' = 'Reference Range'
              ),
              selected = "InQuRa",
              status = "warning"
            )
          ),
          shiny::column(
            2,
            bsplus::bs_embed_tooltip(
              tag = h5(span(shiny::tagList(
                "Select percentage",
                icon("question")
              ))),
              title = "Select a percent value in the method chosen in order to derive the critical boundary. If set to 0, then adjacent lab values must be exactly equal in order to be considered stable.",
              placement = "top",
              expanded = TRUE
            ),
            shiny::sliderInput(
              inputId = ns("percent"),
              label = "",
              min = 0,
              max = 20,
              value = 0,
              step = 0.5
            )
          ),
          shiny::column(
            2,
            bsplus::bs_embed_tooltip(
              tag = h5(span(shiny::tagList(
                "Select a color scale",
                icon("question")
              ))),
              title = "Select your favorite color scale used for highlighting frequent patterns.",
              placement = "top",
              expanded = TRUE
            ),
            shinyWidgets::pickerInput(
              inputId = ns("select.pal1"),
              label = "",
              choices = names(elaborator_app_theme()$colChoice),
              selected = names(elaborator_app_theme()$colChoice)[1],
              multiple = FALSE,
              options = list(
                `live-search` = TRUE,
                `style` = 'background: btn-warning',
                `header` = 'Select item'
              )
            ),
            shiny::plotOutput(outputId = ns("prev.pal1"), height = '20px')
          ),
          shiny::column(
            width = 2,
            offset = 4,
            shiny::helpText(
              class = "color-white",
              "You can minimize/maximize this window with the -/+ button on the top right of the panel"
            )
          )
        )
      ),
      shiny::conditionalPanel(
        condition = "output.ai == true",
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
                outputId = ns("dendro_2"),
                height = "250px"
              )
            )
          )
        )
      ),
      shiny::conditionalPanel(
        condition = "output.flag == true",
        shiny::fluidRow(
          shiny::column(
            2,
            shiny::actionButton(
              inputId = ns("apply_qual_plot"),
              label = paste0('Create Plots'),
              icon = icon("object-group")
            ),
            shiny::uiOutput(ns("cont2"))
          ),
          shiny::column(
            5,
            offset = 2,
            shiny::uiOutput(ns("cont2_text"))
          )
        )
      ),
      shiny::uiOutput(ns("tab2"), width = 'auto'),
      shiny::uiOutput(ns("legendpanel")),
      shiny::uiOutput(ns("hoverpanel2"))
    )
  )
}

mod_qualitative_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$dendro_2 <- shiny::renderPlot({
      shiny::req(r$globals$clusterMethod)
      cm <- r$globals$clusterMethod
      if (startsWith(cm, "OLO") || startsWith(cm, "GW")) {
        shiny::req(r$prepare_dist_matrix_for_clustering)
        tmp <- r$prepare_dist_matrix_for_clustering
        ser <- seriation::seriate(
          elaborator_calculate_spearman_distance(tmp),
          method = cm
        )
        asdendro <- stats::as.dendrogram(ser[[1]])
        dendro2 <- dendextend::assign_values_to_leaves_edgePar(dend = asdendro)

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
        on.exit(suppressWarnings(graphics::par(on_ex)), add = TRUE)
        graphics::par(bg = r$theme$ColorBG)
        graphics::plot(dendro2, ylab = "Distance", horiz = FALSE)
      }
    })

    output$legend <- shiny::renderPlot(
      {
        on_ex <- graphics::par("mfrow", "oma", "mar")
        on.exit(graphics::par(on_ex))
        graphics::par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
        graphics::plot(
          NULL,
          NULL,
          ylim = c(0, 10),
          xlim = c(0, 1),
          axes = FALSE,
          ylab = "",
          xlab = ""
        )
        leg.x <- 0.5
        leg.y <- seq(
          graphics::grconvertY(0, 'npc', 'user'),
          graphics::grconvertY(1, 'npc', 'user'),
          length.out = 12
        )
        leg.width <- 1
        graphics::rect(
          xleft = leg.x - 2,
          xright = leg.x + 2,
          ybottom = leg.y[-1],
          ytop = leg.y[-length(leg.y)],
          xpd = NA,
          col = c(c(
            'white',
            r$theme$colChoice[[shiny::req(input$select.pal1)]]$col,
            'black'
          )),
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
            'black',
            'black',
            'black',
            'black',
            'black',
            'black',
            'white',
            'white',
            'white',
            'white',
            'white'
          )
        )
      },
      width = 84.53
    )

    output$legendpanel <- shiny::renderUI({
      shiny::absolutePanel(
        id = ns("legendpanel"),
        class = "modal-content",
        fixed = TRUE,
        draggable = TRUE,
        top = 240,
        left = "auto",
        right = 50,
        bottom = "auto",
        width = 84.53,
        height = "auto",

        shiny::tags$button(
          class = "btn",
          style = "background: #f6ad82; color:#ffffff",
          `data-toggle` = "collapse",
          `data-target` = paste0("#", ns("demo_co")),
          shiny::HTML("Open/Close")
        ),
        tags$div(
          id = ns("demo_co"),
          class = "collapse in",
          shiny::fluidRow(
            shiny::column(
              2,
              shiny::plotOutput(outputId = ns("legend"))
            )
          )
        ),
        style = "z-index: 10;"
      )
    })

    output$hoverpanel2 <- shiny::renderUI({
      nvi <- r$data_param$nvisit

      shiny::absolutePanel(
        id = ns("hoverpanel2"),
        class = "modal-content",
        fixed = TRUE,
        draggable = TRUE,
        HTML(paste0("<div style='background-color:", r$theme$ColorBG, "'>")),
        shiny::tags$button(
          class = "btn",
          style = "background: #f6ad82; color:#ffffff",
          `data-toggle` = "collapse",
          `data-target` = paste0("#", ns("demo_qual")),
          shiny::HTML(
            "<i class=\"fa-solid fa-search-plus\"></i> Open/Close Zoom Panel"
          )
        ),
        top = 70,
        left = "auto",
        right = 100,
        bottom = "auto",
        width = nvi * 100,
        height = "auto",
        tags$div(
          id = ns("demo_qual"),
          class = "collapse",
          shiny::fluidRow(
            shiny::column(
              2,
              shiny::plotOutput(outputId = ns("hover2"), height = "400px")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              12,
              offset = 4,
              shiny::radioButtons(
                inputId = ns("plot_option_switch2"),
                label = NULL,
                choices = c("hover", "click"),
                selected = c("hover"),
                inline = TRUE
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(12, shiny::uiOutput(ns("hover_info_text2")))
          )
        ),
        style = "z-index: 10;"
      )
    })

    output$hover_info_text2 <- shiny::renderUI({
      input$apply_qual_plot
      shiny::req(r$data_filtered_by_app_selection, input$plot_option_switch2)

      # switch between hover or click options for zoom panel
      if (input$plot_option_switch2 == "hover") {
        plot_coords <- input$dist_hover2
      } else if (input$plot_option_switch2 == "click") {
        plot_coords <- input$dist_click2
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
            trtp_levels_vec <- dat %>%
              dplyr::pull(.data$TRTP) %>%
              levels()
            hover_treatment <- trtp_levels_vec[ceiling(y / isolate(r$globals$zoompx))]

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
    shiny::observe({
      output$hover2 <- shiny::renderPlot(
        {
          input$apply_qual_plot
          shiny::req(
            r$data_with_selected_factor_levels,
            r$Summary_for_qualitative_trends,
            input$plot_option_switch2
          )
          # switch between hover or click options for zoom panel
          if (input$plot_option_switch2 == "hover") {
            plot_coords <- input$dist_hover2
          } else if (input$plot_option_switch2 == "click") {
            plot_coords <- input$dist_click2
          }

          if (
            !is.null(plot_coords$coords_css$y) &
            !is.null(plot_coords$coords_css$x)
          ) {
            if (plot_coords$coords_css$y > 0 & plot_coords$coords_css$x > 0) {
              dat <- r$data_with_only_non_missings_over_visits
              Variab <- levels(dat$LBTESTCD)[
                levels(dat$LBTESTCD) %in% unique(dat$LBTESTCD)
              ]

              dat_filt <- dat[
                dat$TRTP ==
                  levels(dat$TRTP)[ceiling(
                    plot_coords$coords_css$y / r$globals$zoompx
                  )] &
                  dat$LBTESTCD ==
                  Variab[ceiling(plot_coords$coords_css$x / r$globals$zoompx)],
              ]
              dat_filt$TRTP <- factor(dat_filt$TRTP)

              Summa <- r$Summary_for_qualitative_trends

              meth <- input$method
              suppressWarnings(
                elaborator_plot_qual_trends(
                  dat1 = dat_filt,
                  Variab[ceiling(plot_coords$coords_css$x / r$globals$zoompx)],
                  fontsize = 2,
                  method = meth,
                  color_palette = c(
                    'white',
                    r$theme$colChoice[[shiny::req(input$select.pal1)]]$col,
                    'black'
                  ),
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
                  input$plot_option_switch2 == "hover",
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
                input$plot_option_switch2 == "hover",
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
        width = r$data_param$nvisit * 100
      )
    })
    output$prev.pal1 <- shiny::renderPlot({
      col <- c('white', r$theme$colChoice[[shiny::req(input$select.pal1)]]$col, 'black')
      elaborator_draw_scheme_preview(x = col)
    })
    shiny::observeEvent(input$apply_qual_plot, {
      #requirements
      shiny::req(r$data_with_selected_factor_levels)
      if (input$apply_qual_plot > 0) {
        #output of Qualitative trend plots
        output$trendPlot <- shiny::renderPlot(
          {
            shiny::req(r$Summary_for_qualitative_trends)

            dat <- shiny::isolate(r$data_with_only_non_missings_over_visits)

            cex <- shiny::isolate(input$cex.trend)
            Variab <- levels(dat$LBTESTCD)[
              levels(dat$LBTESTCD) %in% unique(dat$LBTESTCD)
            ]

            meth <- shiny::isolate(input$method)
            Summa <- shiny::isolate(r$Summary_for_qualitative_trends)
            elaborator_plot_qual_trends(
              dat1 = dat,
              Variab,
              fontsize = cex,
              method = meth,
              color_palette = c(
                'white',
                r$theme$colChoice[[shiny::req(isolate(input$select.pal1))]]$col,
                'black'
              ),
              Summa = Summa
            )
          },
          res = isolate(r$globals$zoompx) / 3
        )

        output$tab2 <- shiny::renderUI({
          shiny::req(r$data_param)
          hpx <- r$data_param$ntreat
          wpx <- r$data_param$nlab

          zoompx <- shiny::isolate(r$globals$zoompx)
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
              outputId = ns("trendPlot"),
              height = paste0(hpx * zoompx, 'px'),
              width = paste0(wpx * zoompx, 'px'),
              hover = shiny::clickOpts(
                session$ns("dist_hover2"),
                clip = FALSE
              ),
              click = shiny::clickOpts(session$ns("dist_click2"), clip = FALSE)
            )
          )
        })
      }
    })
    output$cont2 <- shiny::renderUI({
      aq <- ns("apply_qual_plot")
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

    output$cont2_text <- shiny::renderUI({
      HTML(paste0(
        "<b style='color: #47d2bc; border-color: #f78300'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"
      ))
    })

    shiny::observeEvent(input$apply_qual_plot, {
      aq <- ns("apply_qual_plot")
      output$cont2 <- shiny::renderUI({
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
      output$cont2_text <- shiny::renderUI({
        HTML("")
      })
    })

    shiny::observeEvent(
      c(
        input$cex.trend,
        input$method,
        input$percent,
        r$globals$select.visit,
        input$select.pal1,
        r$globals$select.treatments,
        r$globals$select.lab,
        r$globals$select.toleratedPercentage,
        r$globals$go3
      ),
      {
        aq <- ns("apply_qual_plot")
        output$cont2 <- shiny::renderUI({
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

        output$cont2_text <- shiny::renderUI({
          HTML(paste0(
            "<b style='color: #47d2bc;'> Please use the 'Create/Update Plots'-button on the left side to update settings!</b>"
          ))
        })
      }
    )
  })
}
