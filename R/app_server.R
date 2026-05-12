#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 50 * 1024^2)

  session$userData$root <- session

  r <- shiny::reactiveValues()
  r$theme <- elaborator_app_theme()
  r$app_input <- shiny::reactive({
    NULL
  })
  r$start <- shiny::reactiveValues(dat = FALSE)
  r$start_ai <- shiny::reactiveValues(dat = FALSE)
  r$method <- shiny::reactiveValues(val = "InQuRa")
  r$statistical_test_results <- shiny::reactiveValues(var = NULL)
  r$values <- shiny::reactiveValues(default = 0)

  globals_init <- list(
    clusterMethod = "OLO_average",
    go = 0L,
    go3 = 0L,
    zoompx = 100,
    panelheight = 500,
    select.visit = NULL,
    select.treatments = NULL,
    select.lab = NULL,
    select.toleratedPercentage = 50,
    arrange.lab = NULL,
    orderinglab = "alphabetically",
    select.ai.first = NULL,
    select.ai.last = NULL,
    percent = 0
  )
  for (i in seq_len(20L)) {
    globals_init[[paste0("id", i, "-col")]] <- NULL
  }
  r$globals <- do.call(shiny::reactiveValues, globals_init)

  ns_qual <- shiny::NS("qualitative_1")
  ns_upload <- shiny::NS("upload_1")

  shiny::observe({
    r$globals$clusterMethod <- input$clusterMethod
    r$globals$go <- input$go
    r$globals$go3 <- input$go3
    r$globals$zoompx <- input$zoompx
    r$globals$panelheight <- input$panelheight
    r$globals$select.visit <- input[[ns_upload("select.visit")]]
    r$globals$select.treatments <- input[[ns_upload("select.treatments")]]
    r$globals$select.lab <- input[[ns_upload("select.lab")]]
    r$globals$select.toleratedPercentage <-
      input[[ns_upload("select.toleratedPercentage")]]
    r$globals$arrange.lab <- input$arrange.lab
    r$globals$orderinglab <- input$orderinglab
    r$globals$select.ai.first <- input$select.ai.first
    r$globals$select.ai.last <- input$select.ai.last
    r$globals$percent <- input[[ns_qual("percent")]]
  })

  output$flag <- shiny::reactive(r$start$dat)
  shiny::outputOptions(output, "flag", suspendWhenHidden = FALSE)

  output$ai <- shiny::reactive(r$start_ai$dat)
  shiny::outputOptions(output, "ai", suspendWhenHidden = FALSE)

  # Dendrogram panels use output$ai; show when AI mode is on, user has clicked
  # "Update selection!" once (go3 > 0), and method supports dendrogram (OLO/GW).
  # Also react to clusterMethod / orderinglab so switching e.g. OLO→VAT updates
  # visibility without relying only on another go3 click.
  shiny::observeEvent(
    list(input$go3, input$clusterMethod, input$orderinglab),
    {
      ol <- input$orderinglab
      cm <- input$clusterMethod
      go3 <- input$go3
      if (
        !is.null(ol) && ol == "auto" &&
        !is.null(cm) &&
        (startsWith(cm, "OLO") || startsWith(cm, "GW")) &&
        !is.null(go3) && is.numeric(go3) && go3 > 0L
      ) {
        r$start_ai$dat <- TRUE
      } else {
        r$start_ai$dat <- FALSE
      }
    },
    ignoreNULL = FALSE
  )

  ns_box <- shiny::NS("boxplots_1")
  output$check <- shiny::reactive({
    trt <- input[[ns_box("trtcompar")]]
    length(trt)
  })
  shiny::outputOptions(output, 'check', suspendWhenHidden = FALSE)

  mod_upload_server("upload_1", r = r)
  mod_filter_server("filter_1", r = r)
  mod_options_server("options_1", r = r)

  mod_boxplots_server("boxplots_1", r = r)
  mod_qualitative_server("qualitative_1", r = r)
  mod_trees_server("trees_1", r = r)
  mod_info_server("info_1", r = r)
  mod_data_manual_server("data_manual_1", r = r)
}
