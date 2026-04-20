testServer(
  mod_options_server,
  args = list(
    id = "options",
    r = shiny::reactiveValues(
      data_param = list(ntreat = 1L, nvisit = 1L, nlab = 1L, nlab2 = 1L),
      start_ai = shiny::reactiveValues(dat = FALSE),
      globals = shiny::reactiveValues()
    )
  ),
  {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )
    # Here are some examples of tests you can
    # run on your module
    # - Testing the setting of inputs
    # session$setInputs(x = 1)
    # expect_true(input$x == 1)
    # - If ever your input updates a reactiveValues
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))
})
 
test_that("module ui works", {
  ui <- mod_options_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_options_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})
 
