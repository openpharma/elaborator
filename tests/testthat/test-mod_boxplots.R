test_that("boxplots module server has id and r", {
  expect_type(mod_boxplots_server, "closure")
  expect_setequal(names(formals(mod_boxplots_server)), c("id", "r"))
})

test_that("module ui works", {
  ui <- mod_boxplots_ui(id = "test")
  golem::expect_shinytaglist(ui)
  fmls <- formals(mod_boxplots_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
