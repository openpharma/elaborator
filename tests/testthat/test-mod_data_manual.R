test_that("data manual module server has id and r", {
  expect_type(mod_data_manual_server, "closure")
  expect_setequal(names(formals(mod_data_manual_server)), c("id", "r"))
})

test_that("module ui works", {
  ui <- mod_data_manual_ui(id = "test")
  golem::expect_shinytaglist(ui)
  fmls <- formals(mod_data_manual_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
