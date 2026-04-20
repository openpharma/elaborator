test_that("qualitative module server has id and r", {
  expect_type(mod_qualitative_server, "closure")
  expect_setequal(names(formals(mod_qualitative_server)), c("id", "r"))
})

test_that("module ui works", {
  ui <- mod_qualitative_ui(id = "test")
  golem::expect_shinytaglist(ui)
  fmls <- formals(mod_qualitative_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
