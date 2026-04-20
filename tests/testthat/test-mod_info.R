test_that("info module server has id and r", {
  expect_type(mod_info_server, "closure")
  expect_setequal(names(formals(mod_info_server)), c("id", "r"))
})

test_that("module ui works", {
  ui <- mod_info_ui(id = "test")
  golem::expect_shinytaglist(ui)
  fmls <- formals(mod_info_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
