test_that("trees module server has id and r", {
  expect_type(mod_trees_server, "closure")
  expect_setequal(names(formals(mod_trees_server)), c("id", "r"))
})

test_that("module ui works", {
  ui <- mod_trees_ui(id = "test")
  golem::expect_shinytaglist(ui)
  fmls <- formals(mod_trees_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
