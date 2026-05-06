#' Path to shipped demo `.RData` (if installed)
#'
#' Demo data must live under `inst/extdata/elaborator_demo.RData` so it is
#' available via [base::system.file()] after `R CMD INSTALL`.
#'
#' @return Single string; empty if the file was not bundled with the package.
#' @keywords internal
elaborator_demo_rdata_path <- function() {
  system.file("extdata", "elaborator_demo.RData", package = "elaborator")
}

#' Whether demo `.RData` is present at the installed package path.
#' @keywords internal
elaborator_demo_rdata_available <- function() {
  p <- elaborator_demo_rdata_path()
  nzchar(p) && file.exists(p)
}
