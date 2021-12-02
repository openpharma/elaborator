.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the elaborator App!")
  shiny::addResourcePath('www', system.file("www", package = "elaborator"))
}
