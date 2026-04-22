.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the elaborator App!")
  shiny::addResourcePath('www', system.file("app/www", package = "elaborator"))
}


