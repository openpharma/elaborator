.registered_elaborator_www <- new.env(parent = emptyenv())

register_elaborator_static_assets <- function() {
  if (isTRUE(get0("done", envir = .registered_elaborator_www, ifnotfound = FALSE))) {
    return(invisible(NULL))
  }
  www_dir <- system.file("www", package = "elaborator")
  if (nzchar(www_dir) && dir.exists(www_dir)) {
    shiny::addResourcePath("www", www_dir)
    .registered_elaborator_www$done <- TRUE
  }
  invisible(NULL)
}

.onLoad <- function(libname, pkgname) {
  register_elaborator_static_assets()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the elaborator App!")
}
