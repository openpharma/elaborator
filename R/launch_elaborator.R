#' Launches the elaborator application
#'
#' @export
#'
#' @description
#' Starts the elaborator application in the client's browser.
#'
#' @param elaborator_data data derived for the elaborator app
#' @param host host link (defaults to the local machine "127.0.0.1")
#' @param port port number (randomly chosen unless specified as a certain number)
#' @param browser path to browser exe (defaults to standard browser)
#'
#' @keywords elaborator
#'
#' @examples
#' if(interactive()){
#' ## Launch application on localhost (127.0.0.1)
#' ## -------------------------------------------
#' ## By default launch_elaborator starts the application on localhost
#' ## and a randomly selected port (e.g. 9876), in which case you can connect
#' ## to the running application by navigating your browser to
#' ## http://localhost:9876.
#' launch_elaborator()
#'
#' ## Launch application on a different host
#' ## --------------------------------------
#' ## You can also run the application on a different host
#' ## by specifying a hostname and port. Just make sure to
#' ## use an open port on your machine. Here "open" means
#' ## that the port should not be used by another service
#' ## and the port is opened by your firewall.
#' launch_elaborator(host="your-hostname", port = 8888, browser = NULL)
#'
#'
#' ## Make the application available to your coworkers
#' ## ------------------------------------------------
#' ## within your local area network even without a
#' ## dedicated Shiny server. The value set through the
#' ## host argument says to accept any connection (not just from localhost).
#' ## Then take note of your local IP (if you are under linux,
#' ## you can see it through ifconfig). Say your IP is 192.168.1.70.
#' ## Your colleagues can use your app by inserting in the address
#' ## bar of their browser 192.168.1.70:8888, i.e. your IP followed
#' ## by : and the port number you selected.
#' launch_elaborator(host="0.0.0.0", port=8888)
#'
#' ## Launch application on a different browser
#' ## ----------------------------------------
#' ## To run the shiny app on a different browser than your standard browser
#' ## use the "browser" argument to set the path to the respective .exe file.
#' launch_elaborator(browser = "C:/Program Files/Mozilla Firefox/firefox.exe")
#' }
#'
#' @import shiny
#' @import haven
#' @import reshape2
#' @import shape
#' @import shinyWidgets
#' @import shinydashboard
#' @import bsplus
#' @import dplyr
#' @import forcats
#' @importFrom purrr map map_int
#' @importFrom rlang sym
#' @import tidyr
#' @importFrom seriation seriate get_order
#' @import RColorBrewer
#' @import dendextend
#' @importFrom grDevices adjustcolor rgb
#' @importFrom graphics abline arrows axis boxplot grconvertX grconvertY lines mtext par plot points rect text
#' @importFrom stats as.dendrogram as.dist binom.test cor median na.omit quantile runif t.test
#'
#' @return A shiny app

launch_elaborator <- function(
    elaborator_data = NULL,
    host = "0.0.0.0",
    port = NULL,
    browser = NULL
  ){

  apppars <- list(
    elaborator_data = elaborator_data
  )
  server_env <- environment(elaborator_server)
  # server_env$apppars <- apppars

  elaborator_app <- shiny::shinyApp(ui = elaborator_ui, server = elaborator_server)
  on_ex_browser <- options()$browser
  on.exit(options(browser = on_ex_browser))
  if (!is.null(browser)) options(browser = browser)



  shiny::runApp(elaborator_app, host = host, port = port)
}
