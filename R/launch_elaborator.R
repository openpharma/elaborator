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
#' @param zoompx graphic option: zoom / pixel ratio (defaults to 100)
#' @param panelheight graphic option: panel height (defaults to 500)
#' @param tolPercent tolerated missings percentage (defaults to 50)
#' @param same_axes_per_treatment_logical same scales within lab parameter in quantitative trends (defaults to FALSE)
#' @param outliers_logical use outlier corrected scale in quantitative trends (defaults to FALSE)
#' @param draw_points_logical add patient-specific values in quantitative trends (defaults to FALSE)
#' @param sort_points sort patient-specific values (only relevant if draw_points_logical = TRUE) in quantitative trends (defaults to FALSE)
#' @param lines_data draw connection lines in quantitative trends (defaults to FALSE)
#' @param con_lin_options options for connection lines (only relevant if lines_data = TRUE) in quantitative trends (options: "first_last", "each_visit", "custom_visits", "all_grey"; defaults to "first_last")
#' @param visit_choices_lin if con_lin_options = "custom_visits", a list of visits to be displayed in the panel must be specified here (these are the possible visit choices). Only choose this option if you know what you are doing! There is no check against the data if these visits actually exist.
#' @param custom_visits_lin if con_lin_options = "custom_visits", then exactly two visits of the visit_choices_lin must be specified. Only choose this option if you know what you are doing! There is no check against the data if these visits actually exist.
#' @param stattest choose statistical test in quantitative trends (options: "none", "signtest", "ttest"; defaults to "none")
#' param trtcompar if stattest != "none" in quantitative trends
#' @param pcutoff p-value cutoff in quantitative trends
#' @param cex_trend font size in qualitative trends (defaults to 0)
#' @param qual_method method for defining stable trend in qualitative trends (options: "InQuRa", "Range", "Reference Range" ; defaults to "InQuRa")
#' @param qual_percent select percentage for defining stable trend in qualitative trends (defaults to 0)
#' @param colChoice select colors in qualitative trends (options: "sequential orange", "sequential blue", "sequential green", "sequential grey", "sequential purple", "sequential red", "sequential blue - green", "sequential blue - purple", "sequential green - blue", "sequential orange - red", "sequential purple - blue", "sequential purple - blue - green", "sequential purple - red", "sequential red - purple", "sequential yellow - green", "sequential yellow - green - blue", "sequential yellow - orange - brown", "sequential yellow - orange - red"; ""defaults to "sequential orange")
#' @param cex_rvbp font size in reference value based patterns (defaults to 0)
#' @param refrange_criterion criterion for definition of abnormal values (above ULN or below LLN, above ULN, below LLN) in reference value based patterns (options: "within", "greater", "less"; defaults to "within")
#' @param abnormal_values_factor factor multiplied with ULN or LLN in reference value based patterns (defaults to 1)
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
    browser = NULL,
    zoompx = 100,
    panelheight = 500,
    tolPercent = 50,
    same_axes_per_treatment_logical = FALSE,
    outliers_logical = FALSE,
    draw_points_logical = FALSE,
    sort_points = FALSE,
    lines_data = FALSE,
    con_lin_options = "first_last",
    visit_choices_lin = NULL,
    custom_visits_lin = NULL,
    stattest = "none",
    # visit_choices_compar = NULL,
    # trtcompar = NULL,
    pcutoff = 0.01,
    cex_trend = 0,
    qual_method = "InQuRa",
    qual_percent = 0,
    colChoice = "sequential orange",
    cex_rvbp = 0,
    refrange_criterion = "within",
    abnormal_values_factor = 1,
    file = NULL
  ){

  # List all arguments of the function call
  apppars <- as.list(environment())


  # check parameter options ####

  # parameters for graphic options
  if(zoompx < 10 | zoompx > 280){stop("Parameter zoompx must be between 10 and 280")}
  if(panelheight < 400 | panelheight > 2400){stop("Parameter panelheight must be between 400 and 2400")}

  # parameters for data upload
  if(tolPercent < 0 | tolPercent > 100){stop("Parameter tolPercent must be between 0 and 100")}

  # parameters for quantitative trends
    # parameters with TRUE/FALSE
  for(i in c("same_axes_per_treatment_logical", "outliers_logical", "draw_points_logical", "sort_points", "lines_data")){
    if(!(get(i) %in% c(TRUE, FALSE))){stop(paste("Parameter", i, "must be TRUE or FALSE"))}
  }
    # parameters for patient-specific values
  if(sort_points == TRUE & draw_points_logical == FALSE){
    apppars["draw_points_logical"]<- TRUE
    warning("Parameter sort_points was set to TRUE but draw_points_logical was FALSE. Parameter draw_points_logical was set to TRUE, assuming you want to display patient-specific values.", immediate. = TRUE)
  }
    # parameters for line drawing
  if(!(con_lin_options %in% c("first_last", "each_visit", "custom_visits", "all_grey"))){stop(paste0("Parameter con_lin_options must be one of the following: ", toString(c("first_last", "each_visit", "custom_visits", "all_grey")), "." ))}
  if(con_lin_options != "first_last" & lines_data == FALSE){
    apppars["lines_data"]<- TRUE
    warning("Parameter con_lin_options was not the default but lines_data was FALSE. Parameter lines_data was set to TRUE, assuming you want to display connection lines.", immediate. = TRUE)
  }
  if((con_lin_options == "custom_visits" | !is.null(visit_choices_lin)| !is.null(custom_visits_lin)) & any(con_lin_options != "custom_visits", is.null(visit_choices_lin), is.null(custom_visits_lin))){
    stop("If individual choices for custom visits should be chosen, all three parameters custom_visits_lin, visit_choices_lin, and custom_visits_lin need to be specified. Only choose this option, if you know what you are doing! There are no checks against the data if these visits actually exist.")
  }
  if(!is.null(custom_visits_lin) & (any(!(custom_visits_lin %in% visit_choices_lin)) | length(custom_visits_lin) != 2)){stop("custom_visits_lin must be a subset of visit_choices_lin of length 2.")}

    # parameters for statistical tests
  if(pcutoff < 0 | pcutoff > 0.2){stop("Parameter pcutoff must be between 0 and 0.2")}
  if(pcutoff != 0.01 & stattest == "none"){
    warning("Parameter pcutoff was not the default but stattest was FALSE. Check, if a statistical test should have been chosen by parameter stattest.", immediate. = TRUE)
  }

  # parameters for qualitative trends
  if(!(qual_method %in% c("InQuRa", "Range", "Reference Range"))){stop(paste0("Parameter qual_method must be one of the following: ", toString(c("InQuRa", "Range", "Reference Range")), "." ))}
  if(qual_percent < 0 | qual_percent > 20){stop("Parameter qual_percent must be between 0 and 20")}
  colChoices<- c("sequential orange", "sequential blue", "sequential green", "sequential grey", "sequential purple", "sequential red", "sequential blue - green", "sequential blue - purple", "sequential green - blue", "sequential orange - red", "sequential purple - blue", "sequential purple - blue - green", "sequential purple - red", "sequential red - purple", "sequential yellow - green", "sequential yellow - green - blue", "sequential yellow - orange - brown", "sequential yellow - orange - red")
  if(!(colChoice %in% colChoices)){stop(paste0("Parameter colChoice must be one of the following: ", toString(colChoices), "." ))}
  if(!(refrange_criterion %in% c("within", "greater", "less"))){stop("Parameter refrange_criterion must be one of the following: ", toString(c("within", "greater", "less")), ".")}
  if(abnormal_values_factor < 0){stop("Parameter abnormal_values_factor must have values greater or equal to 0")}

  server_env <- environment(elaborator_server)
  # server_env$apppars <- apppars

  elaborator_app <- shiny::shinyApp(ui = elaborator_ui(apppars = apppars), server = elaborator_server)
  on_ex_browser <- options()$browser
  on.exit(options(browser = on_ex_browser))
  if (!is.null(browser)) options(browser = browser)

  shiny::runApp(elaborator_app, host = host, port = port)
}
