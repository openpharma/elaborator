#' loading function for elaborator data, also performs checks for required variables
#'
#' @param data_switch app widgets input wheter rdata or csv files or demo data should be uploaded
#' @param rdata_file_path path of rdata file
#' @param csv_file_path path of csv file
#' @param loaded_file saved file for demo data (outdated)
#' @param separator separator (for csv file upload only)
#' @param quote quote (for csv file upload only)
#' @param decimal decimal (for csv file upload only)
#'
#' @return list with data and error message
#'
#' @keywords internal
#'

elaborator_load_and_check <- function(
  data_switch = '*.RData file',
  rdata_file_path = NULL,
  csv_file_path = NULL,
  loaded_file = NULL,
  separator = NULL,
  quote = NULL,
  decimal = NULL
) {
  # Shiny can pass NA / length-0 before inputs exist; comparisons must never be NA for if()
  shiny_path <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    if (length(x) == 0L) {
      return(NULL)
    }
    x <- x[[1L]]
    if (is.na(x) || (is.character(x) && !nzchar(x))) {
      return(NULL)
    }
    x
  }
  rdata_file_path <- shiny_path(rdata_file_path)
  csv_file_path <- shiny_path(csv_file_path)
  loaded_file <- shiny_path(loaded_file)

  if (missing(data_switch) || is.null(data_switch) || length(data_switch) == 0L) {
    data_switch <- "*.RData file"
  } else {
    data_switch <- as.character(data_switch[[1L]])
    if (length(data_switch) != 1L || is.na(data_switch)) {
      data_switch <- "*.RData file"
    }
  }

  # need a non-empty data path
    if (!is.null(rdata_file_path) || !is.null(csv_file_path) || !is.null(loaded_file) || data_switch == "Demo data") {
      required_elaborator_vars <- c("SUBJIDN", "AVISIT", "TRTP", "LBTESTCD", "LBORRES", "LBORNRLO", "LBORNRHI")
      if (data_switch == 'Demo data') {
        demo_path <- elaborator_demo_rdata_path()
        if (!nzchar(demo_path) || !file.exists(demo_path)) {
          elaborator_data <- NULL
          error_message <- paste0(
            "Demo data is not bundled with this build of elaborator ",
            "(missing inst/extdata/elaborator_demo.RData)."
          )
        } else {
          elaborator_data <- get(load(demo_path))
          if (!all(required_elaborator_vars %in% names(elaborator_data))) {
            error_message <- paste0(
              "The following required variable(s) <br> is/are missing: <br>",
              paste(required_elaborator_vars[which(!required_elaborator_vars %in% names(elaborator_data))], collapse = ", "),
              ".<br> Please check the data manual <br> for further information."
            )
            elaborator_data <- NULL
          } else {
            error_message <- NULL
          }
        }
      }
      if (data_switch == '*.RData file') {
        if (!is.null(rdata_file_path)) {
          # error message if selected data have a different format than rdata
          if (!utils::tail(strsplit(rdata_file_path, ".", fixed = TRUE)[[1]], n = 1) %in% c("rdata","rData","Rdata","RData")) {
            elaborator_data <- NULL
            error_message <- paste0(
              "Wrong data format. <br> You have selected a ",
              utils::tail(strsplit(rdata_file_path, ".", fixed = TRUE)[[1]], n = 1),
              " file. <br> Please select a .RData file <br> or choose another file format."
            )
            return(
              list(
                data = elaborator_data,
                message = error_message
              )
            )
          } else {
            elaborator_data <- get(load(rdata_file_path))

            # error message if required variables are missing
            if (!all(required_elaborator_vars %in% names(elaborator_data))) {
              error_message <- paste0(
                "The following required variable(s) <br> is/are missing: <br>",
                paste(required_elaborator_vars[which(!required_elaborator_vars %in% names(elaborator_data))], collapse = ", "),
                ".<br> Please check the data manual <br> for further information."
              )
              elaborator_data <- NULL
            } else {
              error_message <- NULL
            }
          }
        } else {
          elaborator_data <- NULL
          error_message <- NULL
        }
      } else if (data_switch == '*.CSV file') {

        if (!is.null(csv_file_path)) {
          # error message if selected data have a different format than csv
          if (utils::tail(strsplit(csv_file_path, ".", fixed = TRUE)[[1]], n = 1) != "csv") {
            elaborator_data <- NULL
            error_message <- paste0(
              "Wrong data format. <br> You have selected a ",
              utils::tail(strsplit(csv_file_path, ".", fixed = TRUE)[[1]], n = 1),
              " file. <br> Please select a .csv file <br> or choose another file format."
            )
          } else {
            elaborator_data <- utils::read.csv(
              csv_file_path,
              row.names = NULL,
              header = TRUE,
              na.strings = c('NA','.',''),
              sep = separator,
              quote = quote,
              dec = decimal
            )


            if ("LBORRES" %in% names(elaborator_data)) {
              if (!is.numeric(elaborator_data$LBORRES)) {
                elaborator_data <- NULL
                error_message <- "Non numeric lab parameter. <br> Select another decimal character!"
                return(list(data = elaborator_data,
                            message = error_message))
              }
            }
            # error message if required variables are missing
            if (!all(required_elaborator_vars %in% names(elaborator_data))) {

              if (all(required_elaborator_vars %in% (strsplit(names(elaborator_data), ".", fixed = TRUE)[[1]]))) {
                error_message <- paste0(
                  "Please change separator and/or quote <br> input as in csv data set. <br>",
                  "For further information <br> check the data manual."
                )
                elaborator_data <- NULL
              } else {
                error_message <- paste0(
                  "The following required variable(s) <br> is/are missing: <br>",
                  paste(required_elaborator_vars[which(!required_elaborator_vars %in% names(elaborator_data))], collapse = ", <br>"),
                  ". <br> Try to change separator and/or quoute <br> input as in csv data set.
                  <br> For further information <br> check the data manual."
                )
                elaborator_data <- NULL
              }
            } else {

                error_message <- NULL
            }
            elaborator_data
          }
        } else {
          elaborator_data <- NULL
          error_message <- NULL
        }
      }
    } else {
      elaborator_data <- NULL
      error_message <- NULL
    }

    # add checks on data:
    #1. check for unique values in SUBJIDN/LBTESTCD/AVISIT
    if (is.null(error_message) & !is.null(elaborator_data)) {
      reduced_elaborator_data <- elaborator_data %>%
        dplyr::select("SUBJIDN","LBTESTCD","AVISIT","TRTP")
      if (any(duplicated(reduced_elaborator_data))) {
        error_message <- paste0("
        The are duplicated values in the data set! Please check the data manual!
        ")
        elaborator_data <- NULL
      }
    }
  return(
    list(data = elaborator_data,
         message = error_message
    )
  )
}

