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
  decimal = NULL,
  file_creation_data = NULL
) {
  # need a non-empty data path
    if (!is.null(rdata_file_path) || !is.null(csv_file_path) || !is.null(loaded_file) || data_switch == "Demo data"|| data_switch == "Use file creation tab") {
      required_elaborator_vars <- c("SUBJIDN", "AVISIT", "TRTP", "LBTESTCD", "LBORRES", "LBORNRLO", "LBORNRHI")
      if (data_switch == 'Demo data') {
        elaborator_data <- get(load(here::here("data", "elaborator_demo.RData")))
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
      } else if (data_switch == 'Use file creation tab') {
        if (!is.null(file_creation_data)) {
            elaborator_data <- file_creation_data
            error_message <- NULL
        } else {
          elaborator_data <- NULL
          error_message <- paste0(
            "No data send to app"
          )
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
        There are duplicated values in the data set! Please check the data manual!
        ")
        elaborator_data <- NULL
      }
    }
    #2. remove labparameter with no non-empty labvalues
  if (is.null(error_message) & !is.null(elaborator_data)) {
    #get lab parameter with at least one non missing lab value
    lbtestcd_index <- elaborator_data %>%
      dplyr::group_by(LBTESTCD) %>%
      dplyr::summarise(n_non_empty = sum(!is.na(LBORRES))) %>%
      dplyr::filter(n_non_empty > 0) %>%
      dplyr::pull(LBTESTCD) %>%
      as.vector()

    #filter
    elaborator_data <- elaborator_data %>%
      dplyr::filter(LBTESTCD %in% lbtestcd_index)

  }
  return(
    list(data = elaborator_data,
         message = error_message
    )
  )
}

