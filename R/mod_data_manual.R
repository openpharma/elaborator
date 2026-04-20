#' data_manual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_data_manual_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    list(
      HTML(
        "<h2>File Format and Structure </h2>
            <h4>File Format</h4>
            Currently, the following two file formats are supported:
            <ul>
            <li> A <b>c</b>omma <b>s</b>eparated <b>v</b>alues (CSV) file </li>
            <li> An RData file <br>
            </ul><br>
            <h4>File Structure</h4>
            In order to use the e<b>lab</b>orator, your laboratory data file has to include the following columns:<br>
            <ul>
            <li>  a subject identifier (called <kbd>SUBJIDN</kbd>) </li>
            <li>  the visit (called <kbd>AVISIT</kbd>) </li>
            <li>  the treatment group (called <kbd>TRTP</kbd>) </li>
            <li>  an (abbreviated) name of the laboratory parameter (called <kbd>LBTESTCD</kbd>) </li>
            <li>  the laboratory value measurement (called <kbd>LBORRES</kbd>) </li>
            <li>  the lower limit of normal (LLN) (called <kbd>LBORNRLO</kbd>) </li>
            <li>  the upper limit of normal (ULN) (called <kbd>LBORNRHI</kbd>) </li>
            </ul>
            <h5>Example</h5>
            The first 6 lines of an <i> examplary dataset </i> are shown in the following.<br>
            <ul>
            <samp>
            SUBJIDN &ensp;          AVISIT &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;           TRTP &ensp;&ensp; LBTESTCD LBORRES LBORNRLO LBORNRHI<br>
            100080021    Randomization &ensp;&ensp;&ensp;Placebo      HGB &ensp;&ensp;&ensp;&ensp;&ensp;    15.2 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>
            100080021    Visit 5 &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp; Placebo      HGB &ensp;&ensp;&ensp;&ensp;&ensp;    15.3 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>
            100080021 End of Treatment Placebo      HGB &ensp;&ensp;&ensp;&ensp;&ensp;    15.9 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>
            100080021        Follow-up &ensp;&ensp;&ensp;&ensp;&ensp;&ensp; Placebo      HGB &ensp;&ensp;&ensp;&ensp;&ensp;    16.2 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>
            100080053    Randomization &ensp;&ensp; 1 mg &ensp;&ensp;        HGB &ensp;&ensp;&ensp;&ensp;&ensp;    14.7 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>
            100080053          Visit 5 &ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp;&ensp; 1 mg &ensp;&ensp;         HGB &ensp;&ensp;&ensp;&ensp;&ensp;    13.9 &ensp;&ensp;    12.0 &ensp;&ensp;&ensp;    16.0<br>
            </samp>
            </ul>
            <br>
            <h4>Important points to consider</h4>
            <ul>
            <li> Missing laboratory values must be coded as NA . We recommend carefully reading the section
            on <i>Handling Missing Data</i> in the &nbsp; <i class='fa fa-info'></i> "
      ),
      shiny::actionLink(ns("link_to_tab_info"), "Information"),
      HTML(
        "-tab for correct interpretation. The section describes in detail how the e<b>lab</b>orator deals with missing data. </li>
            <li> If a laboratory parameter has no lower  or upper limit of normal, please do not insert any character in the respective cell but leave the cell empty or use the NA coding. Please do not use blank/space. </li>
            <li> Variable names must be spelled correctly as shown above (please use upper case letters). </li>
            <li> Do not use special characters for variable names or laboratory parameter names. </li>
            <li> All laboratory measurements have to be numeric. That means, do not use '+', '-', '>', '<', 'negative' etc. For example, '<1' is not a valid laboratory measurement. </li>
            <li> <b> Please always check your data carefully before uploading it to the e<b>lab</b>orator.  </b> You can also inspect the data loaded in the e<b>lab</b>orator app via the &nbsp; <i class='fa fa-file-lines'></i> <b> Raw Data</b>-tab. </li>
            </ul>
            "
      )
    )
  )
}

mod_data_manual_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rs <- session$userData$root
    if (is.null(rs)) {
      rs <- session
    }
    shiny::observeEvent(input$link_to_tab_info, {
      shinydashboard::updateTabItems(rs, "sidebarmenu", "helptext")
    })
  })
}
