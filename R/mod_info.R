#' info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_info_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    list(
      HTML(
        "<h2>The Concept of the e<b>lab</b>orator for Clinical Trial Laboratory Data</h2>
        The e<b>lab</b>orator app provides a <i>complete overview</i> of laboratory results for each laboratory parameter
        and treatment group in a matrix-like structure. All the results related to a specific laboratory parameter are shown in one column.
        Results for a treatment group are presented within a row.
        By providing this overview, you will be able to
        identify <i>differences between treatment groups, similarities in laboratory parameters</i> and <i>frequent patterns</i>.<br> <br>
        By using various types of analyses you will be able to view  your laboratory data from different perspectives.
        The following three different types of analyses are available:
        <ul> <li> Quantitative trends analysis </li>
        <li> Qualitative trends analysis </li>
        <li> Reference-value based pattern analysis </li></ul>
        You can find a concept description of each type of analysis in the following.
        Available graphic and filter options as well as missing data handling are described below. <br>
        <br>
        <h4><i class='fa fa-chart-line'></i><b><i> &nbsp;Quantitative Trends</i></b></h4>
        Aim: Examine changes in laboratory values across study visits and explore whether changes differ between treatment groups. <br><br>
        This type of analysis depicts the distribution of laboratory parameters in each study visit.
        An example is shown in Figure 1. Figure 1 shows the distribution of platelets (in giga/l) in the 2 mg dose group at
        all four visits during a study ('Randomization', 'Treatment 1', 'End of Treatment' and 'Follow-up 3').
        Distributions are shown using boxplots. The middle 50% of patient-specific values fall inside the box.
        The median value is represented by the horizontal line crossing through the box and might be used as an indicator for the central tendency.
        The whiskers indicate the variability in the values. The upper whisker is derived as the smaller of the maximum observed laboratory value
        and the third quartile (i.e. upper limit of the box) + 1.5 x interquartile range.
        The upper whisker is derived as the larger of the minimum observed laboratory value and
        the first quartile (i.e. lower limit of the box) - 1.5 x interquartile range. Values outside of the box and whiskers (outliers) are indicated as filled points. <br>
        Changes over time can be easily detected by a shift in the boxplots along the y-axis.
        In this example, a  decrease in platelets is observed until the End of Treatment-Visit followed by a subsequent increase between the End of Treatment-Visit and the Follow-Up 3-Visit. <br><br>
        <img src='www/Fig1.png' alt='Graphic cannot be displayed' width='300' height='300'>
        <p> <i><b>Figure 1</b>: Example plot for quantitative trends analysis.
        The distribution of platelets (in giga/l) is shown for the 2 mg dose group at
        four study visits 'Randomization', 'Treatment 1', 'End of Treatment' and 'Follow-up 3'.
        Normal range, i.e. upper limit of normal and lower limit of normal, are indicated by dotted
        horizontal lines. </i></p>
        Click the 'Open/Close Zoom Panel'-button and use the mouse to hover over a specific plot
        (if option 'hover' is selected within the zoom panel) or, alternatively, click on a selected plot
        (if option 'click' is selected) to see an enlarged version. Further options are described below.
        <ul>
        <li> <h6><b>Same scales within lab parameter</b></h6>
        You can select whether the y-axis range is the same as a specific laboratory parameter (default) or
        it has to be on the data in the respective treatment group.
        Using the same range, simplyfies the comparison between the treatment groups.
        Using this option, extreme outliers will not appear (due to the cut-off scale),
        but they are indicated by arrows.
        The values next to the arrow indicate the values of the outliers.
        When 'same scale among treatments' is not ticked, outliers are still shown when present.</li>
        <li> <h6><b>Patient-specific values</b></h6>
        You can permit or plot patient-specific values. When permitted (default),
        patient-specific values will be added as dots to the boxplots.
        Note that outliers are indicated through dots as well and 'belong' to the boxplot
        (i.e. you can not suppress showing outliers).
        Moreover, you can choose whether patient-specific values are sorted
        from smallest to largest (default). When 'draw connection lines' is ticked,
        the patients' measurements at
        study visits are connected. This option can be useful in particular for small patient numbers.
        A blue connection line indicates a decrease, and an orange
        connection line indicates an increase in the values.</li>
        <li> <h6><b>Test for explorative trend detection</b></h6>
        You can search for changes between study visits by applying hypothesis testing.
        Note that a comparison of test results between the treatment groups is only recommended
        for balanced treatment groups, i.e. if treatment groups are of the same size.
        When treatment groups have different sizes, comparisons between treatment groups should
        not be made because of a difference in the statistical power. The figure background is
        colored if the p-value of the respective test falls below a specified local significance
        level (called 'p-value cutoff').
        The background is green for decreases, and yellow for increases (see e.g. Figure 1).  <br>
        The user can choose between two types of tests: the sign test and the t-test.
        The sign test is recommended for general use as it does not rely on distributional
        assumptions. It checks, for a specific laboratory parameter and treatment group,
        whether there are more patients with an increase than patients with a decrease
        between the two visits, or vice versa. Patients with consistent values
        (i.e. without any change in the values) are eliminated when applying the sign test.
        The t-test is recommended for expert users only because test assumptions should apply. <br>
        The user is required to select at least two visits used for detecting any changes.
        If more than two visits are selected the first visit selected is tested against each of
        the remaining visits (pairwise tests). <br>
        No adjustments for multiple testing are performed (tests for several treatment groups,
        several laboratory parameters and eventually several visits). Be aware that the multiple
        testing problem might lead to many falsely detected changes.
        <b>The use of this feature is specifically for exploration,
        where significant test results must be interpreted with caution.</b></li>
        </ul><br>
        <h4><i class='fab fa-buromobelexperte'></i><b><i>&nbsp; Qualitative Trends</i></b></h4>
        Aim: Study frequent time courses and check if they differ between treatment groups. <br><br>
        This type of analysis assesses frequent time courses that are described through
        increases/decreases between two subsequent study visits.
        A patient might, for example, have the following measurements for a specific laboratory parameter:
        Value 3.2 at Randomization Visit; 1.6 at Treatment 1-Visit; 2.9 at the End of Treatment-Visit;
        2.9 at the Follow-Up 3-visit.
        The time course for this patient will be characterized as decrease (from 3.2 to 1.6) -
        increase (from 1.6 to 2.9) - stable (from 2.9 to 2.9). This pattern is represented as '- + ='.<br>
        In this way, the patterns / time courses for each patient can be derived and the frequency of
        each pattern / time course can be counted. The time courses and frequencies are transferred to a
        diagram-like structure. Each cell of this diagram represents one specific pattern / time course.
        The time courses are arranged in a symmetric way within the diagram.
        For example, the time course '+ + +' is represented in the cell in the top, while the 'opposite'
        time course '- - -' is in the cell at the bottom of the diagram.
        There are three entries within each of the cells:
        the first and second entries show the absolute and relative number of subjects in the
        treatment group which have the specific time course, and the third entry shows the
        respective time course. You can use the font size slider to display the entries and
        increase the size of the numbers. By default, the font size is set to 0, that is,
        all entries are blocked.
        When a time course does not occur at all (i.e. the frequency and percentage are 0),
        the entries of the cell are not shown by default.
        <br>
        The frequency of a time course is shown by the color of the cell.
        Darker colors reflect more frequent and lighter colors less frequent time courses.
        The color key is provided on the right side of Figure 2. It can also be suppressed by
        clicking on the 'Open/close'-button above the color legend.
        No more than approx. 5 visits are recommended because diagrams will get too complex
        with increasing number of cells.<br>
        <img src='www/Fig2.png' alt='Graphic cannot be displayed' width='500' height='350'>
        <p><i><b>Figure 2</b>: Example plot for qualitative trends analysis (left) and color key (right).
        Frequent patterns of increases/decreases in platelets between four subsequent study visits within
        the 2mg dose group are shown.
        The background of the cell is colored depending on the frequency of the respective pattern
        (cf. color key).</i></p>
        Use the 'Open/Close Zoom Panel'-button to inspect a specific plot and see details.
        Further options are described below.
        <ul>
        <li><h6><b>Font size</b></h6>
        Use the slider to increase (larger value) or decrease (smaller value).
        If the font size slider is set to 0 (default), no numbers or patterns are printed inside the cells.
        The background colors are more visible when numbers are not printed.
        </li>
        <li><h6><b>Method for defining stability</b></h6>
        Often, laboratory parameters are measured on a continuous scale and measurements have several decimals.
        In this case, it might make sense not to consider very slight changes in laboratory values from one visit to
        another as increases or decreases. Instead laboratory values might be considered equal/stable even
        though they differ slightly.
        This 'tolerated difference' can be controlled by the user.
        By default, stability is defined only when two values are exactly equal, that is, the tolerated
        difference is set at 0.
        There are three options available for determining the tolerated difference:
        <ul>
        <li> Select the option 'IQR' (for interquartile range derived based on patient data at first visit)
        to determine the tolerated difference for each laboratory parameter as a (user-specified) percentage of the IQR. </li>
        <li> Select the option 'range' (i.e., maximum value minus minimum value observed based on patients data at first visit) to determine the tolerated
        difference for each laboratory parameter as a (user-specified) percentage of the range. Note that the range is sensitive to extreme values observed
        in the data (outliers). </li>
        <li> Select the option 'reference range' (i.e., upper limit of normal minus lower limit of normal) to determine the
        tolerated difference for each laboratory parameter as a (user-specified) percentage of the reference range.
        Note that the tolerated difference cannot be calculated for laboratory parameters which do not have a
        reference range defined by both the upper and the lower limit of normal. </li>
        </ul>
        The tolerated differences, derived based on the method you have chosen and the percentage, will be printed
        next to the diagram for each laboratory parameter.
        </li>
        <li><h6><b>Percentage</b></h6>
        Use the slider to specify the percentage of IQR, range or reference range to determine the tolerated
        difference (see also 'Method for defining stability'). If set to 0 (default) the tolerated difference
        is 0, that is, stability is defined only if two values are completely equal. <br>
        The exact value ('tolerated difference') is printed next to the diagram for each laboratory parameter.
        </li>
        <li><h6><b>Color scale</b></h6>
        Use the drop-down menu to select your favorite color scale.
        This color scale is used to color the cell backgrounds.
        The darker the background color, the more frequent the pattern.
        </li>
        </ul>
        <br>
        <h4><i class='fab fa-cloudsmith'></i><b><i>&nbsp; Reference-value Based Patterns</i></b></h4>
        Aim: Assess how many patients have laboratory values outside the normal range during the study
        and whether there is a difference between treatment groups. <br><br>
        The tree diagram consists of a starting point (i.e. the root of the tree) and several layers.
        The first layer represents the first visit, the second layer the second visit, and so on.
        An example for a specific laboratory parameter in the placebo group is shown in Figure 3.
        You are able to track patients during the trial, and identify at which visits abnormal
        laboratory values occur. From the starting point the sample is split up into two groups:
        one group with patients who have laboratory values outside the normal range at the first visit
        (lower path / orange circle) and the other group of patients with laboratory values inside the
        normal range at the first visit (upper path / green circle). Each of the groups is then split
        up based on the laboratory values at the second visit, and so on.
        Note that patients may have different normal ranges for the same laboratory parameter. <br>
        The size of the circles is proportional to the number of patients.
        This enables users to identify frequent patterns (e.g. normal - abnormal - abnormal - normal)
        among visits.
        The total number of patients is depicted inside the circle at the starting point. <br><br>
        No more than approx. 5 visits are recommended because tree structures will get too complex
        with an increasing number of layers.<br>
        Laboratory parameters without reference range(s) are not analysed.
        Thus, for the reference-value based pattern analysis the total number of plots shown might be
        smaller than for the other two analyses types.<br>
        <img src='www/Fig3.png' alt='Graphic cannot be displayed' width='350' height='400'>
        <p> <i><b>Figure 3</b>: Example plot for reference-value based pattern analysis.
        The number of patients with hematocrit (HCT) values within the reference range(green)
        or outside the reference range(orange) at four visits, 'Randomization', 'Treatment 1',
        'End of Treatment' and 'Follow-up 3', for the placebo group are shown. </i></p>
        Use the 'Open/Close Zoom Panel'-button to check a specific plot and see details.
        Further options are described below.
        <ul>
        <li><h6><b>Font size</b></h6>
        Use the slider to increase (larger value) or decrease (smaller value) the font size of the numbers inside the circles.
        When the font size is set to 0 (default), the numbers inside the circles are not shown.
        </li>
        <li><h6><b>Definition of abnormal values</b></h6>
        Choose the definition of abnormal values. The following three options are available:
        <ul>
        <li> Select the option 'above ULN or below LLN' if laboratory values are considered abnormal if they either exceed the upper limit of normal (ULN) or if they fall below the lower limit of normal (LLN). </li>
        <li> Select the option 'above ULN' if laboratory values are considered abnormal only if they exceed the upper limit of normal (ULN). </li>
        <li> Select the option 'below LLN' if laboratory values are considered abnormal only if they fall below the lower limit of normal (LLN). </li>
        </ul>
        </li>
        <li><h6><b> Factor multiplied with ULN or LLN</b></h6>
        Define abnormal values in terms of ULN or LLN multiplied with a positive value.
        For example, if entering the value 1.5 abnormal values will be defined as values above 1.5xULN and/or
        below 1.5xLLN depending on the selection within the option 'Definition of abnormal values'.
        </ul><br>
        <h4><i class='fa fa-file-upload'></i><b>&nbsp; Data Upload</b></h4>
        The data structure and format required for upload is outlined in the &nbsp; <i class='fa fa-file'></i>"
      ),
      shiny::actionLink(ns("link_to_structure_info"), "Data Manual"),
      HTML(
        "-tab. You can inspect the data uploaded to the e<b>lab</b>orator via the &nbsp; <i class='fa fa-file-lines'></i> <b> Raw Data</b>-tab.
        Searching for specific data points is possible within this tab using filters and sorting options in the header of the table. <br>
        Options for omitting laboratory parameters, treatment groups or visits are described below.
        Whenever changing options, please click on 'Create/Update graphs' to refresh the plots.  <br>
        <ul>
        <li>
        <h6><b>Visits (exclude and rearrange)</b></h6>
        Click the 'x' next to the visit to remove visits.
        Please note that this will remove the visit for all laboratory parameters.
        It is not possible to include the visit for some laboratory parameters but to exclude it for others
        (expect by manually setting the values to NA in your data file for the respective laboratory parameter).
        Drag and drop visits to change the order of the visits in the three types of analyses.</li>
        <li>
        <h6><b>Treatment groups (exclude and rearrange)</b></h6>
        Click the 'x' next to the treatment group to remove treatment groups.
        Drag and drop treatment groups to change the order of the treatment groups in the three types of analyses.</li>
        <li>
        <h6><b>Lab parameters</b></h6>
        Click the laboratory parameters in the drop-down menu to deselect laboratory parameters or re-select
        previously omitted laboratory parameters. You can also use the text field to search for specific
        laboratory parameters.<br>
        If you want to change the order of the laboratory parameters,
        please see the section below on 'Graphic Options'.</li>
        </ul><br>
        <h4><i class='fa fa-filter'></i> <b> Filters </b></h4>
        If you want to see only results for a specific subgroup of patients, you can use the filter-tab.
        Note that filtering based on treatment groups, visits or laboratory parameters can also be performed
        within the &nbsp; <i class='fa fa-file-upload'></i> <b> Data Upload</b>-tab.<br><br>
        As a first step, you can choose the variable(s) which you would like to filter on.
        You can apply multiple filters by selecting multiple filter variables.
        When all filter variables are selected, click on '+Add'.
        A menu will appear, which enables the selection of categories (for categorical filter variables)
        or ranges (for continuous filter variables) for each of the selected filtering variables.<br>
        Click on 'Apply Filter Selection' once you have finalized your selection.
        The progress bar at the top shows the approximate percentage of completion.
        If it shows 100% completion, you can inspect the results based on the selected filter options
        by selecting the desired analysis tab and refresh the plots by clicking on 'Create/Update graphs'.
        <br><br>
        <h4><i class='fa fa-cogs'></i> <b>Graphic Options</b></h4>
        The following graphic options are available:
        <ul><li> <h6> <i class='fa fa-arrows-alt'></i><b>&nbsp; Panel/Plot Size</b></h6>
        Adjust the plot size and height by using the sliders and click the 'Create/Update graphs'-button
        to reload the plots. </li>
        <li><h6><i class='fa fa-sort-alpha-down'> </i><b>&nbsp; Arrange Lab Parameters</b></h6>
        Use one of three options to change the arrangement of laboratory parameters in the three types of analyses.
        The following options are available to arrange laboratory parameters:
        <ul>
        <li> Select the option 'as in input' (default) to arrange laboratory parameters according to your
        preference. You can implement your individual arrangement of laboratory parameters by modifying the
        arrangement in your input data file, so that your input data file reflects your preferred arrangement. </li>
        <li> Select the option 'AI' (for artificial intelligence) to use an intelligent data-driven ordering.
        This option searches for an arrangement which locates laboratory parameters with (either positively
        or negatively) correlated changes over time close to each other.
        Use the drop-down menu to select the visits which will be used for deriving the change.
        Several sorting algorithms can be selected by the user.
        The default method is hierarchical clustering combined with optimal leaf ordering.
        More information on the methodology can be found in this "
      ),
      shiny::actionLink(ns("link_to_pdf_view"), "short manual"),
      "."
    ),
    shiny::uiOutput(ns("pdfview")),
    list(
      HTML(
        "For methods that are based on hierarchical clustering, a dendrogram is also shown above the results window.
        When laboratory parameters are not included at a specific visit and if you have chosen this visit for
        defining change, the laboratory parameters cannot be used in the sorting algorithm.
        Therefore, the respective laboratory parameters will simply be relocated
        to the arrangement/list obtained by the algorithm, and thus will be relocated at the last position. </li>
        <li> Select the option 'alphabetically' to arrange laboratory parameters alphabetically. </li>
        </ul>
        After your selection click 'Update Order!' and the 'Create/Update Plots'-button to reload the plots with the
        changed arrangement. <br></li>
        <li><h6><i class='fa fa-palette'></i> <b>&nbsp; Colors</b></h6>
        Use the drop-down menu to select colors for each visit in the quantitative analysis.
        You may e.g. choose one color for visits at which patients were on treatment and another
        color for visits (e.g. randomization, follow-up) at which patients were off treatment.</li><br>
        </ul>
        <h4><b>Missing Value Handling</b></h4>
        The  analyses of a specific laboratory parameter require that the patients data must be complete (non-missing for all included visits).
        The following mechanisms are implemented:
        <ul>
        <li> The e<b>lab</b>orator-app automatically omits study visits for a laboratory parameter if more than 50% of patients
        have missing values for that laboratory parameter.
        You can also change this percentage using the &nbsp; <i class='fa fa-file-upload'></i> <b> Data Upload</b>-tab. </li>
        <li> Patients who have a missing laboratory value at any of the 'considered' visits (i.e., excluding visits with more
        than 50% missing values, see first item, and visits that are manually removed by the user) will be excluded from all
        analyses of the respective laboratory parameter.</li></li>
        </ul>
        There are many different 'patterns' of missing values that might lead to a substantially reduced sample size.
        The user can, however, decide to automatically exclude some visits in order to avoid a possible substantial reduction in the sample size.
        For example, if a specific laboratory parameter is missing at a specific visit for 40% of the subjects, then the analyses can
        only use the remaining 60% of subjects with non-missing values for that visit (assuming no missing values for the remaining
        subjects at any of the other visits).
        A single visit with many missing values can therefore reduce the number of evaluable patients drastically.
        If you want to avoid the exclusion of too many subjects due to a large percentage of missing values at a specific visit
        (and accept the omission of visits instead), you can set the percentage of 'tolerated' missing values
        (which is by default set to 50%) to a small value (down to 25%, which is the smallest possible value).<br> <br>
        You can check the number of patients per treatment group used for all analyses of a specific laboratory parameters
        by clicking on the 'Open/Close Zoom Panel'. In the lower part, detailed information about the numbers used for the analysis
        of a specific laboratory parameter is shown as well as missing values at each considered visit.
        Note that the number of subjects analyzed might differ between the laboratory parameters, because the laboratory parameters
        are analyzed independently of each other.<br><br>
        The following example illustrates which subjects and visits will be used in the analysis if missing data exist. <br>
        <h5> <b> Example </b></h5>
        The data of a study consists of three subjects and two laboratory parameters hematocrit (HCT) and hemoglobin (HGB). The user has not changed the percentage of 'tolerated' missing values, and therefore the default of 50% is used.
        The original data is summarized below. <br> <br>
        <table>
        <tr>
        <th colspan='1'> </th>
        <th colspan='3'>HCT</th>
        <th colspan='3'>HGB</th>
        </tr>
        <tr>
        <th>Subject</th>
        <th>Visit 1</th>
        <th>Visit 2</th>
        <th>Visit 3</th>
        <th>Visit 1</th>
        <th>Visit 2</th>
        <th>Visit 3</th>
        </tr>
        <tr>
        <th> 1 </th>
        <th> 42.8 </th>
        <th class = 'color-orange'> NA </th>
        <th class = 'color-orange'> NA </th>
        <th>  13.8</th>
        <th>  13.8</th>
        <th>  14.1</th>
        </tr>
        <tr>
        <th>2</th>
        <th> 41.2 </th>
        <th class = 'color-orange'> NA </th>
        <th>  42.2</th>
        <th>  16.2</th>
        <th>  15.8</th>
        <th>  16.4</th>
        </tr>
        <tr>
        <th>3</th>
        <th class = 'color-orange'> NA </th>
        <th> 40.9 </th>
        <th>  40.7</th>
        <th class = 'color-orange'> NA </th>
        <th>  14.3</th>
        <th>  13.3</th>
        </tr>
        </table>
        <br>
        <i> Which visits will be omitted for each of the two laboratory parameters? </i>
        <ul>
        <li>Visit 2 will be automatically omitted for HCT since more than 50% of the values are missing.
        Visits 1 and 3 remain for HCT and will be used in the analyses. </li>
        <li> No visit will be omitted for HGB. At maximum 1/3 of the values are missing, thus all three visits will be saved for HGB.
        Visits which are not automatically deleted will be referred to as 'considered' visits in the following.
        </li></ul>
        <i> Which subjects will be omitted from the analyses for each of the two laboratory parameters? </i>
        <ul>
        <li> Subject 1 will not be used for the analysis of HCT because this subject has a missing value at the considered visit 3.
        In contrast, subject 1 has no missing value for any of the considered visits 1, 2 and 3, and is therefore included in the analysis of HGB.
        </li>
        <li> Subject 2 is included in both the analyses of HCT and HGB because it has no missing values for any of the considered visits (note that visit 2 has been omitted for HCT).
        </li>
        <li> Subject 3 is excluded for both HCT and HGB because it has a missing value at any of the considered visit.
        </li></ul>
        "
      )
    )
  )
}

mod_info_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rs <- session$userData$root
    if (is.null(rs)) {
      rs <- session
    }
    shiny::observeEvent(input$link_to_pdf_view, {
      output$pdfview <- shiny::renderUI({
        shiny::tags$iframe(
          style = "height:500px; width:100%",
          src = "www/Seriation_methods_20191115.pdf"
        )
      })
    })
    shiny::observeEvent(input$link_to_structure_info, {
      shinydashboard::updateTabItems(rs, "sidebarmenu", "datamanual")
    })
  })
}
