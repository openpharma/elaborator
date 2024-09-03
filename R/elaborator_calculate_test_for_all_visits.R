#' Perform pairwise statistical tests for quantitative trend plots
#'
#' @param elab_data nested elaborator data with possibly up to three nests 'raw',
#' @param Visit1 first visit for statistical tests (only one allowed)
#' @param Visit2 second visit(s) for statistical tests (multiple visits applicable - pairwise tests performed)
#' @param sign_test character string if sign test or t-test should be performed ("signtest"/"ttest")
#' @param pcutoff p value for test(s)
#'
#'@return tibble with test results (estimate/p-values) and color code for quantitative trend graphs
#'
#'@keywords internal

elaborator_calculate_test_for_all_visits <- function(
  elab_data,
  Visit1,
  Visit2,
  sign_test,
  pcutoff
) {

  SUBJIDN <- AVISIT <- TRTP <- LBTESTCD <- LBORRES <- LBTESTCD <- TRTP <- NULL

  #transfer long format lab data to wide format by visits
  tmp <- elab_data %>%
    dplyr::select(SUBJIDN,AVISIT,TRTP,LBTESTCD,LBORRES) %>%
    pivot_wider(names_from = AVISIT, values_from = LBORRES)

  #if sign test is selected
  if (sign_test == "signtest") {

    #helper function to compare labvalues to Visit1
    prep_sign_test <- function(x) {
      dplyr::case_when(
        tmp %>% pull(Visit1) < x ~ TRUE,
        tmp %>% pull(Visit1) > x ~ FALSE,
        tmp %>% pull(Visit1) == x ~ NA,
        is.na(tmp %>% pull(Visit1) < x) ~ NA
      )
    }

    tmp2 <- tmp %>%
      #use helper function for every Visit2
      dplyr::mutate_at(Visit2, prep_sign_test) %>%
      dplyr::select(SUBJIDN,TRTP,LBTESTCD, tidyr::all_of(Visit2)) %>%
      #transform back to long format
      tidyr::pivot_longer(cols = Visit2, names_to = "AVISIT") %>%
        dplyr::group_by(LBTESTCD,TRTP,AVISIT) %>%
        #use group_modify to perform tests for each group (LBTESTCD,TRTP,AVISIT)
        dplyr::group_modify(~ {
          data.frame(
            "p.value" = ifelse(
              sum(!is.na(.x$value))>0,
              stats::binom.test(x = sum(.x$value, na.rm = TRUE), n = sum(!is.na(.x$value)), p = 0.5)$p.value,
              NA
            ),
            "estimate" = ifelse(
              sum(!is.na(.x$value))>0,
              stats::binom.test(x = sum(.x$value, na.rm = TRUE), n = sum(!is.na(.x$value)), p = 0.5)$estimate,
              NA
            )
          )
        }) %>%
        #create columns with test color in quantitative trend plots
        dplyr::mutate(
          estimate_directions = dplyr::case_when(
            p.value < pcutoff & estimate < 0.5 ~ "#47d2bc30",
            p.value < pcutoff & estimate > 0.5 ~ "#ffeeaa50",
            is.na(p.value) ~ "#d3d3d330",
            p.value >= pcutoff ~ ""
          )
        )
  } else if (sign_test == "ttest") {
    #helper function for t-test
    prep_t_test <- function(x) {
      tmp %>% dplyr::pull(Visit1) - x
    }

    tmp2 <- tmp %>%
      dplyr::mutate_at(Visit2, prep_t_test) %>%
      dplyr::select(SUBJIDN,TRTP,LBTESTCD, all_of(Visit2)) %>%
      tidyr::pivot_longer(cols = Visit2, names_to = "AVISIT") %>%
        dplyr::group_by(LBTESTCD,TRTP,AVISIT) %>%
        dplyr::group_modify(~ {
          data.frame(
            "p.value" = ifelse(
              sum(!is.na(.x$value))>0,
              stats::t.test(x = .x$value)$p.value,
              NA
            ),
            "estimate" = ifelse(
              sum(!is.na(.x$value))>0,
              stats::t.test(x = .x$value)$estimate,
              NA
            )
          )
        }
      ) %>%
      #create columns with test color in quantitative trend plots
      dplyr::mutate(estimate_directions = dplyr::case_when(
        p.value < pcutoff & estimate > 0 ~ "#47d2bc30",
        p.value < pcutoff & estimate < 0 ~ "#ffeeaa50",
        is.na(p.value) ~ "#d3d3d360",
        p.value >= pcutoff ~ ""
      ))
  }
  tmp2
}

