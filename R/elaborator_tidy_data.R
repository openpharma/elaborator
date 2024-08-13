#' old version (needs to be removed)

elaborator_tidy_data <- function(
      elab_data,
      tolerated_value
) {

      nest_elab_data <- elab_data %>%
        dplyr::group_by(AVISIT, TRTP, LBTESTCD) %>%
        tidyr::nest_legacy()

      nonmissing <- unlist(
        lapply(
          nest_elab_data %>%
            pull(data),
          function(x){sum(!is.na(x$LBORRES))}
        )
      )

      nonMissingVisits <- cbind(
        nest_elab_data %>%
          select(-data),
        nonmissing
      )


      tmp1.2 <- elab_data %>%
        dplyr::select(SUBJIDN,TRTP) %>%
        dplyr::distinct() %>%
        dplyr::group_by(TRTP) %>%
        dplyr::summarise(tot = n()) %>%
        dplyr::full_join(elab_data, by ="TRTP") %>%
        dplyr::full_join(
          nonMissingVisits,
          by = c("AVISIT","TRTP","LBTESTCD")
        ) %>%
        dplyr::mutate(percentage = nonmissing/tot)


      tmp2.2 <- tmp1.2 %>%
        dplyr::group_by(AVISIT, LBTESTCD) %>%
        dplyr::summarise(tr = min(percentage))

      tmp3.2 <- tmp2.2 %>%
        dplyr::full_join(tmp1.2, by = c("AVISIT","LBTESTCD")) %>%
        dplyr::filter(tr >= tolerated_value) %>%
        dplyr::select(colnames(elab_data)) %>%
        dplyr::ungroup()

      tmp3.2 <- tmp3.2 %>%
        dplyr::mutate(LBTESTCD = forcats::fct_explicit_na(LBTESTCD, "NA"))

      countVisits.2 <- tmp3.2 %>%
        dplyr::group_by(LBTESTCD) %>%
        dplyr::group_modify(~ data.frame(nr_visits = length(unique(.x$AVISIT))))


      tmp3.2_gb <- tmp3.2 %>%
        dplyr::group_by(SUBJIDN, LBTESTCD) %>%
        tidyr::nest_legacy()


      nonmiss <- unlist(
        lapply(
          tmp3.2_gb %>%
            dplyr::pull(data),
          function(x){sum(!is.na(x$LBORRES))}
        )
      )

      nonMissing.2 <- cbind(tmp3.2_gb %>%
                              dplyr::select(SUBJIDN, LBTESTCD), nonmiss)


      res.2 <- nonMissing.2 %>%
        dplyr::full_join(countVisits.2, by = c("LBTESTCD")) %>%
        dplyr::filter(nr_visits == nonmiss) %>%
        dplyr::ungroup() %>%
        dplyr::select(SUBJIDN, LBTESTCD)

    as.data.frame(
      tmp1.2 %>%
        dplyr::right_join(res.2, by = c("SUBJIDN","LBTESTCD")) %>%
        dplyr::select(colnames(tmp3.2))
    )
}
