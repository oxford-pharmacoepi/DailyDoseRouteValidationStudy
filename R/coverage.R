amount <- unique_no.NA(validpattern$amount_unit_concept_id)
numerator <- unique_no.NA(validpattern$numerator_unit_concept_id)
denominator <- unique_no.NA(validpattern$denominator_unit_concept_id)


## coverage of valid patterns in drug strength table

strength_ids_amount <- cdm$drug_strength %>%
  dplyr::select("drug_concept_id", "amount_unit_concept_id","numerator_unit_concept_id","denominator_unit_concept_id") %>%
  dplyr::filter(amount_unit_concept_id %in% amount & is.na(numerator_unit_concept_id) & is.na(denominator_unit_concept_id)) %>%
  dplyr::select("drug_concept_id") %>%
  collect()

strength_ids_conc <- cdm$drug_strength %>%
  dplyr::select("drug_concept_id", "amount_unit_concept_id","numerator_unit_concept_id","denominator_unit_concept_id") %>%
  dplyr::filter(numerator_unit_concept_id %in% numerator & denominator_unit_concept_id %in% denominator) %>%
  dplyr::select("drug_concept_id") %>%
  collect()

results_strength_help <- bind_rows(

    strengths_ids_tally <- rbind(strength_ids_amount,strength_ids_conc) %>%
        distinct() %>%
        tally(),

    total_strengths <- cdm$drug_strength %>% collect() %>% tally(),

    coverage_strength <- (strengths_ids_tally/total_strengths)*100
      )

rownames <- c("count","total","coverage")

results_strength <- cbind(rownames,results_strength_help) %>%
  pivot_wider(names_from = rownames, values_from = n )

write.csv(results_strength, here::here("results_strength.csv"))


## coverage of valid patterns in drug exposure table

strengths_ids <- rbind(strength_ids_amount,strength_ids_conc) %>%
  distinct() %>% collect()

results_exposure_help <- bind_rows(

  exposure_tally <- cdm$drug_exposure %>%
    dplyr::select("drug_concept_id") %>%
    inner_join(strengths_ids, by = "drug_concept_id", copy = TRUE) %>%
    distinct() %>%
    collect() %>%
    tally(),

  total_exposure  <- cdm$drug_exposure %>% dplyr::select("drug_concept_id") %>% distinct() %>% collect() %>% tally(),

  coverage_exposure <- (exposure_tally/total_exposure)*100
)

rownames <- c("count","total","coverage")

results_exposure <- cbind(rownames,results_exposure_help) %>%
  pivot_wider(names_from = rownames, values_from = n )

write.csv(results_exposure, here::here("results_exposure.csv"))
