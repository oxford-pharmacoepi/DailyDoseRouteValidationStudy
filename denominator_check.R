############# check what is going on between denominator missing versus denominator present for a few patterns
############# and compare with quantity
############# and give the number of concepts per pattern

drug_exposure <- cdm$drug_exposure %>%
  dplyr::group_by(quantity,drug_concept_id) %>%
  dplyr::count() %>%
  CDMConnector::computeQuery()

overall_match <- cdm$drug_strength %>%
  dplyr::select("drug_concept_id","denominator_value","denominator_unit_concept_id","numerator_value","numerator_unit_concept_id") %>%
  dplyr::filter(numerator_unit_concept_id %in% c(8718,9655,9551,8576,8587) &
                  denominator_unit_concept_id %in% c(8576,8587,8505,45744809,8519,9483)) %>%
  dplyr::left_join(select(cdm$concept, "concept_name", "concept_id"), by = c("drug_concept_id" = "concept_id" )) %>%
  dplyr::inner_join(drug_exposure, by = "drug_concept_id") %>%
  CDMConnector::computeQuery()


# IU per milligram
pattern_isna1 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 8718 &
                  denominator_unit_concept_id == 8576  &
                  !is.na(numerator_value) &
                  is.na(denominator_value)) %>% collect()

write.csv(pattern_isna1, file = here::here(resultsFolder,"pattern_isna1.csv"))


pattern_notna1 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 8718 &
                  denominator_unit_concept_id == 8576  &
                  !is.na(numerator_value) &
                  !is.na(denominator_value)) %>% collect()

write.csv(pattern_notna1, file = here::here(resultsFolder,"pattern_notna1.csv"))


# IU per milliliter
pattern_isna2 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id ==  8718 &
                  denominator_unit_concept_id ==  8587 &
                  !is.na(numerator_value) &
                  is.na(denominator_value))  %>% collect()

write.csv(pattern_isna2, file = here::here(resultsFolder,"pattern_isna2.csv"))


pattern_notna2 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id ==  8718 &
                  denominator_unit_concept_id ==  8587 &
                  !is.na(numerator_value) &
                  !is.na(denominator_value))  %>% collect()

write.csv(pattern_notna2, file = here::here(resultsFolder,"pattern_notna2.csv"))


#mega international unit per milliliter
pattern_isna3 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id ==  9573 &
                  denominator_unit_concept_id ==  8587 &
                  !is.na(numerator_value) &
                  is.na(denominator_value))   %>% collect()

write.csv(pattern_isna3, file = here::here(resultsFolder,"pattern_isna3.csv"))


pattern_notna3 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id ==  9573 &
                  denominator_unit_concept_id ==  8587 &
                  !is.na(numerator_value) &
                  !is.na(denominator_value))   %>% collect()

write.csv(pattern_notna3, file = here::here(resultsFolder,"pattern_notna3.csv"))


#microgram per hour
pattern_isna4 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 9655 &
                  denominator_unit_concept_id == 8505 &
                  !is.na(numerator_value) &
                  is.na(denominator_value))   %>% collect()

write.csv(pattern_isna4, file = here::here(resultsFolder,"pattern_isna4.csv"))


pattern_notna4 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 9655 &
                  denominator_unit_concept_id == 8505 &
                  !is.na(numerator_value) &
                  !is.na(denominator_value))   %>% collect()

write.csv(pattern_notna4, file = here::here(resultsFolder,"pattern_notna4.csv"))


#milliequivalent per milligram
pattern_isna5 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 9551 &
                  denominator_unit_concept_id == 8576 &
                  !is.na(numerator_value) &
                  is.na(denominator_value)) %>% collect()

write.csv(pattern_isna5, file = here::here(resultsFolder,"pattern_isna5.csv"))


pattern_notna5 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 9551 &
                  denominator_unit_concept_id == 8576 &
                  !is.na(numerator_value) &
                  !is.na(denominator_value)) %>% collect()

write.csv(pattern_notna5, file = here::here(resultsFolder,"pattern_notna5.csv"))


#milliequivalent per milliliter
pattern_isna6 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 9551 &
                  denominator_unit_concept_id == 8587 &
                  !is.na(numerator_value) &
                  is.na(denominator_value)) %>% collect()

write.csv(pattern_isna6, file = here::here(resultsFolder,"pattern_isna6.csv"))


pattern_notna6 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 9551 &
                  denominator_unit_concept_id == 8587 &
                  !is.na(numerator_value) &
                  !is.na(denominator_value)) %>% collect()

write.csv(pattern_notna6, file = here::here(resultsFolder,"pattern_notna6.csv"))


#milligram per actuation
pattern_isna7 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 8576 &
                  denominator_unit_concept_id == 45744809 &
                  !is.na(numerator_value) &
                  is.na(denominator_value))  %>% collect()

write.csv(pattern_isna7, file = here::here(resultsFolder,"pattern_isna7.csv"))


pattern_notna7 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 8576 &
                  denominator_unit_concept_id == 45744809 &
                  !is.na(numerator_value) &
                  !is.na(denominator_value))  %>% collect()

write.csv(pattern_notna7, file = here::here(resultsFolder,"pattern_notna7.csv"))


#milligram per hour
pattern_isna8 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 8576 &
                  denominator_unit_concept_id == 8505 &
                  !is.na(numerator_value) &
                  is.na(denominator_value))  %>% collect()

write.csv(pattern_isna8, file = here::here(resultsFolder,"pattern_isna8.csv"))


pattern_notna8 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 8576 &
                  denominator_unit_concept_id == 8505 &
                  !is.na(numerator_value) &
                  !is.na(denominator_value))  %>% collect()

write.csv(pattern_notna8, file = here::here(resultsFolder,"pattern_notna8.csv"))


# milligram per liter
pattern_isna9 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 8576 &
                  denominator_unit_concept_id == 8519 &
                  !is.na(numerator_value) &
                  is.na(denominator_value))   %>% collect()

write.csv(pattern_isna9, file = here::here(resultsFolder,"pattern_isna9.csv"))


pattern_notna9 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 8576 &
                  denominator_unit_concept_id == 8519 &
                  !is.na(numerator_value) &
                  !is.na(denominator_value))   %>% collect()

write.csv(pattern_notna9, file = here::here(resultsFolder,"pattern_notna9.csv"))


#milligram per milligram
pattern_isna10 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 8576 &
                  denominator_unit_concept_id == 8576 &
                  !is.na(numerator_value) &
                  is.na(denominator_value))  %>% collect()

write.csv(pattern_isna10, file = here::here(resultsFolder,"pattern_isna10.csv"))


pattern_notna10 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 8576 &
                  denominator_unit_concept_id == 8576 &
                  !is.na(numerator_value) &
                  !is.na(denominator_value))  %>% collect()

write.csv(pattern_notna10, file = here::here(resultsFolder,"pattern_notna10.csv"))


# milligram per milliliter
pattern_isna11 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 8576 &
                  denominator_unit_concept_id == 8587 &
                  !is.na(numerator_value) &
                  is.na(denominator_value))  %>% collect()

write.csv(pattern_isna11, file = here::here(resultsFolder,"pattern_isna11.csv"))


pattern_notna11 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 8576 &
                  denominator_unit_concept_id == 8587 &
                  !is.na(numerator_value) &
                  !is.na(denominator_value))  %>% collect()

write.csv(pattern_notna11, file = here::here(resultsFolder,"pattern_notna11.csv"))


# milligram per square centimeter
pattern_isna12 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id ==  8576 &
                  denominator_unit_concept_id ==  9483 &
                  !is.na(numerator_value) &
                  is.na(denominator_value))  %>% collect()

write.csv(pattern_isna12, file = here::here(resultsFolder,"pattern_isna12.csv"))


pattern_notna12 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id ==  8576 &
                  denominator_unit_concept_id ==  9483 &
                  !is.na(numerator_value) &
                  !is.na(denominator_value))  %>% collect()

write.csv(pattern_notna12, file = here::here(resultsFolder,"pattern_notna12.csv"))


# milliliter per milligram
pattern_isna13 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 8587 &
                  denominator_unit_concept_id == 8576 &
                  !is.na(numerator_value) &
                  is.na(denominator_value))  %>% collect()

write.csv(pattern_isna13, file = here::here(resultsFolder,"pattern_isna13.csv"))


pattern_notna13 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 8587 &
                  denominator_unit_concept_id == 8576 &
                  !is.na(numerator_value) &
                  !is.na(denominator_value))  %>% collect()

write.csv(pattern_notna13, file = here::here(resultsFolder,"pattern_notna13.csv"))


#milliter per milliliter
pattern_isna14 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 8587 &
                  denominator_unit_concept_id == 8587 &
                  !is.na(numerator_value) &
                  is.na(denominator_value)) %>% collect()

write.csv(pattern_isna14, file = here::here(resultsFolder,"pattern_isna14.csv"))


pattern_notna14 <- overall_match %>%
  dplyr::filter(numerator_unit_concept_id == 8587 &
                  denominator_unit_concept_id == 8587 &
                  !is.na(numerator_value) &
                  !is.na(denominator_value)) %>% collect()

write.csv(pattern_notna14, file = here::here(resultsFolder,"pattern_notna14.csv"))


n_tibble <- tibble::tibble(name = c(""), n = 0)
for(i in c(1:14)) {
  name_not <- paste0("pattern_notna",i)
  name_is <- paste0("pattern_isna",i)
  write.csv(eval(rlang::sym(name_not)), file = here::here(resultsFolder,paste0(name_not,".csv")))
  write.csv(eval(rlang::sym(name_is)), file = here::here(resultsFolder,paste0(name_is,".csv")))
  n_not = eval(rlang::sym(name_not)) %>% summarise(nrow(eval(rlang::sym(name_not)))) %>% pull()
  n_is = eval(rlang::sym(name_is)) %>% summarise(nrow(eval(rlang::sym(name_is)))) %>% pull()
  n_tibble <- rbind(n_tibble,
                    tibble::tibble(
                      name = c(name_not, name_is),
                      n = c(n_not, n_is)
                    )
  )
}

n_tibble <- n_tibble[-1,]

#add a column with the name and a column with the number of concept ids

write.csv(n_tibble, file = here::here(resultsFolder,"n_patterns.csv"))
