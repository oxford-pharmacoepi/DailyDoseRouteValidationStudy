### Check MG/MG and ML/ML in drug strength table



drug_db <- cdm$concept %>%
  select(
    "drug_concept_id" = "concept_id",
    "drug_name" = "concept_name"
  ) %>%
  compute()


conc <- cdm$drug_strength %>% dplyr::select(denominator_value, numerator_value, drug_concept_id) %>%
  dplyr::left_join(drug_db, by = "drug_concept_id") %>%
  dplyr::filter(grepl("MG/MG|ML/ML",drug_name)) %>% collect()



# Extract the numbers before MG/MG and ML/ML
conc_numbers <- stringr::str_extract_all(conc[["drug_name"]], "[[:digit:]]+\\.*[[:digit:]]*\\s(?=MG/MG|ML/ML)")
conc_numbers <- lapply(conc_numbers, "length<-", max(lengths(conc_numbers)))


# Add columns to table with all the numbers (1 to max_length, here 9))

do_number <- function(name,  j) {
  name <- enquo(name)
  columnn <- sapply(conc_numbers,"[[",j)
  conc <- conc %>% dplyr::mutate(!!name := as.numeric(columnn))
  return(conc)
}

# 9 is a hardcoded number, max number of concentrations we see
conc <- do_number("conc_1_exct", 1)
conc <- do_number("conc_2_exct", 2)
conc <- do_number("conc_3_exct", 3)
conc <- do_number("conc_4_exct", 4)
conc <- do_number("conc_5_exct", 5)
conc <- do_number("conc_6_exct", 6)
conc <- do_number("conc_7_exct", 7)
conc <- do_number("conc_8_exct", 8)
conc <- do_number("conc_9_exct", 9)


bigger_than_1 <- conc %>% filter_at(vars(starts_with("conc")), any_vars(. > 1))
bigger_than_2 <- conc %>% filter_at(vars(starts_with("conc")), any_vars(. > 2))

write.csv(bigger_than_1, file = here::here(resultsFolder,"New_check_gt1.csv"))
write.csv(bigger_than_2, file = here::here(resultsFolder,"New_check_gt2.csv"))
