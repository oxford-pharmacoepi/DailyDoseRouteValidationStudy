###### the numerator check assesses whether the concentration multiplied with the denominator value equals the numerator value
##### carried out in the drug strength table

######### patterns

######### amount and amount unit

amount_unit_db <- cdm$concept %>%
  select(
    "amount_unit_concept_id" = "concept_id",
    "amount_unit" = "concept_name"
  ) %>%
  compute()

numerator_unit_db <- cdm$concept %>%
  select(
    "numerator_unit_concept_id" = "concept_id",
    "numerator_unit" = "concept_name"
  ) %>%
  compute()

denominator_unit_db <- cdm$concept %>%
  select(
    "denominator_unit_concept_id" = "concept_id",
    "denominator_unit" = "concept_name"
  ) %>%
  compute()

ingredient_db <- cdm$concept %>%
  select(
    "ingredient_concept_id" = "concept_id",
    "ingredient_name" = "concept_name"
  ) %>%
  compute()

drug_db <- cdm$concept %>%
  select(
    "drug_concept_id" = "concept_id",
    "drug_name" = "concept_name"
  ) %>%
  compute()

x <- cdm$drug_strength %>%
  left_join(drug_db, by = "drug_concept_id") %>%
  left_join(ingredient_db, by = "ingredient_concept_id") %>%
  mutate(amount = if_else(is.na(amount_value), NA, "numeric")) %>%
  left_join(amount_unit_db, by = "amount_unit_concept_id") %>%
  mutate(numerator = if_else(is.na(numerator_value), NA, "numeric")) %>%
  left_join(numerator_unit_db, by = "numerator_unit_concept_id") %>%
  mutate(denominator = if_else(is.na(denominator_value), NA, "numeric")) %>%
  left_join(denominator_unit_db, by = "denominator_unit_concept_id") %>%
  select(
    drug_concept_id, ingredient_concept_id, amount, amount_unit, amount_unit_concept_id, numerator,
    numerator_unit, numerator_unit_concept_id, denominator, denominator_unit, amount_value,
    numerator_value, denominator_value, denominator_unit_concept_id, ingredient_name, drug_name
  ) %>%
  compute()

amount_international_unit <- x %>%
  dplyr::filter(amount_unit_concept_id == 8718 & !is.na(amount_value) &
                  is.na(numerator_unit_concept_id)  & is.na(denominator_unit_concept_id)) %>%
  select("drug_name","ingredient_name","amount_value","amount_unit") %>% collect()



amount_microgram <- x %>%
  dplyr::filter(amount_unit_concept_id == 9655 & !is.na(amount_value) &
                  is.na(numerator_unit_concept_id)  & is.na(denominator_unit_concept_id)) %>%
  select("drug_name","ingredient_name","amount_value","amount_unit") %>% collect()


amount_milliequivalent <- x %>%
  dplyr::filter(amount_unit_concept_id == 9551 & !is.na(amount_value) &
                  is.na(numerator_unit_concept_id)  & is.na(denominator_unit_concept_id)) %>%
  select("drug_name","ingredient_name","amount_value","amount_unit") %>% collect()


amount_milligram <- x %>%
  dplyr::filter(amount_unit_concept_id == 8576 & !is.na(amount_value) &
                  is.na(numerator_unit_concept_id)  & is.na(denominator_unit_concept_id)) %>%
  select("drug_name","ingredient_name","amount_value","amount_unit") %>% collect()


amount_milliliter <- x %>%
  dplyr::filter(amount_unit_concept_id == 8587 & !is.na(amount_value) &
                  is.na(numerator_unit_concept_id)  & is.na(denominator_unit_concept_id)) %>%
  select("drug_name","ingredient_name","amount_value","amount_unit") %>% collect()


######### numerator and PRESENT denominator


numerator_international_unit_denominator_milliliter <- x %>%
  dplyr::filter(is.na(amount_unit_concept_id) & is.na(amount_value) &
                  !is.na(numerator) & numerator_unit_concept_id == 8718 &
                  !is.na(denominator) & denominator_unit_concept_id == 8587) %>%
  select("drug_name","ingredient_name","numerator_value","denominator_value") %>% collect()


numerator_international_unit_denominator_milligram <- x %>%
  dplyr::filter(is.na(amount_unit_concept_id) & is.na(amount_value) &
                  !is.na(numerator) & numerator_unit_concept_id == 8718 &
                  !is.na(denominator) & denominator_unit_concept_id == 8576) %>%
  select("drug_name","ingredient_name","numerator_value","denominator_value") %>% collect()


numerator_microgram_denominator_hour <- x %>%
  dplyr::filter(is.na(amount_unit_concept_id) & is.na(amount_value) &
                  !is.na(numerator) & numerator_unit_concept_id == 9655 &
                  !is.na(denominator) & denominator_unit_concept_id == 8505) %>%
  select("drug_name","ingredient_name","numerator_value","denominator_value") %>% collect()


numerator_milliequivalent_denominator_milliliter <- x %>%
  dplyr::filter(is.na(amount_unit_concept_id) & is.na(amount_value) &
                  !is.na(numerator) & numerator_unit_concept_id == 9551 &
                  !is.na(denominator) & denominator_unit_concept_id == 8587) %>%
  select("drug_name","ingredient_name","numerator_value","denominator_value") %>% collect()


numerator_milligram_denominator_actuation <- x %>%
  dplyr::filter(is.na(amount_unit_concept_id) & is.na(amount_value) &
                  !is.na(numerator) & numerator_unit_concept_id == 8576 &
                  !is.na(denominator) & denominator_unit_concept_id == 45744809) %>%
  select("drug_name","ingredient_name","numerator_value","denominator_value") %>% collect()



numerator_milligram_denominator_hour <- x %>%
  dplyr::filter(is.na(amount_unit_concept_id) & is.na(amount_value) &
                  !is.na(numerator) & numerator_unit_concept_id == 8576 &
                  !is.na(denominator) & denominator_unit_concept_id == 8505) %>%
  select("drug_name","ingredient_name","numerator_value","denominator_value") %>% collect()



numerator_milligram_denominator_liter <- x %>%
  dplyr::filter(is.na(amount_unit_concept_id) & is.na(amount_value) &
                  !is.na(numerator) & numerator_unit_concept_id == 8576 &
                  !is.na(denominator) & denominator_unit_concept_id == 8519) %>%
  select("drug_name","ingredient_name","numerator_value","denominator_value") %>% collect()


numerator_milligram_denominator_milligram <- x %>%
  dplyr::filter(is.na(amount_unit_concept_id) & is.na(amount_value) &
                  !is.na(numerator) & numerator_unit_concept_id == 8576 &
                  !is.na(denominator) & denominator_unit_concept_id == 8576) %>%
  select("drug_name","ingredient_name","numerator_value","denominator_value") %>% collect()




numerator_milligram_denominator_milliliter <- x %>%
  dplyr::filter(is.na(amount_unit_concept_id) & is.na(amount_value) &
                  !is.na(numerator) & numerator_unit_concept_id == 8576 &
                  !is.na(denominator) & denominator_unit_concept_id == 8587) %>%
  select("drug_name","ingredient_name","numerator_value","denominator_value") %>% collect()


numerator_milligram_denominator_square_centimeter <- x %>%
  dplyr::filter(is.na(amount_unit_concept_id) & is.na(amount_value) &
                  !is.na(numerator) & numerator_unit_concept_id == 8576 &
                  !is.na(denominator) & denominator_unit_concept_id == 9483) %>%
  select("drug_name","ingredient_name","numerator_value","denominator_value") %>% collect()


numerator_milliliter_denominator_milligram <- x %>%
  dplyr::filter(is.na(amount_unit_concept_id) & is.na(amount_value) &
                  !is.na(numerator) & numerator_unit_concept_id == 8587 &
                  !is.na(denominator) & denominator_unit_concept_id == 8576) %>%
  select("drug_name","ingredient_name","numerator_value","denominator_value") %>% collect()



numerator_milliliter_denominator_milliliter <- x %>%
  dplyr::filter(is.na(amount_unit_concept_id) & is.na(amount_value) &
                  !is.na(numerator) & numerator_unit_concept_id == 8587 &
                  !is.na(denominator) & denominator_unit_concept_id == 8587) %>%
  select("drug_name","ingredient_name","numerator_value","denominator_value") %>% collect()



table_to_sort <- rbind(numerator_milliliter_denominator_milliliter %>% dplyr::mutate(pattern_id = 1),
                       numerator_milliliter_denominator_milligram  %>% dplyr::mutate(pattern_id = 2),
                       numerator_milligram_denominator_square_centimeter  %>% dplyr::mutate(pattern_id = 3),
                       numerator_milliequivalent_denominator_milliliter %>% dplyr::mutate(pattern_id = 4),
                       numerator_international_unit_denominator_milliliter %>% dplyr::mutate(pattern_id = 5),
                       numerator_international_unit_denominator_milligram %>% dplyr::mutate(pattern_id = 6),
                       numerator_milligram_denominator_actuation %>% dplyr::mutate(pattern_id = 7),
                       numerator_microgram_denominator_hour %>% dplyr::mutate(pattern_id = 8),
                       numerator_milligram_denominator_milligram %>% dplyr::mutate(pattern_id = 9),
                       numerator_milligram_denominator_liter %>% dplyr::mutate(pattern_id = 10),
                       numerator_milligram_denominator_milliliter %>% dplyr::mutate(pattern_id = 11),
                       numerator_milligram_denominator_hour %>% dplyr::mutate(pattern_id = 12))

# Extract all numbers from drug_name
list_numbers <- stringr::str_extract_all(table_to_sort[["drug_name"]], "(?>-)*[[:digit:]]+\\.*[[:digit:]]*")
list_numbers <- lapply(list_numbers, "length<-", max(lengths(list_numbers)))
length_all <- lengths(list_numbers)

# Add columns to table with all the numbers (first = denominator, following = potential numerator (1 to max_length, here 18))
do_number <- function(name, j) {
  name <- enquo(name)
  columnn <- sapply(list_numbers,"[[",j)
  table_to_sort <- table_to_sort %>% dplyr::mutate(!!name := as.numeric(columnn))
  return(table_to_sort)
}

# 18 is a hardcoded number, max number of potential numerator we see
table_to_sort <- do_number("denominator_exct", 1)
table_to_sort <- do_number("numerator_1_exct", 2)
table_to_sort <- do_number("numerator_2_exct", 3)
table_to_sort <- do_number("numerator_3_exct", 4)
table_to_sort <- do_number("numerator_4_exct", 5)
table_to_sort <- do_number("numerator_5_exct", 6)
table_to_sort <- do_number("numerator_6_exct", 7)
table_to_sort <- do_number("numerator_7_exct", 8)
table_to_sort <- do_number("numerator_8_exct", 9)
table_to_sort <- do_number("numerator_9_exct", 10)
table_to_sort <- do_number("numerator_10_exct", 11)
table_to_sort <- do_number("numerator_11_exct", 12)
table_to_sort <- do_number("numerator_12_exct", 13)
table_to_sort <- do_number("numerator_13_exct", 14)
table_to_sort <- do_number("numerator_14_exct", 15)
table_to_sort <- do_number("numerator_15_exct", 16)
table_to_sort <- do_number("numerator_16_exct", 17)
table_to_sort <- do_number("numerator_17_exct", 18)

check_num <- function(name) {
  name_enq <- enquo(name)
  table_to_sort <- table_to_sort %>%
    dplyr::mutate(!!name_enq := eval(as.symbol(name)) *denominator_value)
  return(table_to_sort)
}

table_to_sort <- check_num("numerator_1_exct")
table_to_sort <- check_num("numerator_2_exct")
table_to_sort <- check_num("numerator_3_exct")
table_to_sort <- check_num("numerator_4_exct")
table_to_sort <- check_num("numerator_5_exct")
table_to_sort <- check_num("numerator_6_exct")
table_to_sort <- check_num("numerator_7_exct")
table_to_sort <- check_num("numerator_8_exct")
table_to_sort <- check_num("numerator_9_exct")
table_to_sort <- check_num("numerator_10_exct")
table_to_sort <- check_num("numerator_11_exct")
table_to_sort <- check_num("numerator_12_exct")
table_to_sort <- check_num("numerator_13_exct")
table_to_sort <- check_num("numerator_14_exct")
table_to_sort <- check_num("numerator_15_exct")
table_to_sort <- check_num("numerator_16_exct")
table_to_sort <- check_num("numerator_17_exct")

check_num2 <- function(name) {
  table_to_sort <- table_to_sort %>%
    dplyr::mutate(numerator_check = ifelse(eval(as.symbol(name)) == numerator_value && !is.na(eval(as.symbol(name))), numerator_check + 1, numerator_check))
  return(table_to_sort)
}

table_to_sort <- table_to_sort %>% dplyr::mutate(numerator_check = 0)
table_to_sort <- check_num2("numerator_1_exct")
table_to_sort <- check_num2("numerator_2_exct")
table_to_sort <- check_num2("numerator_3_exct")
table_to_sort <- check_num2("numerator_4_exct")
table_to_sort <- check_num2("numerator_5_exct")
table_to_sort <- check_num2("numerator_6_exct")
table_to_sort <- check_num2("numerator_7_exct")
table_to_sort <- check_num2("numerator_8_exct")
table_to_sort <- check_num2("numerator_9_exct")
table_to_sort <- check_num2("numerator_10_exct")
table_to_sort <- check_num2("numerator_11_exct")
table_to_sort <- check_num2("numerator_12_exct")
table_to_sort <- check_num2("numerator_13_exct")
table_to_sort <- check_num2("numerator_14_exct")
table_to_sort <- check_num2("numerator_15_exct")
table_to_sort <- check_num2("numerator_16_exct")
table_to_sort <- check_num2("numerator_17_exct")

# Numerator_check does the numerator_value always equals one of the extracted "numerators"?
numerator_check_result <- table_to_sort %>% dplyr::filter(numerator_check != 1)

write.csv(numerator_check_result, file = here::here(resultsFolder,"numerator_check_result.csv"))
