# Creates patternfile csv

# Get columns from cdm$drug_strength table
amount_unit_db <- cdm$concept %>%
  dplyr::select(
    "amount_unit_concept_id" = "concept_id",
    "amount_unit" = "concept_name"
  ) %>%
  compute()

numerator_unit_db <- cdm$concept %>%
  dplyr::select(
    "numerator_unit_concept_id" = "concept_id",
    "numerator_unit" = "concept_name"
  ) %>%
  compute()

denominator_unit_db <- cdm$concept %>%
  dplyr::select(
    "denominator_unit_concept_id" = "concept_id",
    "denominator_unit" = "concept_name"
  ) %>%
  compute()

ingredient_db <- cdm$concept %>%
  dplyr::select(
    "ingredient_concept_id" = "concept_id",
    "ingredient_name" = "concept_name"
  ) %>%
  compute()

drug_db <- cdm$concept %>%
  dplyr::select(
    "drug_concept_id" = "concept_id",
    "drug_name" = "concept_name"
  ) %>%
  compute()

x <- cdm$drug_strength %>%
  dplyr::left_join(drug_db, by = "drug_concept_id") %>%
  dplyr::left_join(ingredient_db, by = "ingredient_concept_id") %>%
  dplyr::mutate(amount = if_else(is.na(amount_value), NA, "numeric")) %>%
  dplyr::left_join(amount_unit_db, by = "amount_unit_concept_id") %>%
  dplyr::mutate(numerator = if_else(is.na(numerator_value), NA, "numeric")) %>%
  dplyr::left_join(numerator_unit_db, by = "numerator_unit_concept_id") %>%
  dplyr::mutate(denominator = if_else(is.na(denominator_value), NA, "numeric")) %>%
  dplyr::left_join(denominator_unit_db, by = "denominator_unit_concept_id") %>% dplyr::select(
    drug_concept_id, ingredient_concept_id, amount, amount_unit, amount_unit_concept_id,
    numerator, numerator_unit, numerator_unit_concept_id, denominator, denominator_unit,
    denominator_unit_concept_id
  ) %>%
  compute()

patternfile <- x %>%
  group_by( amount, amount_unit, amount_unit_concept_id,
            numerator, numerator_unit, numerator_unit_concept_id,
            denominator, denominator_unit,
            denominator_unit_concept_id) %>%
  window_order(drug_concept_id) %>%
  mutate(row_num = row_number()) %>%
  filter(row_num == 1) %>%
  dplyr::select(
    amount, amount_unit, amount_unit_concept_id, numerator,  numerator_unit, numerator_unit_concept_id,
    denominator, denominator_unit, denominator_unit_concept_id
  ) %>%
  ungroup() %>%
  window_order() %>%
  collect()

#### logic for valid versus non valid pattern
patternfile <- patternfile %>%
  dplyr::mutate(
    valid =
      dplyr::if_else ((!is.na(amount) & grepl("gram|international unit|liter|milliequivalent", amount_unit) & is.na(denominator_unit) & is.na(numerator_unit)) |
                        (!is.na(numerator) & grepl("gram|international unit|liter|milliequivalent", numerator_unit) &
                           grepl("gram|hour|liter|Actuation|square centimeter", denominator_unit)),TRUE,FALSE))

validpattern <- subset(patternfile, valid==TRUE) %>% arrange(amount, denominator)
