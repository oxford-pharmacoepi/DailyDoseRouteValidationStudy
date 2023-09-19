# Create cdm object

cdm <- cdmFromCon(
  con = db,
  cdmSchema = c(schema = cdmSchema),
  writeSchema = c(schema = writeSchema, prefix = writePrefix),
  cdmName = dbName
)

# check that you can create temp tables
cdm$person %>%
  head(1) %>%
  computeQuery() %>%
  invisible()


# Create zip file
zipName <- paste0(db.name,"_Results")
tempDir <- zipName
tempDirCreated <- FALSE
if (!dir.exists(tempDir)) {
  dir.create(tempDir)
  tempDirCreated <- TRUE
}

start <- Sys.time()

# Start log
log_file <- paste0(tempDir, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# Create pattern
info(logger, 'CREATING VALID PATTERN FILE')
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


# Assess coverage
info(logger, 'ASSESSING COVERAGE')
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

write.csv(results_strength, here::here(tempDir,"results_strength.csv"))


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

write.csv(results_exposure, here::here(tempDir,"results_exposure.csv"))


# Zip file
zip::zip(zipfile = paste0(zipName, ".zip"),
         files = list.files(tempDir, full.names = TRUE))
if (tempDirCreated) {
  unlink(tempDir, recursive = TRUE)
}
info(logger, 'SAVED RESULTS IN THE OUTPUT FOLDER IN GITHUB')

print("Done!")
print("If all has worked, there should now be a zip file with your results
      in the output folder to share")
print("Thank you for running the study!")
Sys.time() - start
readLines(log_file)
