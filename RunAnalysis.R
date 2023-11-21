source(here("functions.R"))

# Start log ----
resultsFolder <- here("Results")
log_file <- paste0(resultsFolder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# Create cdm object ----
info(logger, 'CREATE CDM OBJECT')
cdm <- cdmFromCon(
  con = db,
  cdmSchema = c(schema = cdmSchema),
  writeSchema = c(schema = writeSchema, prefix = writePrefix),
  cdmName = dbName
)

# check that you can create temp tables
info(logger, 'CHECK TEMP TABLE PERMISSION')
cdm$person %>%
  head(1) %>%
  computeQuery() %>%
  invisible()

# cdm snapshot ----
info(logger, 'CREATE SNAPSHOT')
write.csv(
  x = snapshot(cdm),
  file = here("Results", paste0("snapshot_", cdmName(cdm), ".csv")),
  row.names = FALSE
)

# Create pattern ----
info(logger, 'CREATING PATTERN FILE')
patternSummary <- patternTable(cdm) %>%
  addCdmName(cdm = cdm)
write.csv(
  x = patternSummary,
  file = here("Results", paste0("pattern_", cdmName(cdm), ".csv")),
  row.names = FALSE
)
info(logger, 'PATTERN FILE CREATED')

# Pattern checks
## we really only need the numerator check from IPCI
info(logger, 'DOING NUMERATOR CHECK')
source(here::here("numerator_check.R"))
info(logger, 'CHECKS DONE')

# route ----
info(logger, 'SUMMARY ROUTE')
routeSummary <- cdm$drug_exposure %>%
  addRoute() %>%
  group_by(.data$route) %>%
  summarise(
    number_concepts = n_distinct(.data$drug_concept_id),
    number_records = n()
  ) %>%
  collect() %>%
  addCdmName(cdm = cdm)
write.csv(
  x = routeSummary,
  file = here("Results", paste0("route_", cdmName(cdm), ".csv")),
  row.names = FALSE
)
info(logger, 'ROUTE SUMMARISED')

# coverage ----
info(logger, 'DOSE COVERAGE')
ingredients <- c(956874, 1106776, 1137529, 1301025, 1503297,1154029)
doseCoverage <- dailyDosePatternCoverage(cdm, ingredients)
write.csv(
  x = doseCoverage,
  file = here("Results", paste0("dose_", cdmName(cdm), ".csv")),
  row.names = FALSE
)
info(logger, 'DOSE COVERAGE END')

# zip results ----
zip(
  zipfile = here(resultsFolder, paste0("Results_", cdmName(cdm), ".zip")),
  files = list.files(resultsFolder),
  root = resultsFolder
)
