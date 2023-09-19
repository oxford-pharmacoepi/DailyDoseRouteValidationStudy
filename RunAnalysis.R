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

info(logger, 'CHECK TEMP TABLE PERMISSION')
# check that you can create temp tables
cdm$person %>%
  head(1) %>%
  computeQuery() %>%
  invisible()

# cdm snapshot ----
write.csv(
  x = snapshot(cdm),
  file = here("Results", paste0("snapshot_", cdmName(cdm), ".csv")),
  row.names = FALSE
)

# Create pattern ----
info(logger, 'CREATING VALID PATTERN FILE')
patternSummary <- patternTable(cdm, TRUE) %>%
  mutate(cdm_name = cdmName(cdm))
write.csv(
  x = patternSummary,
  file = here("Results", paste0("pattern_", cdmName(cdm), ".csv")),
  row.names = FALSE
)
info(logger, 'PATTERN FILE CREATED')

# TO ADD route ----

# TO ADD coverage ----

# zip results ----
zip(
  zipfile = here(resultsFolder, "Results.zip"),
  files = list.files(resultsFolder),
  root = resultsFolder
)
