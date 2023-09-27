dailyDosePatternCoverage <- function(cdm,
                                     ingredientConceptId) {
  # initial checks
  checkInputs(cdm = cdm)

  # get concepts
  concepts <- cdm[["concept_ancestor"]] %>%
    dplyr::filter(ancestor_concept_id %in% .env$ingredientConceptId) %>%
    dplyr::pull("descendant_concept_id")

  # get daily dosage
  dailyDose <- cdm[["drug_exposure"]] %>%
    dplyr::filter(.data$drug_concept_id %in% .env$concepts) %>%
    dplyr::select(
      "drug_concept_id", "drug_exposure_start_date", "drug_exposure_end_date",
      "quantity"
    ) %>%
    dplyr::mutate(days_exposed = !!CDMConnector::datediff(
      start = "drug_exposure_start_date",
      end = "drug_exposure_end_date"
    ) + 1) %>%
    dplyr::left_join(
      DrugUtilisation:::drugStrengthPattern(cdm = cdm, ingredientConceptId = ingredientConceptId),
      by = "drug_concept_id"
    ) %>%
    DrugUtilisation:::standardUnits() %>%
    DrugUtilisation:::applyFormula() %>%
    dplyr::select(
      "drug_concept_id", "daily_dose", "unit", "route", "pattern_id",
      "concept_id" =  "ingredient_concept_id"
    ) %>%
    dplyr::left_join(
      cdm[["concept"]] %>%
        dplyr::rename("ingredient_name" = "concept_name") %>%
        dplyr::select("concept_id", "ingredient_name"),
      by = "concept_id"
    ) %>%
    dplyr::collect()

  # summarise
  dailyDoseSummary <- dailyDose %>%
    suppressWarnings(PatientProfiles::summariseResult(
      group = list("ingredient_name"),
      includeOverallGroup = FALSE,
      strata = list(
        "pattern_id", "route", c("route", "pattern_id"),
        c("unit", "pattern_id"), c("route", "unit", "pattern_id")
      ),
      includeOverallStrata = TRUE,
      variables = "daily_dose",
      functions = c(
        "missing", "mean", "sd", "min", "q05", "q25", "median", "q75", "q95",
        "max"
      )
    )) %>%
    dplyr::filter(
      !(.data$strata_name %in% c("Overall", "route")) |
        .data$variable != "daily_dose" |
        .data$estimate_type %in% c("count", "percentage")
    )

  return(dailyDoseSummary)
}
