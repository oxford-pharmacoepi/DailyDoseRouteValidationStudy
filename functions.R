dailyDosePatternCoverage <- function(cdm,
                                     ingredientConceptId) {
  # initial checks
  DrugUtilisation:::checkInputs(cdm = cdm)

  # get daily dosage
  dailyDose <- cdm[["drug_exposure"]] %>%
    dplyr::inner_join(
      cdm[["concept_ancestor"]] %>%
        dplyr::filter(ancestor_concept_id %in% .env$ingredientConceptId) %>%
        dplyr::select("drug_concept_id" = "descendant_concept_id"),
      by = "drug_concept_id"
    ) %>%
    dplyr::select(
      "drug_concept_id", "drug_exposure_start_date", "drug_exposure_end_date",
      "quantity"
    ) %>%
    dplyr::mutate(days_exposed = !!CDMConnector::datediff(
      start = "drug_exposure_start_date",
      end = "drug_exposure_end_date"
    ) + 1) %>%
    CDMConnector::computeQuery() %>%
    dplyr::left_join(
      DrugUtilisation:::drugStrengthPattern(
        cdm = cdm, ingredientConceptId = ingredientConceptId
      ) %>%
        DrugUtilisation::addRoute(),
      by = "drug_concept_id"
    ) %>%
    CDMConnector::computeQuery() %>%
    dplyr::mutate(formula_name = dplyr::if_else(
      .data$days_exposed <= 0,
      as.character(NA),
      .data$formula_name
    )) %>%
    DrugUtilisation:::standardUnits() %>%
    DrugUtilisation:::applyFormula() %>%
    dplyr::select(
      "drug_concept_id", "daily_dose", "unit", "pattern_id", "route",
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
        PatientProfiles::summariseResult(
          group = list("ingredient_name"),
          includeOverallGroup = FALSE,
          strata = list(
            "unit", c("unit", "route"), c("unit", "route", "pattern_id")
          ),
          includeOverallStrata = TRUE,
          variables = "daily_dose",
          functions = c(
            "missing", "mean", "sd", "min", "q05", "q25", "median", "q75", "q95",
            "max"
          )
        )

  return(dailyDoseSummary)
}
