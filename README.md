# Study to develop the approach to compute daily dose in the OMOP CDM

In this project, we document our approach towards daily dose calculations, which comprises the establishment of drug strength pattern, clinical assessment, and comparison of calculated daily dose with WHO DDD.

## Interested in the finished product and want to calculate dose for your own cohorts?
You are in luck. The dose calculations were implemented in the R package DrugUtilisation. Just follow their vignettes on how to calculate dose here: 
https://darwin-eu-dev.github.io/DrugUtilisation/



## If you want to re-run our development code
Complete the connection details in `CodeToRun.R` and run it. Results will be zipped in the Results folder. 

## Interested in what the development code does?
The 'CodeToRun.R' sources the 'RunAnalysis.R' script that was used to obtain results as can be seen in the manuscript "Calculating daily dose in the Observational Medical Outcomes Partnership (OMOP) Common Data Model".

In this script we create the drug strength pattern from which we also obtained the proportion of drug concepts for which we can estimate dose with our formulas (line 34), we add the route to the drug exposures (line 46), and we calculate the dose (line 63). 

## other scripts in this development repository
We carried out three checks in the vocabulary. More information can be found in this OHDSI Europe symposium abstract (2023). https://www.ohdsi.org/wp-content/uploads/2023/10/Burkard_Theresa_DosePreparation_2023symposium-Theresa-Burkard-1.pdf

The code in numerator_check.R contains the first check described:
"First, we assessed whether the following formula (1) held for all the drug concept ids. 
(1) Numerator_value = “drug concentration (as provided in the drug concept name)” * denominator_value"

The code in new_check.R contains the second check described:
"Second, we assessed whether there were drug concentrations (as provided in the drug concept name)
that may not be feasible, e.g. >1 mg/mg or >1 ml/ml (and as a sensitivity analysis: >2 mg/mg or >2 
ml/ml)"

The code in denominator_check.R contains the third check described:
"For all combinations of numerator and denominator units (i.e. “concentration patterns”) we assessed 
the number of used unique drug concept ids (in combination with the quantity field) in the drug 
exposure table. This check was carried out to assess the difference between patterns with 
denominator value present versus those with missing denominator values. This would give us more 
insight into which patterns to prioritize"
