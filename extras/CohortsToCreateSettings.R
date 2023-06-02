library(magrittr)
baseUrl <- keyring::key_get("BASE_URL")
ROhdsiWebApi::authorizeWebApi(baseUrl, "windows")

targetCohortIds <- 12100
outcomeCohortIds <- 12083:12098
validationCohortIds <- c(12159, 12160)
cohortIds <- c(targetCohortIds, outcomeCohortIds, validationCohortIds)

cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = cohortIds,
  generateStats = FALSE
)

CohortGenerator::saveCohortDefinitionSet(
  cohortDefinitionSet = cohortDefinitionSet,
  settingsFileName = file.path("inst/settings/CohortsToCreate.csv"),
  jsonFolder = file.path("inst/cohorts"),
  sqlFolder = file.path("inst/sql/sql_server")
)
