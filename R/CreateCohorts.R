#' @export
createCohorts <- function(baseUrl,
                          cohortIds,
                          connectionDetails,
                          cdmDatabaseSchema,
                          cohortDatabaseSchema,
                          cohortTable,
                          outputFolder,
                          databaseId) {

  cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
    baseUrl = baseUrl,
    cohortIds = cohortIds,
    generateStats = FALSE
  )

  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable)

  cohortsGenerated <- CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = cohortDefinitionSet,
    incremental = TRUE,
    incrementalFolder = file.path(outputFolder, databaseId, "incremental")
  )

  cohortCounts <- CohortGenerator::getCohortCounts(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTableNames$cohortTable
  )

  readr::write_csv(cohortCounts, file.path(outputFolder, databaseId, "cohort_counts.csv"))

}
