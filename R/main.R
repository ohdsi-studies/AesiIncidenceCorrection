#' @export
execute <- function(connectionDetails,
                    baseUrl,
                    outputFolder,
                    cohortRef,
                    databaseId,
                    cohortIds = NULL,
                    targetCohortIds = NULL,
                    outcomeCohortIds = NULL,
                    validationCohortIds = NULL,
                    cdmDatabaseSchema,
                    cohortDatabaseSchema,
                    cohortTable = "ir_correction_cohort",
                    createCohortTable = FALSE,
                    createCohorts = FALSE,
                    createValidationCohorts = FALSE,
                    runOutcomeValidation = FALSE,
                    runIncidenceAnalysis = FALSE,
                    runIncidenceCorrection = FALSE,
                    evaluateIrCorrection = FALSE,
                    runMetaAnalysis = FALSE,
                    exportResults = FALSE) {

  start <- Sys.time()

  if (createCohortTable) {

    CohortGenerator::createCohortTables(
      connectionDetails = connectionDetails,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable),
      incremental = TRUE
    )
  }

  if (createCohorts) {

    AesiIncidenceCorrection::createCohorts(
      baseUrl = baseUrl,
      cohortIds = cohortIds,
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      outputFolder = outputFolder,
      databaseId = databaseId
    )

  }

  if (runOutcomeValidation) {
    AesiIncidenceCorrection::runOutcomeValidation(
      baseUrl = baseUrl,
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      outputFolder = outputFolder,
      databaseId = databaseId,
      cohortRef = cohortRef
    )
  }

  if (runIncidenceAnalysis) {
    AesiIncidenceCorrection::runIncidenceAnalysis(
      baseUrl = baseUrl,
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmDatabaseSchema,
      cohortDatabaseSchema = cohortDatabaseSchema,
      cohortTable = cohortTable,
      databaseId = databaseId,
      targetCohortIds = targetCohortIds,
      outcomeCohortIds = outcomeCohortIds,
      outputFolder = outputFolder
    )
  }

  if (runIncidenceCorrection) {
    AesiIncidenceCorrection::runIncidenceCorrection(
      outputFolder = outputFolder,
      cohortRef = cohortRef
    )
  }

  if (evaluateIrCorrection) {
    AesiIncidenceCorrection::evaluateIrCorrection(outputFolder = outputFolder)
  }

  if (exportResults) {
    print("TODO")
  }

  delta <- Sys.time() - start
  sprintf("Executing study took %f %s", signif(delta, 3), attr(delta, "units"))

}
