#' @export
runOutcomeValidation <- function(baseUrl,
                                 connectionDetails,
                                 cdmDatabaseSchema,
                                 cohortDatabaseSchema,
                                 cohortTable,
                                 outputFolder,
                                 databaseId,
                                 cohortRef) {


  pvFolder <- file.path(outputFolder, databaseId, "pheValuator")
  if (!file.exists(pvFolder)) {
    dir.create(pvFolder, recursive = TRUE)
  }

  chronicOutcomeCohortIds <- c(
    12083, # narcolepsy
    12087, # GBS
    12093 #transverse myelitis
  )

  pheValuatorAnalysisList <- list()

  for (i in seq_len(nrow(cohortRef))) { # i = 5

    outcomeCohortId <- cohortRef$cohortId[i]
    xSpecId <- cohortRef$xSpecId[i]
    prevId <- cohortRef$prevId[i]
    excludedCovariateConceptIds <- as.numeric(strsplit(cohortRef$exclusionConceptIds[i], split = ";")[[1]])

    if (outcomeCohortId %in% chronicOutcomeCohortIds) { # chronic condition

      daysFromxSpec = 14
      covariateSettings <- PheValuator::createDefaultCovariateSettings(
        excludedCovariateConceptIds = excludedCovariateConceptIds,
        addDescendantsToExclude = TRUE,
        startDayWindow1 = 0,
        endDayWindow1 = 30,
        startDayWindow2 = 31,
        endDayWindow2 = 60,
        startDayWindow3 = 61,
        endDayWindow3 = 365
      )

    } else { # acute condition

      daysFromxSpec = 1
      covariateSettings <- PheValuator::createDefaultCovariateSettings(
        excludedCovariateConceptIds = excludedCovariateConceptIds,
        addDescendantsToExclude = TRUE,
        startDayWindow1 = 0,
        endDayWindow1 = 10,
        startDayWindow2 = 11,
        endDayWindow2 = 20,
        startDayWindow3 = 21,
        endDayWindow3 = 30
      )
    }

    #  set DemographicsGender = FALSE, DemographicsAgeGroup = FALSE to remove from Dx model predictors
    covariateSettings[[1]]$DemographicsGender <- FALSE
    covariateSettings[[1]]$DemographicsAgeGroup <- FALSE

    CohortArgs <- PheValuator::createCreateEvaluationCohortArgs(
      xSpecCohortId = xSpecId,
      daysFromxSpec = daysFromxSpec,
      xSensCohortId = prevId,
      prevalenceCohortId = prevId,
      modelBaseSampleSize = 25000,
      xSpecCohortSize = 5000,
      covariateSettings = covariateSettings,
      baseSampleSize = 6000000,
      lowerAgeLimit = 0,
      upperAgeLimit = 100,
      startDate = "20170101",
      endDate = "20191231",
      excludeModelFromEvaluation = FALSE
    )

    testArgs <- PheValuator::createTestPhenotypeAlgorithmArgs(
      cutPoints =  c("EV"),
      phenotypeCohortId = outcomeCohortId,
      washoutPeriod = 0
    )

    analysis <- PheValuator::createPheValuatorAnalysis(
      analysisId = i,
      description = ROhdsiWebApi::getCohortDefinition(outcomeCohortId, baseUrl)$name,
      createEvaluationCohortArgs = CohortArgs,
      testPhenotypeAlgorithmArgs = testArgs
    )

    pheValuatorAnalysisList[[length(pheValuatorAnalysisList) + 1]] <- analysis
  }

  referenceTable <- PheValuator::runPheValuatorAnalyses(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    workDatabaseSchema = cohortDatabaseSchema,
    outputFolder = pvFolder,
    pheValuatorAnalysisList = pheValuatorAnalysisList
  )

  results <- PheValuator::summarizePheValuatorAnalyses(
    referenceTable = referenceTable,
    outputFolder = pvFolder
  )
  readr::write_csv(results, file.path(pvFolder, "pvResults.csv"))
}



# if want to evaluate xSpec and Prev cohorts
#
# testArgs1 <- PheValuator::createTestPhenotypeAlgorithmArgs(
#   cutPoints = c("EV"),
#   phenotypeCohortId = xSpecId,
#   washoutPeriod = 0
# )
#
# analysis1 <- PheValuator::createPheValuatorAnalysis(
#   analysisId = 1,
#   description = ROhdsiWebApi::getCohortDefinition(xSpecId, baseUrl)$name,
#   createEvaluationCohortArgs = CohortArgs,
#   testPhenotypeAlgorithmArgs = testArgs1
# )
#
# testArgs2 <- PheValuator::createTestPhenotypeAlgorithmArgs(
#   cutPoints =  c("EV"),
#   phenotypeCohortId = prevId,
#   washoutPeriod = 0
# )
#
# analysis2 <- PheValuator::createPheValuatorAnalysis(
#   analysisId = 2,
#   description = ROhdsiWebApi::getCohortDefinition(prevId, baseUrl)$name,
#   createEvaluationCohortArgs = CohortArgs,
#   testPhenotypeAlgorithmArgs = testArgs2
# )
#
# pheValuatorAnalysisList[[length(pheValuatorAnalysisList) + 1]] <- analysis1
# pheValuatorAnalysisList[[length(pheValuatorAnalysisList) + 1]] <- analysis2

