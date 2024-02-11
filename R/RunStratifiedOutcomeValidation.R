#' @export
runStratificedOutcomeValidation <- function(baseUrl,
                                            connectionDetails,
                                            cdmDatabaseSchema,
                                            cohortDatabaseSchema,
                                            cohortTable,
                                            outputFolder,
                                            databaseId) {

  # create PV and outcome refs =================================================

  pvSubroupResultsFile <- file.path(outputFolder, databaseId, "pheValuator", "stratifiedPvResults.csv")
  if (file.exists(pvSubroupResultsFile)) {
    stop(print("PV results by sex and age already exist."))
  }

  keeps <- c(
    "[IrCorrection] Acute myocardial infarction",
    "[IrCorrection] Appendicitis",
    "[IrCorrection] Deep vein thrombosis",
    "[IrCorrection] Non-hemorrhagic stroke",
    "[IrCorrection] Pulmonary embolism"
  )

  sexStrata <- c("Male","Female")
  ageStrata <- c("<5", "5 - 17", "18 - 34", "35 - 54", "55 - 64", "65 - 74", "75 - 84", ">=85")
  ageSexStrata <- c(
    "Male <5",
    "Male 5 - 17",
    "Male 18 - 34",
    "Male 35 - 54",
    "Male 55 - 64",
    "Male 65 - 74",
    "Male 75 - 84",
    "Male >=85",
    "Female <5",
    "Female 5 - 17",
    "Female 18 - 34",
    "Female 35 - 54",
    "Female 55 - 64",
    "Female 65 - 74",
    "Female 75 - 84",
    "Female >=85"
  )

  ref <- readRDS(file.path(outputFolder, databaseId, "pheValuator", "reference.rds")) %>%
    dplyr::filter(description %in% keeps)

  outcomeCohorts <- readr::read_csv("inst/settings/CohortsToCreate.csv", show_col_types = FALSE) %>%
    dplyr::filter(cohort_name %in% keeps)
  names(outcomeCohorts) <- SqlRender::snakeCaseToCamelCase(names(outcomeCohorts))

  allStratifiedPvResults <- tibble::tibble()

  # PV ref loop ================================================================

  for (i in 1:nrow(ref)) { # i=1

    phenotypeName <- ref$description[i]
    evaluationCohortFolder <- ref$evaluationCohortFolder[ref$description == phenotypeName]
    evaluationCohortFolder <- file.path(outputFolder, databaseId, "pheValuator", evaluationCohortFolder)

    #test for pheval results present
    #pvResultFile <- ref$resultsFile[ref$description == phenotypeName]
    #pvResult <- readRDS(file.path(file.path(outputFolder, databaseId, "pheValuator", pvResultFile)))

    ParallelLogger::logInfo("Evaluating ", phenotypeName)
    phenotypeCohortId <- outcomeCohorts$cohortId[outcomeCohorts$cohortName == phenotypeName]

    ## sex strata loop =========================================================

    resultsBySex <- tibble::tibble()

    for (strataValue in sexStrata) {  # strataValue="Female"

      ParallelLogger::logInfo("... among subgroup: ", strataValue)

      pvResultbySex <- testPhenotypeStrata(
        phenotype = phenotypeName,
        runDateTime = Sys.time(),
        connectionDetails = connectionDetails,
        cutPoints = c("EV"),
        outFolder = evaluationCohortFolder,
        evaluationCohortId = "main",
        phenotypeCohortId = phenotypeCohortId,
        cdmDatabaseSchema = cdmDatabaseSchema,
        databaseId = databaseId,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        washoutPeriod = 0,
        splayPrior = 7,
        splayPost = 7,
        strataValue = strataValue
      )
      resultsBySex <- dplyr::bind_rows(
        resultsBySex,
        pvResultbySex
      )
    }

    # age strata loop ==========================================================

    resultsByAge <- tibble::tibble()

    for (strataValue in ageStrata) {

      ParallelLogger::logInfo("... among subgroup: ", strataValue)

      pvResultbyAge <- testPhenotypeStrata(
        phenotype = phenotypeName,
        runDateTime = as.POSIXct(format(Sys.time()), tz = "GMT"),
        connectionDetails = connectionDetails,
        cutPoints = c("EV"),
        outFolder = evaluationCohortFolder,
        evaluationCohortId = "main",
        phenotypeCohortId = phenotypeCohortId,
        cdmDatabaseSchema = cdmDatabaseSchema,
        databaseId = databaseId,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        washoutPeriod = 0,
        splayPrior = 7,
        splayPost = 7,
        strataValue = strataValue
      )
      resultsByAge <- dplyr::bind_rows(
        resultsByAge,
        pvResultbyAge)
    }

    ## sex * age loop ==========================================================

    resultsBySexAge <- tibble::tibble()

    for (strataValue in ageSexStrata) {

      ParallelLogger::logInfo("... among subgroup: ", strataValue)

      pvResultbySexAge <- testPhenotypeStrata(
        phenotype = phenotypeName,
        runDateTime = as.POSIXct(format(Sys.time()), tz = "GMT"),
        connectionDetails = connectionDetails,
        cutPoints = c("EV"),
        outFolder = evaluationCohortFolder,
        evaluationCohortId = "main",
        phenotypeCohortId = phenotypeCohortId,
        cdmDatabaseSchema = cdmDatabaseSchema,
        databaseId = databaseId,
        cohortDatabaseSchema = cohortDatabaseSchema,
        cohortTable = cohortTable,
        washoutPeriod = 0,
        splayPrior = 7,
        splayPost = 7,
        strataValue = strataValue
      )
      resultsBySexAge <- dplyr::bind_rows(
        resultsBySexAge,
        pvResultbySexAge)
    }

    # compile stratified PV results ============================================

    stratifiedPvResults <- dplyr::bind_rows(
      resultsBySex,
      resultsByAge,
      resultsBySexAge
    )

    allStratifiedPvResults <- dplyr::bind_rows(
      allStratifiedPvResults,
      stratifiedPvResults
    )
  }

  allStratifiedPvResultsSave <- allStratifiedPvResults %>%
    dplyr::rename(
      cdm = databaseId,
      description = phenotype,
      stratum = analysisName
    ) %>%
    # dplyr::select(
    #   -c(totalSubjects,
    #      rawCases,
    #      estimatedCases,
    #      rawTar,
    #      estimatedTar,
    #      rawPrevalence,
    #      rawPrevalenceRate,
    #      estimatedPrevalenceRate)
    # ) %>%
    dplyr::mutate_at(
      c("f1Score",
        "estimatedPrevalence",
        "sensitivity",
        "sensitivityCi95Lb",
        "sensitivityCi95Ub",
        "ppv",
        "ppvCi95Lb",
        "ppvCi95Ub",
        "specificity",
        "specificityCi95Lb",
        "specificityCi95Ub",
        "npv",
        "npvCi95Lb",
        "npvCi95Ub"),
      as.numeric
    ) %>%
    dplyr::mutate_at(
      "runDateTimeGMT",
      as.character
    )

  readr::write_csv(
    x = allStratifiedPvResultsSave,
    file = file.path(outputFolder, databaseId, "pheValuator", "stratifiedPvResults.csv")
  )
}
