#' @export
runSubgroupOutcomeValidation <- function(baseUrl,
                                         connectionDetails,
                                         cdmDatabaseSchema,
                                         cohortDatabaseSchema,
                                         cohortTable,
                                         outputFolder,
                                         databaseId) {

  pvSubroupResultsFile <- file.path(outputFolder, databaseId, "pheValuator", "stratifiedPvResults.csv")
  if (file.exists(pvSubroupResultsFile)) {
    return(print("PV results by sex and age already exist."))
  }

  ref <- readRDS(file.path(outputFolder, databaseId, "pheValuator", "reference.rds"))
  ref <- ref[ref$description != "[IrCorrection] Encephalomyelitis", ]
  ref <- ref[ref$description != "[IrCorrection] Hemorrhagic stroke", ]

  outcomeCohorts <- readr::read_csv("inst/settings/CohortsToCreate.csv", show_col_types = FALSE)
  names(outcomeCohorts) <- SqlRender::snakeCaseToCamelCase(names(outcomeCohorts))
  drops <- c("[IrCorrection] Encephalomyelitis",
             "[IrCorrection] Hemorrhagic stroke",
             "[IrCorrection] Bells Palsy",
             "[IrCorrection] Disseminated intravascular coagulation",
             "[IrCorrection] Thrombosis with thrombocytopenia",
             "[IrCorrection] Persons observed on 1 Jan 2017-2019",
             "[IrCorrection] AESI xSpec",
             "[IrCorrection] AESI prevalence")
  outcomeCohorts <- outcomeCohorts[!(outcomeCohorts$cohortName %in% drops), ]


  allStratifiedPvResults <- tibble::tibble()

  for (i in 1:nrow(ref)) { # i=5

    phenotypeName <- ref$description[i]
    evaluationCohortFolder <- ref$evaluationCohortFolder[ref$description == phenotypeName]
    evaluationCohortFolder <- file.path(outputFolder, databaseId, "pheValuator", evaluationCohortFolder)

    # pvResultFile <- ref$resultsFile[ref$description == phenotypeName]
    # pvResult <- readRDS(file.path(file.path(outputFolder, databaseId, "pheValuator", pvResultFile)))

    ParallelLogger::logInfo("Evaluating ", phenotypeName)
    phenotypeCohortId <- outcomeCohorts$cohortId[outcomeCohorts$cohortName == phenotypeName]


    resultsBySex <- tibble::tibble()
    for (subgroupValue in c(8532, 8507)) {

      ParallelLogger::logInfo("... among subgroup: ", subgroupValue)

      pvResultbySex <- testPhenotypeAlgorithmSubgroup(
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
        subgroupValue = subgroupValue
      )
      resultsBySex <- dplyr::bind_rows(
        resultsBySex,
        pvResultbySex
      )
    }

    resultsByAge <- tibble::tibble()
    for (subgroupValue in c("<5", "5 - 17", "18 - 34", "35 - 54", "55 - 64", "65 - 74", "75 - 84", ">=85")) {

      ParallelLogger::logInfo("... among subgroup: ", subgroupValue)

      pvResultbyAge <- testPhenotypeAlgorithmSubgroup(
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
        subgroupValue = subgroupValue
      )
      resultsByAge <- dplyr::bind_rows(
        resultsByAge,
        pvResultbyAge)
    }

    stratifiedPvResults <- dplyr::bind_rows(
      resultsBySex,
      resultsByAge
    )

    allStratifiedPvResults <- dplyr::bind_rows(
      allStratifiedPvResults,
      stratifiedPvResults
    )
  }

  allStratifiedPvResultsSave <- allStratifiedPvResults %>%
    dplyr::rename(
      cdm = databaseId,
      description = phenotype
      #stratum = analysisName
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
