#' @export
testPhenotypeAlgorithmSubgroup <- function(phenotype,
                                           analysisName,
                                           runDateTime,
                                           connectionDetails,
                                           cutPoints = c("EV"),
                                           outFolder,
                                           evaluationCohortId = "main",
                                           phenotypeCohortId,
                                           cdmDatabaseSchema,
                                           databaseId,
                                           cohortDatabaseSchema,
                                           cohortTable,
                                           washoutPeriod = 0,
                                           splayPrior = 7,
                                           splayPost = 7,
                                           subgroupValue) {

  if (length(connectionDetails) == 0) {
    stop("Must supply a connection string")
  }
  if (cohortDatabaseSchema == "") {
    stop("Must have a defined Cohort schema (e.g., \"YourCDM.YourSchema\")")
  }
  if (cohortTable == "") {
    stop("Must have a defined Cohort table (e.g., \"cohort\")")
  }
  if (phenotypeCohortId == "") {
    stop("Must have a defined Phenotype Cohort ID to test (e.g., 1234)")
  }

  start <- Sys.time()

  evaluationCohortFileName <- file.path(outFolder, sprintf("evaluationCohort_%s.rds", evaluationCohortId))
  if (!file.exists(evaluationCohortFileName)) {
    stop(paste("Evaluation cohort file (", evaluationCohortFileName, ") does not exist"))
  }
  ParallelLogger::logInfo("Loading evaluation cohort from ", evaluationCohortFileName)
  evaluationCohort <- readRDS(evaluationCohortFileName)
  if (is.null(evaluationCohort$errorMessage) | is.null(evaluationCohort$prediction)) {
    return(tibble::tibble(phenotype = phenotype,
                          databaseId = databaseId,
                          cohortId = phenotypeCohortId,
                          analysisName = NULL))
  }


  ### ADDITION: create subgroups ###

  if (is.numeric(subgroupValue)) {
    evaluationCohort$prediction <- evaluationCohort$prediction %>%
      dplyr::filter(gender == subgroupValue)

    if (is.null(evaluationCohort$prediction)) {
      return(tibble::tibble(phenotype = phenotype,
                            databaseId = databaseId,
                            cohortId = phenotypeCohortId,
                            analysisName = NULL))
    }

    if (subgroupValue == 8507) {
      analysisName <- "Male"
    } else {
      analysisName <- "Female"
    }

  } else {

    if (subgroupValue == "<5") {
      evaluationCohort$prediction <- evaluationCohort$prediction %>%
        dplyr::filter(ageYear < 5)
      if (is.null(evaluationCohort$prediction)) {
        return(tibble::tibble(phenotype = phenotype,
                              databaseId = databaseId,
                              cohortId = phenotypeCohortId,
                              analysisName = NULL))
      }

      analysisName <- subgroupValue
    }

    if (subgroupValue == "5 - 17") {
      evaluationCohort$prediction <- evaluationCohort$prediction %>%
        dplyr::filter(ageYear >= 5 & ageYear <= 17)
      if (is.null(evaluationCohort$prediction)) {
        return(tibble::tibble(phenotype = phenotype,
                              databaseId = databaseId,
                              cohortId = phenotypeCohortId,
                              analysisName = NULL))
      }
      analysisName <- subgroupValue
    }

    if (subgroupValue == "18 - 34") {
      evaluationCohort$prediction <- evaluationCohort$prediction %>%
        dplyr::filter(ageYear >= 18 & ageYear <= 34)
      if (is.null(evaluationCohort$prediction)) {
        return(tibble::tibble(phenotype = phenotype,
                              databaseId = databaseId,
                              cohortId = phenotypeCohortId,
                              analysisName = NULL))
      }
      analysisName <- subgroupValue
    }

    if (subgroupValue == "35 - 54") {
      evaluationCohort$prediction <- evaluationCohort$prediction %>%
        dplyr::filter(ageYear >= 35 & ageYear <= 54)
      if (is.null(evaluationCohort$prediction)) {
        return(tibble::tibble(phenotype = phenotype,
                              databaseId = databaseId,
                              cohortId = phenotypeCohortId,
                              analysisName = NULL))
      }
      analysisName <- subgroupValue
    }

    if (subgroupValue == "55 - 64") {
      evaluationCohort$prediction <- evaluationCohort$prediction %>%
        dplyr::filter(ageYear >= 55 & ageYear <= 64)
      if (is.null(evaluationCohort$prediction)) {
        return(tibble::tibble(phenotype = phenotype,
                              databaseId = databaseId,
                              cohortId = phenotypeCohortId,
                              analysisName = NULL))
      }
      analysisName <- subgroupValue
    }

    if (subgroupValue == "65 - 74") {
      evaluationCohort$prediction <- evaluationCohort$prediction %>%
        dplyr::filter(ageYear >= 65 & ageYear <= 74)
      if (is.null(evaluationCohort$prediction)) {
        return(tibble::tibble(phenotype = phenotype,
                              databaseId = databaseId,
                              cohortId = phenotypeCohortId,
                              analysisName = NULL))
      }
      analysisName <- subgroupValue
    }
    if (subgroupValue == "75 - 84") {
      evaluationCohort$prediction <- evaluationCohort$prediction %>%
        dplyr::filter(ageYear >= 75 & ageYear <= 84)
      if (is.null(evaluationCohort$prediction)) {
        return(tibble::tibble(phenotype = phenotype,
                              databaseId = databaseId,
                              cohortId = phenotypeCohortId,
                              analysisName = NULL))
      }
      analysisName <- subgroupValue
    }
    if (subgroupValue == ">=85") {
      evaluationCohort$prediction <- evaluationCohort$prediction %>%
        dplyr::filter(ageYear >= 85)
      if (is.null(evaluationCohort$prediction)) {
        return(tibble::tibble(phenotype = phenotype,
                              databaseId = databaseId,
                              cohortId = phenotypeCohortId,
                              analysisName = NULL))
      }
      analysisName <- subgroupValue
    }
  }

  ### ADDITION: create subgroups end ###


  minCohortStartDate <- as.Date(min(evaluationCohort$prediction$cohortStartDate)) - splayPrior # get earliest date in evaluation cohort
  maxCohortStartDate <- as.Date(max(evaluationCohort$prediction$cohortStartDate)) + splayPost # get latest date in evaluation cohort

  # test that viable evaluation cohort was created
  if (!is.null(evaluationCohort$errorMessage)) {
    ParallelLogger::logInfo(evaluationCohort$errorMessage, " - Evaluation cohort not created.")
    results <- tibble::tibble(cutPoint = evaluationCohort$errorMessage)
  } else {
    countsTable <- tibble::tibble()
    misses <- tibble::tibble()
    xSpecP <- 0.5
    xSpecP2 <- -1
    xSpecP3 <- -1

    sql <- paste0("SELECT DISTINCT subject_id,
                cohort_start_date AS pheno_cohort_start_date
              FROM @cohort_database_schema.@cohort_table
              JOIN @cdm_database_schema.observation_period
                ON subject_id = person_id
                  and cohort_start_date >= observation_period_start_date
                  and cohort_start_date <= observation_period_end_date
              WHERE cohort_definition_id = @cohort_id ;")


    sql <- SqlRender::render(
      sql = sql,
      cohort_database_schema = cohortDatabaseSchema,
      cdm_database_schema = cdmDatabaseSchema,
      cohort_table = cohortTable,
      cohort_id = phenotypeCohortId
    )

    sql <- SqlRender::translate(sql = sql, targetDialect = connectionDetails$dbms)
    connection <- DatabaseConnector::connect(connectionDetails)
    ParallelLogger::logInfo("Downloading cohort to evaluate: ", phenotypeCohortId)
    phenoPop <- DatabaseConnector::querySql(connection = connection, sql, snakeCaseToCamelCase = TRUE)
    DatabaseConnector::disconnect(connection)

    if (nrow(phenoPop) == 0) {
      #warning("Phenotype cohort is empty")
      ParallelLogger::logInfo("NOTE: ", phenotypeCohortId, " has 0 counts")
      cutPoints[cutPoints == "EV"] <- "Expected Value"
      #return(tibble::tibble("Cut Point" = cutPoints, check.names = FALSE))
      return(tibble::tibble(phenotype = phenotype,
                            databaseId = databaseId,
                            cohortId = phenotypeCohortId))
    }
    ParallelLogger::logInfo("Computing evaluation statistics")

    # extract data from evaluation cohort that is not an outcome and has at least the number of days post obs start as washout period
    modelAll <- evaluationCohort$prediction[evaluationCohort$prediction$outcomeCount == 0 &
                                              evaluationCohort$prediction$daysFromObsStart >= washoutPeriod, ]

    modelAll <- modelAll[order(modelAll$value),]

    if(nrow(modelAll) == 0) {
      return(tibble::tibble(phenotype = phenotype,
                            databaseId = databaseId,
                            cohortId = phenotypeCohortId))
    }

    modelAll$rownum <- 1:nrow(modelAll)
    phenoPop$inPhenotype <- rep(TRUE, nrow(phenoPop))

    for (cpUp in 1:length(cutPoints)) {
      # join the phenotype table with the prediction table
      # join phenotype and evaluation cohort on subject_id and phenotype visit date +/- the splay
      # first join by subject id only
      fullTable <- dplyr::left_join(modelAll,
                                    phenoPop[, c("subjectId", "phenoCohortStartDate", "inPhenotype")],
                                    by = c("subjectId")
      )

      fullTable$cohortStartDate <- as.Date(fullTable$cohortStartDate)
      fullTable$phenophenoCohortStartDate <- as.Date(fullTable$phenoCohortStartDate)

      # now set match (inPhenotype) to false if the cohort and visit date do not match within splay setting
      fullTable$inPhenotype[!is.na(fullTable$phenoCohortStartDate) &
                              (fullTable$cohortStartDate <= fullTable$phenoCohortStartDate - splayPost |
                                 fullTable$cohortStartDate >= fullTable$phenoCohortStartDate + splayPrior)] <- FALSE

      # remove the subjects in the phenotype algorithm that matched the eval cohort on subject id but didn't match the dates
      # these are mis-matches due to the random selection of visit process and should not be counted
      fullTable <- fullTable[fullTable$inPhenotype == TRUE | is.na(fullTable$inPhenotype), ]


      # } else {
      #   fullTable <- dplyr::left_join(modelAll,
      #                                 phenoPop[, c("subjectId", "inPhenotype")],
      #                                 by = c("subjectId"))
      # }

      # set all the rest of the non-matches to false
      fullTable$inPhenotype[is.na(fullTable$inPhenotype)] <- FALSE

      ######
      # write.csv(fullTable, paste0("p:/shared/",  phenotypeCohortId, splayPrior, ".csv"))
      ######

      # a cut point = 'EV' indicates to calculate the expected values - using the probability to proportion
      # trues and falses
      fullTable$valueOrig <- fullTable$value
      if (cutPoints[cpUp] != "EV") {
        # for numeric cutpoints determine the cut point to use
        cutPt <- as.numeric(cutPoints[cpUp])
        fullTable$value <- fullTable$value > cutPt
      }

      fullTable$tp <- 0
      fullTable$tn <- 0
      fullTable$fp <- 0
      fullTable$fn <- 0
      fullTable$tp[fullTable$inPhenotype] <- fullTable$value[fullTable$inPhenotype]
      fullTable$tn[!fullTable$inPhenotype] <- 1 - fullTable$value[!fullTable$inPhenotype]
      fullTable$fp[fullTable$inPhenotype] <- 1 - fullTable$value[fullTable$inPhenotype]
      fullTable$fn[!fullTable$inPhenotype] <- fullTable$value[!fullTable$inPhenotype]

      fullTable <- fullTable[!is.na(fullTable$subjectId),] #remove null rows

      ######
      # write.csv(fullTable, paste0("p:/shared/",  phenotypeCohortId, splayPrior, ".csv"))
      ######

      # capture subject id's of mistakes if requested - only for the 0.5 or xSpecP cutpoint
      if (!is.null(xSpecP)) {
        missesCP <- xSpecP
      } else {
        missesCP <- 0.5
      }
      if (cutPoints[cpUp] == missesCP) {
        subjects <- fullTable[fullTable$tp == 1, ]
        if (nrow(subjects) > 0) {
          subjects <- subjects[order(-subjects$valueOrig), ]
          tempMisses <- subjects[min(5, nrow(subjects)), c("subjectId", "cohortStartDate", "daysFromObsStart", "valueOrig")]
          tempMisses$miss <- "TP"
          misses <- dplyr::bind_rows(misses, tempMisses)
        }
        subjects <- fullTable[fullTable$fp == 1, ]
        if (nrow(subjects) > 0) {
          subjects <- subjects[order(-subjects$valueOrig), ]
          tempMisses <- subjects[min(50, nrow(subjects)), c("subjectId", "cohortStartDate", "daysFromObsStart", "valueOrig")]
          tempMisses$miss <- "FP"
          misses <- dplyr::bind_rows(misses, tempMisses)
        }
        subjects <- fullTable[fullTable$fn == 1, ]
        if (nrow(subjects) > 0) {
          subjects <- subjects[order(-subjects$valueOrig), ]
          tempMisses <- subjects[min(50, nrow(subjects)), c("subjectId", "cohortStartDate", "daysFromObsStart", "valueOrig")]
          tempMisses$miss <- "FN"
          misses <- dplyr::bind_rows(misses, tempMisses)
        }
      }
      newRow <- tibble::tibble(
        truePositives = sum(fullTable$tp, na.rm = TRUE),
        trueNegatives = sum(fullTable$tn, na.rm = TRUE),
        falsePositives = sum(fullTable$fp, na.rm = TRUE),
        falseNegatives = sum(fullTable$fn, na.rm = TRUE)
      )

      if (cutPoints[cpUp] == xSpecP) {
        newRow$cutPoint <- paste("EmpirCP0.5 (", round(xSpecP, 2), ")", sep = "")
      } else if (cutPoints[cpUp] == xSpecP2) {
        newRow$cutPoint <- paste("EmpirCP1.0 (", round(xSpecP2, 2), ")", sep = "")
      } else if (cutPoints[cpUp] == xSpecP3) {
        newRow$cutPoint <- paste("EmpirCP1.5 (", round(xSpecP3, 2), ")", sep = "")
      } else if (cutPoints[cpUp] == "EV") {
        newRow$cutPoint <- paste("Expected Value", sep = "")
      } else {
        newRow$cutPoint <- cutPoints[cpUp]
      }
      countsTable <- dplyr::bind_rows(countsTable, newRow)
    }

    if(nrow(countsTable) > 0) {
      countsTable <- computePerformanceMetricsFromCounts(countsTable)

      totalSubjects <- nrow(fullTable)
      rawCases <- nrow(fullTable[!is.na(fullTable$comparisonCohortStartDate),])
      estimatedCases <- countsTable$truePositives + countsTable$falseNegatives

      rawTar <-  sum(fullTable$rawTar/365, na.rm = TRUE) #tar in years
      estimatedTar <- sum(fullTable$estimatedTar/365, na.rm = TRUE) #tar in years

      rawPrevalence <- rawCases/totalSubjects
      estimatedPrevalence <- countsTable$estimatedPrevalence

      rawPrevalenceRate <- rawCases*100/rawTar
      estimatedPrevalenceRate <- estimatedCases*100/estimatedTar

      # Make pretty results table
      results <- tibble::tibble(
        phenotype = phenotype,
        databaseId = cdmDatabaseSchema,
        cohortId = phenotypeCohortId,
        sensitivity95Ci = sprintf("%0.3f (%0.3f - %0.3f)", countsTable$sens, countsTable$sensCi95Lb, countsTable$sensCi95Ub),
        ppv95Ci = sprintf("%0.3f (%0.3f - %0.3f)", countsTable$ppv, countsTable$ppvCi95Lb, countsTable$ppvCi95Ub),
        specificity95Ci = sprintf("%0.3f (%0.3f - %0.3f)", countsTable$spec, countsTable$specCi95Lb, countsTable$specCi95Ub),
        npv95Ci = sprintf("%0.3f (%0.3f - %0.3f)", countsTable$npv, countsTable$npvCi95Lb, countsTable$npvCi95Ub),
        f1Score = sprintf("%0.3f", countsTable$f1Score),

        totalSubjects = totalSubjects ,
        rawCases = rawCases,
        estimatedCases = round(estimatedCases, 0),

        rawTar = sprintf("%0.1f", rawTar),
        estimatedTar = sprintf("%0.1f", estimatedTar),

        rawPrevalence = sprintf("%0.3f", 100 * rawPrevalence),
        estimatedPrevalence = sprintf("%0.3f", 100 * estimatedPrevalence),

        rawPrevalenceRate = sprintf("%0.3f", rawPrevalenceRate),
        estimatedPrevalenceRate = sprintf("%0.3f", estimatedPrevalenceRate),

        truePositives = round(countsTable$truePositives, 0),
        trueNegatives = round(countsTable$trueNegatives, 0),
        falsePositives = round(countsTable$falsePositives, 0),
        falseNegatives = round(countsTable$falseNegatives, 0),
        washoutPeriod = washoutPeriod,
        splayPrior = splayPrior,
        splayPost = splayPost,
        cutPoint = countsTable$cutPoint,
        sensitivity = sprintf("%0.6f", countsTable$sens),
        sensitivityCi95Lb = sprintf("%0.6f ", countsTable$sensCi95Lb),
        sensitivityCi95Ub = sprintf("%0.6f", countsTable$sensCi95Ub),
        ppv = sprintf("%0.6f", countsTable$ppv),
        ppvCi95Lb = sprintf("%0.6f", countsTable$ppvCi95Lb),
        ppvCi95Ub = sprintf("%0.6f", countsTable$ppvCi95Ub),
        specificity = sprintf("%0.6f", countsTable$spec),
        specificityCi95Lb = sprintf("%0.6f", countsTable$specCi95Lb),
        specificityCi95Ub = sprintf("%0.6f", countsTable$specCi95Ub),
        npv = sprintf("%0.6f", countsTable$npv),
        npvCi95Lb = sprintf("%0.6f", countsTable$npvCi95Lb),
        npvCi95Ub = sprintf("%0.6f", countsTable$npvCi95Ub),
        analysisName = analysisName,
        runDateTimeGMT = runDateTime
      )
    } else {
      results <- tibble::tibble()
    }

    if (nrow(misses) > 0) {
      attr(results, "misses") <- misses
    }
    delta <- Sys.time() - start
    ParallelLogger::logInfo("Testing phenotype algorithm took ", signif(delta, 3), " ", attr(delta, "units"))
  }
  return(results)
}

computePerformanceMetricsFromCounts <- function(counts) {
  # Note: negative counts indicate the cell counts was below the specified minimum for sharing.

  computeSingleProportion <- function(i, x, n) {
    if(as.integer(n[i]) < as.integer(x[i]) | as.integer(n[i]) == 0) {n[i] <- 1; x[i] <- 0} #set to 0 if n < x or n = 0 to avoid error
    exact <- binom.test(as.integer(x[i]), as.integer(n[i]), conf.level = 0.95)
    return(tibble::tibble(
      estimate = exact$estimate,
      ci95Lb = exact$conf.int[1],
      ci95Ub = exact$conf.int[2]
    ))
  }

  computeProportions <- function(x, n, name) {
    proportions <- lapply(1:length(x), computeSingleProportion, x = abs(x), n = n)
    proportions <- dplyr::bind_rows(proportions)
    names(proportions) <- paste0(name, c("", "Ci95Lb", "Ci95Ub"))
    proportions[x < 0, ] <- -proportions[x < 0, ]
    return(proportions)
  }

  counts$falseNegatives[counts$falseNegatives == 0] <- 1 # prevent division by 0
  counts$falsePositives[counts$falsePositives == 0] <- 1 # prevent division by 0
  counts <- dplyr::bind_cols(
    counts,
    computeProportions(
      counts$truePositives,
      abs(counts$truePositives) + abs(counts$falseNegatives),
      "sens"
    )
  )

  counts <- dplyr::bind_cols(
    counts,
    computeProportions(
      counts$truePositives,
      abs(counts$truePositives) + abs(counts$falsePositives),
      "ppv"
    )
  )

  counts <- dplyr::bind_cols(
    counts,
    computeProportions(
      counts$trueNegatives,
      abs(counts$trueNegatives) + abs(counts$falsePositives),
      "spec"
    )
  )

  counts <- dplyr::bind_cols(
    counts,
    computeProportions(
      counts$trueNegatives,
      abs(counts$trueNegatives) + abs(counts$falseNegatives),
      "npv"
    )
  )

  counts$estimatedPrevalence <- (abs(counts$truePositives) + abs(counts$falseNegatives)) / (abs(counts$truePositives) + abs(counts$trueNegatives) + abs(counts$falsePositives) + abs(counts$falseNegatives))
  idx <- counts$truePositives < 0 | counts$falseNegatives < 0
  counts$estimatedPrevalence[idx] <- -counts$estimatedPrevalence[idx]

  counts$f1Score <- 1 / ((1 / abs(counts$sens) + 1 / abs(counts$ppv)) / 2)
  return(counts)
}
