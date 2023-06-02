#' @export
runIncidenceAnalysis <- function(outputFolder,     # outputFolder <- "G:/aesiIncidenceCorrection"
                                 cohortRef) {

  databaseIds <- cdmSources$databaseName


  irSummary <- tibble::tibble()
  valSummary <- tibble::tibble()

  for (databaseId in databaseIds) { # databaseId <- databaseIds[1]

    irSummaryDbFile <- file.path(outputFolder, databaseId, "incidence", "irSummary.csv")
    irSummaryDb <- readr::read_csv(irSummaryDbFile, show_col_types = FALSE) %>%
      dplyr::filter(is.na(startYear) & is.na(genderId) & is.na(ageId)) %>%
      dplyr::select(
        sourceName,
        targetCohortDefinitionId,
        targetName,
        outcomeCohortDefinitionId,
        outcomeName,
        personsAtRisk,
        personDays,
        outcomes,
        incidenceProportionP100p,
        incidenceRateP100py
      )
    irSummary <- dplyr::bind_rows(irSummary, irSummaryDb)


    valSummaryDbFile <- file.path(outputFolder, databaseId, "pheValuator", "pvResults.csv")
    valSummaryDb <- readr::read_csv(valSummaryDbFile, show_col_types = FALSE) %>%
      dplyr::select(
        cohortId,
        cdm,
        truePositives,
        trueNegatives,
        falsePositives,
        falseNegatives,
        estimatedPrevalence,
        sensitivity,
        specificity,
        ppv,
        npv
      ) %>%
      dplyr::mutate(
        cdm = gsub("\\_v....", "", cdm),
        cdm = gsub("cdm_", "", cdm)
      )
    valSummary <- dplyr::bind_rows(valSummary, valSummaryDb)
  }

  irSummary <- dplyr::inner_join(
    x = irSummary,
    y = valSummary,
    by = c("sourceName" = "cdm",
           "outcomeCohortDefinitionId" = "cohortId")
  )

  irSummaryList <- split(irSummary, 1:nrow(irSummary))
  irSummaryList <- lapply(irSummaryList, correctIpIr, method = "sensSpec")
  irSummarySensSpec <- dplyr::bind_rows(irSummaryList)

  irSummaryList <- split(irSummary, 1:nrow(irSummary))
  irSummaryList <- lapply(irSummaryList, correctIpIr, method = "ppvNpv")
  irSummaryPpvNpv <- dplyr::bind_rows(irSummaryList)

  irSummaryList <- split(irSummary, 1:nrow(irSummary))
  irSummaryList <- lapply(irSummaryList, correctIpIr, method = "sensPpv")
  irSummarySensPpv <- dplyr::bind_rows(irSummaryList)

  irSummaryList <- split(irSummary, 1:nrow(irSummary))
  irSummaryList <- lapply(irSummaryList, correctIpIr, method = "fpRate")
  irSummaryFpRate <- dplyr::bind_rows(irSummaryList)

  irSummary <-  dplyr::bind_rows(irSummarySensSpec,
                                 irSummaryPpvNpv,
                                 irSummarySensPpv,
                                 irSummaryFpRate)

  readr::write_csv(
    x = irSummary,
    file = file.path(outputFolder, "correctedIrSummary.csv")
  )
}


correctIpIr <- function(row,       # row <- irSummary[1, ]
                        method) {  # "sensSpec" or "ppvNpv" or "sensPpv" or "fpRate"

  personsAtRisk <- row$personsAtRisk
  personDays <- row$personDays
  outcomes <- row$outcomes
  ip100p <- row$incidenceProportionP100p
  ir100p <- row$incidenceRateP100py
  sens <- row$sensitivity
  spec <- row$specificity
  ppv <- row$ppv
  npv <- row$npv
  daysPerPerson <- personDays / personsAtRisk

  if (method == "sensSpec") {
    correctedOutcomes <- (outcomes - (1 - spec) * personsAtRisk) / (sens - (1 - spec)) # 14328
  }

  if (method == "ppvNpv") {
    correctedOutcomes <- (outcomes * ppv + (personsAtRisk - outcomes) * (1 - npv)) # 22329.75
  }

  if (method == "sensPpv") {
    correctedOutcomes <- outcomes * ppv / sens # 16222.46
  }

  if (method == "fpRate") {
    fpRate <- outcomes * (1 - spec) / personDays
    correctedOutcomes <- (outcomes - (fpRate * personDays)) / sens # 17955.81
  }

  if (correctedOutcomes >= 0) {

    outcomesDiff <- correctedOutcomes - outcomes
    personDaysDiff <- outcomesDiff * daysPerPerson / 2 # assumes added or removed events occur halfway through avg survival time
    correctedPersonDays <- personDays + personDaysDiff

    # IP
    correctedIp100p <- correctedOutcomes / personsAtRisk * 100
    absIpDiff <- correctedIp100p - ip100p
    relIpDiff <- correctedIp100p / ip100p
    eameIp <- abs(log(relIpDiff)) # expected absolute measurement error, analogous to EASE

    # IR
    if (method == "fpRate") {
      correctedIr100py <- correctedOutcomes / personDays * 100 * 365
      correctedPersonDays <- personDays
    } else {
      correctedIr100py <- correctedOutcomes / correctedPersonDays * 100 * 365
    }
    absIrDiff <- correctedIr100py - ir100p
    relIrDiff <- correctedIr100py / ir100p
    eameIr <- abs(log(relIrDiff))

  } else {

    warning(sprintf("correctedOutcomes < 0 in %s, %s, %s, %s", method, row$sourceName, row$targetName, row$outcomeName))
    correctedIp100p <- NA
    absIpDiff <- NA
    relIpDiff <- NA
    eameIp <- NA
    correctedPersonDays <- NA
    correctedIr100py <- NA
    absIrDiff <- NA
    relIrDiff <- NA
    eameIr <- NA
  }

  row$correctedOutcomes <- correctedOutcomes
  row$correctedIp100p <- correctedIp100p
  row$absIpDiff <- absIpDiff
  row$relIpDiff <- relIpDiff
  row$eameIp <- eameIp
  row$correctedPersonDays <- correctedPersonDays
  row$correctedIr100py <- correctedIr100py
  row$absIrDiff <- absIrDiff
  row$relIrDiff <- relIrDiff
  row$eameIr <- eameIr
  row$method <- method

  row <- dplyr::relocate(row, method)
  return(row)
}
