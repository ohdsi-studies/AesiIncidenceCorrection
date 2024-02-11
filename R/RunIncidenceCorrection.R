#' @export
runIncidenceCorrection <- function(outputFolder,
                                   databaseIds) {

  # outputFolder <- "G:/aesiIncidenceCorrection"
  # databaseIds <- c("optum_extended_dod", "truven_ccae", "truven_mdcd", "truven_mdcr", "optum_ehr")

  irSummary <- tibble::tibble()
  valSummary <- tibble::tibble()

  for (databaseId in databaseIds) { # databaseId <- databaseIds[1]

    irSummaryDbFile <- file.path(outputFolder, databaseId, "incidence", "irSummary.csv")

    irSummaryNoStrata <- readr::read_csv(irSummaryDbFile, show_col_types = FALSE) %>%
      dplyr::filter(is.na(startYear) & is.na(genderId) & is.na(ageId))

    irSummaryAgeStrata <- readr::read_csv(irSummaryDbFile, show_col_types = FALSE) %>%
      dplyr::filter(is.na(startYear) & is.na(genderId) & !is.na(ageId))

    irSummarySexStrata <- readr::read_csv(irSummaryDbFile, show_col_types = FALSE) %>%
      dplyr::filter(is.na(startYear) & !is.na(genderId) & is.na(ageId))

    # do later, needs validation fixing
    # irSummaryAgeSexStrata <- readr::read_csv(irSummaryDbFile, show_col_types = FALSE) %>%
    #   dplyr::filter(is.na(startYear) & !is.na(genderId) & !is.na(ageId))

    irSummaryDb <- dplyr::bind_rows(
      irSummaryNoStrata,
      irSummaryAgeStrata,
      irSummarySexStrata
    ) %>%
    dplyr::mutate(
      stratum = ifelse(is.na(genderName) & is.na(ageGroupName), "ALL", NA),
      stratum = ifelse(is.na(stratum) & is.na(genderName) & !is.na(ageGroupName), ageGroupName, stratum),
      stratum = ifelse(is.na(stratum) & !is.na(genderName) & is.na(ageGroupName), genderName, stratum)
    ) %>%
    dplyr::select(
      sourceName,
      targetCohortDefinitionId,
      targetName,
      outcomeCohortDefinitionId,
      outcomeName,
      stratum,
      personsAtRisk,
      personDays,
      outcomes,
      incidenceProportionP100p,
      incidenceRateP100py
    )
    irSummaryDb$stratum[irSummaryDb$stratum == "MALE"] <- "Male"
    irSummaryDb$stratum[irSummaryDb$stratum == "FEMALE"] <- "Female"

    irSummary <- dplyr::bind_rows(irSummary, irSummaryDb)


    valSummaryDbFile <- file.path(outputFolder, databaseId, "pheValuator", "pvResults.csv")
    valSummaryDb <- readr::read_csv(valSummaryDbFile, show_col_types = FALSE) %>%
      dplyr::filter(cutPoint != "Error: Too few outcomes to produce model") %>%
      dplyr::mutate(
        cdm = gsub("\\_v....", "", cdm),
        cdm = gsub("cdm_", "", cdm),
        stratum = "ALL"
      ) %>%
      dplyr::select(
        cohortId,
        cdm,
        stratum,
        truePositives,
        trueNegatives,
        falsePositives,
        falseNegatives,
        estimatedPrevalence,
        sensitivity,
        specificity,
        ppv,
        npv
      )

    valStratSummaryDbFile <- file.path(outputFolder, databaseId, "pheValuator", "stratifiedPvResults.csv")
    valStratSummaryDb <- readr::read_csv(valStratSummaryDbFile, show_col_types = FALSE) %>%
      dplyr::filter(!is.na(stratum)) %>%
      dplyr::mutate(
        cdm = gsub("\\_v....", "", cdm),
        cdm = gsub("cdm_", "", cdm)
      ) %>%
      dplyr::select(
        cohortId,
        cdm,
        stratum,
        truePositives,
        trueNegatives,
        falsePositives,
        falseNegatives,
        estimatedPrevalence,
        sensitivity,
        specificity,
        ppv,
        npv
      )

    valSummary <- dplyr::bind_rows(
      valSummary,
      valSummaryDb,
      valStratSummaryDb
    )
  }

  rm(irSummaryNoStrata,
     irSummaryAgeStrata,
     irSummarySexStrata,
     irSummaryDb,
     valSummaryDb,
     valStratSummaryDb)


  irSummaryVal <- dplyr::inner_join(
    x = irSummary,
    y = valSummary,
    by = c("sourceName" = "cdm",
           "outcomeCohortDefinitionId" = "cohortId",
           "stratum" = "stratum")
  )

  # keep IRs without corresponding phenotype errors
  # irSummaryLeftTest <- dplyr::left_join(
  #   x = irSummary,
  #   y = valSummary,
  #   by = c("sourceName" = "cdm",
  #          "outcomeCohortDefinitionId" = "cohortId",
  #          "stratum" = "stratum")
  # )


  irSummaryValList <- split(irSummaryVal, 1:nrow(irSummaryVal))
  irSummaryValList <- lapply(irSummaryValList, correctIpIr, method = "sensSpec")
  irSummarySensSpec <- dplyr::bind_rows(irSummaryValList)

  irSummaryValList <- split(irSummaryVal, 1:nrow(irSummaryVal))
  irSummaryValList <- lapply(irSummaryValList, correctIpIr, method = "ppvNpv")
  irSummaryPpvNpv <- dplyr::bind_rows(irSummaryValList)

  irSummaryValList <- split(irSummaryVal, 1:nrow(irSummaryVal))
  irSummaryValList <- lapply(irSummaryValList, correctIpIr, method = "sensPpv")
  irSummarySensPpv <- dplyr::bind_rows(irSummaryValList)

  irSummaryValList <- split(irSummaryVal, 1:nrow(irSummaryVal))
  irSummaryValList <- lapply(irSummaryValList, correctIpIr, method = "fpRate")
  irSummaryFpRate <- dplyr::bind_rows(irSummaryValList)

  irSummaryFinal <-  dplyr::bind_rows(irSummarySensSpec,
                                      irSummaryPpvNpv,
                                      irSummarySensPpv,
                                      irSummaryFpRate)

  readr::write_csv(
    x = irSummaryFinal,
    file = file.path(outputFolder, "correctedIrSummary.csv")
  )
}

correctIpIr <- function(row,       # row <- irSummaryVal[, ]
                        method) {  # "sensSpec" or "ppvNpv" or "sensPpv" or "fpRate"

  #print(c(row$sourceName, row$outcomeName, row$stratum))

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
    correctedOutcomes <- (outcomes - (1 - spec) * personsAtRisk) / (sens - (1 - spec))
  }

  if (method == "ppvNpv") {
    correctedOutcomes <- (outcomes * ppv + (personsAtRisk - outcomes) * (1 - npv))
  }

  if (method == "sensPpv") {
    correctedOutcomes <- outcomes * ppv / sens
  }

  if (method == "fpRate") {
    fpRate <- outcomes * (1 - spec) / personDays
    correctedOutcomes <- (outcomes - (fpRate * personDays)) / sens
  }

  #print(correctedOutcomes)

  if (is.nan(correctedOutcomes)) {
    warning(sprintf("correctedOutcomes is NaN in %s, %s, %s, %s", method, row$sourceName, row$targetName, row$outcomeName))
    correctedIp100p <- NA
    absIpDiff <- NA
    relIpDiff <- NA
    eameIp <- NA
    correctedPersonDays <- NA
    correctedIr100py <- NA
    absIrDiff <- NA
    relIrDiff <- NA
    eameIr <- NA

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
