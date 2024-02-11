#' @export
evaluateIrCorrection <- function(outputFolder) {     # outputFolder <- "G:/aesiIncidenceCorrection"

  correctedIrSummaryFile <- file.path(outputFolder, "correctedIrSummary.csv")
  correctedIrSummary <- readr::read_csv(correctedIrSummaryFile, show_col_types = FALSE)

  # correctedIrSummary$stratumId <- correctedIrSummary$stratum
  # correctedIrSummary$stratumId[correctedIrSummary$stratumId == "<5"] <- "_5"
  # correctedIrSummary$stratumId[correctedIrSummary$stratumId == ">=85"] <- "_85"

  uncorrectedIrs <- getUncorrectedIrCis(correctedIrSummary)
  correctedIrs <- getCorrectedIrCis(correctedIrSummary)
  irSummary <- dplyr::bind_rows(uncorrectedIrs, correctedIrs)

  maSummary <- computeIrMa(irSummary)
  results <- dplyr::bind_rows(irSummary, maSummary)

  readr::write_csv(
    x = results,
    file = file.path(outputFolder, "irMaSummary.csv")
  )
}


getUncorrectedIrCis <- function(irSummary) {  # irSummary <- correctedIrSummary

  irSummary <- irSummary %>% dplyr::mutate(
    personYears = personDays / 365.25,
    correctedPersonYears = correctedPersonDays / 365.25
  )

  irs <- irSummary %>%
    dplyr::select(
      sourceName,
      stratum,
      targetCohortDefinitionId,
      targetName,
      outcomeCohortDefinitionId,
      outcomeName,
      outcomes,
      personYears,
      incidenceRateP100py
    ) %>%
    dplyr::distinct(
      sourceName,
      stratum,
      targetCohortDefinitionId,
      outcomeCohortDefinitionId,
      .keep_all = TRUE
    )

  irGroups <- split(
    x = irs,
    f = paste(irs$stratum, irs$targetCohortDefinitionId, irs$outcomeCohortDefinitionId),
  )

  irCis <- lapply(
    X = irGroups,
    FUN = computeUncorrectedIrCis
  )
  irCis <- dplyr::bind_rows(irCis)
  return(irCis)
}

computeUncorrectedIrCis <- function(irGroup) { # irGroup <- irGroups[[5]]

  irGroup <- irGroup %>% dplyr::filter(outcomes > 0)

  irRnd <- meta::metarate(
    data = irGroup,
    event = outcomes,
    time = personYears,
    method = "Inverse",
    studlab = sourceName,
    sm = "IRLN",
    random = TRUE,
    method.tau = "DL",
    irscale = 100000,
    irunit = "person-years"
  )

  irGroup <- irGroup %>%
    dplyr::mutate(
      method = "uncorrected",
      ir = exp(irRnd$TE) * 100000,
      irLowerCi = irRnd$lower * 100000,
      irUpperCi = irRnd$upper * 100000,
      ciIntervalWidth = irRnd$upper * 100000 - irRnd$lower * 100000,
      irLowerPredict = rep(NA, nrow(irGroup)),
      irUpperPredict = rep(NA, nrow(irGroup)),
      predictIntervalWidth = rep(NA, nrow(irGroup))
    ) %>%
    dplyr::relocate(
      method
    )
  return(irGroup)
}


getCorrectedIrCis <- function(irSummary) { # irSummary <- correctedIrSummary

  irSummary <- irSummary %>% dplyr::mutate(
    personYears = personDays / 365.25,
    correctedPersonYears = correctedPersonDays / 365.25
  )

  irs <- irSummary %>%
    dplyr::mutate(
      correctedPersonYears = ifelse(method == "fpRate", personYears, correctedPersonYears) # person-time correction made when calculating fpRate
    ) %>%
    dplyr::select(
      method,
      sourceName,
      stratum,
      targetCohortDefinitionId,
      targetName,
      outcomeCohortDefinitionId,
      outcomeName,
      outcomes = correctedOutcomes,
      personYears = correctedPersonYears,
      incidenceRateP100py = correctedIr100py
    ) %>%
    dplyr::distinct( # no change here, maybe necessary when age/sex stratified
      method,
      sourceName,
      stratum,
      targetCohortDefinitionId,
      outcomeCohortDefinitionId,
      .keep_all = TRUE
    )

  irGroups <- split(
    x = irs,
    f = paste(irs$method, irs$stratum, irs$targetCohortDefinitionId, irs$outcomeCohortDefinitionId)
  )

  irCis <- lapply(
    X = irGroups,
    FUN = computeCorrectedIrCis # warming: Zero values in seTE replaced by NAs
  )

  irCis <- dplyr::bind_rows(irCis)
  return(irCis)
}

computeCorrectedIrCis <- function(irGroup) { # irGroup <- irGroups[[1]]

  irGroup <- irGroup %>% dplyr::filter(outcomes > 0)

  irRnd <- meta::metarate(
    data = irGroup,
    event = outcomes,
    time = personYears,
    method = "Inverse",
    studlab = sourceName,
    sm = "IRLN",
    random = TRUE,
    method.tau = "DL",
    irscale = 100000,
    irunit = "person-years"
  )

  irGroup <- irGroup %>%
    dplyr::mutate(
      ir = exp(irRnd$TE) * 100000,
      irLowerCi = irRnd$lower * 100000,
      irUpperCi = irRnd$upper * 100000,
      ciIntervalWidth = irRnd$upper * 100000 - irRnd$lower * 100000,
      irLowerPredict = rep(NA, nrow(irGroup)),
      irUpperPredict = rep(NA, nrow(irGroup)),
      predictIntervalWidth = rep(NA, nrow(irGroup))
    )

  return(irGroup)
}


computeIrMa <- function(irSummary) {

  irs <- irSummary %>%
    dplyr::select(
      method,
      sourceName,
      stratum,
      targetCohortDefinitionId,
      targetName,
      outcomeCohortDefinitionId,
      outcomeName,
      outcomes,
      personYears
    ) %>%
    dplyr::distinct(
      method,
      stratum,
      sourceName,
      targetCohortDefinitionId,
      outcomeCohortDefinitionId,
      .keep_all = TRUE
    )

  irGroups <- split(
    x = irs,
    f = paste(irs$method, irs$stratum, irs$targetCohortDefinitionId, irs$outcomeCohortDefinitionId),
    drop = TRUE
  )

  maDlResults <- lapply(
    X = irGroups,
    FUN = computeMa
  )
  maResults <- dplyr::bind_rows(maDlResults)
  return(maResults)
}


computeMa <- function(irGroup) { # irGroup <- irGroups[["sensSpec ALL 12100 12088"]]

  irGroup <- irGroup %>% dplyr::filter(outcomes > 0 & !is.infinite(outcomes))

  irRnd <- meta::metarate(
    data = irGroup,
    event = outcomes,
    time = personYears,
    method = "Inverse",
    studlab = sourceName,
    sm = "IRLN",
    random = TRUE,
    method.tau = "DL",
    irscale = 100000,
    irunit = "person-years",
    prediction = TRUE
  )

  irMa <- tibble::tibble(
    method = irGroup$method[1],
    sourceName = "dlMa",
    stratum = irGroup$stratum[1],
    targetCohortDefinitionId = irGroup$targetCohortDefinitionId[1],
    targetName = irGroup$targetName[1],
    outcomeCohortDefinitionId = irGroup$outcomeCohortDefinitionId[1],
    outcomeName = irGroup$outcomeName[1],
    ir = exp(irRnd$TE.random) * 100000,
    irLowerCi = exp(irRnd$lower.random) * 100000,
    irUpperCi = exp(irRnd$upper.random) * 100000,
    ciIntervalWidth = exp(irRnd$upper.random) * 100000 - exp(irRnd$lower.random) * 100000,
    irLowerPredict = exp(irRnd$lower.predict) * 100000,
    irUpperPredict = exp(irRnd$upper.predict) * 100000,
    predictIntervalWidth = (exp(irRnd$upper.predict) * 100000) - (exp(irRnd$lower.predict) * 100000),
    seIr = irRnd$seTE.random,
    irTau2 = irRnd$tau2,
    irTau = irRnd$tau,
    i2 = irRnd$I2,
    k = irRnd$k
  )

  return(irMa)
}
