#' @export
evaluateIrCorrection <- function(outputFolder) {     # outputFolder <- "G:/aesiIncidenceCorrection"

  correctedIrSummaryFile <- file.path(outputFolder, "correctedIrSummary.csv")
  correctedIrSummary <- readr::read_csv(correctedIrSummaryFile, show_col_types = FALSE)

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


getUncorrectedIrCis <- function(irSummary) {
  irSummary <- irSummary %>% dplyr::mutate(
    personYears = personDays / 365.25,
    correctedPersonYears = correctedPersonDays / 365.25
  )
  irs <- irSummary %>%
    dplyr::select(
      sourceName,
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
      targetCohortDefinitionId,
      outcomeCohortDefinitionId,
      .keep_all = TRUE
    )
  irGroups <- split(
    x = irs,
    f = paste(irs$targetCohortDefinitionId, irs$outcomeCohortDefinitionId)
  )
  irCis <- lapply(
    X = irGroups,
    FUN = computeUncorrectedIrCis
  )
  irCis <- dplyr::bind_rows(irCis)
  return(irCis)
}

computeUncorrectedIrCis <- function(irGroup) { # irGroup <- irGroups[[1]]
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
    irscale = 100,
    irunit = "person-years"
  )
  irGroup <- irGroup %>%
    dplyr::mutate(
      method = "uncorrected",
      ir = exp(irRnd$TE) * 100,
      irLower = irRnd$lower * 100,
      irUpper = irRnd$upper * 100,
      intervalWidth = irRnd$upper * 100 - irRnd$lower * 100,
    ) %>%
    dplyr::relocate(
      method
    )
  return(irGroup)
}



getCorrectedIrCis <- function(irSummary) {
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
      targetCohortDefinitionId,
      outcomeCohortDefinitionId,
      .keep_all = TRUE
    )
  irGroups <- split(
    x = irs,
    f = paste(irs$method, irs$targetCohortDefinitionId, irs$outcomeCohortDefinitionId)
  )
  irCis <- lapply(
    X = irGroups,
    FUN = computeCorrectedIrCis
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
    irscale = 100,
    irunit = "person-years"
  )
  irGroup <- irGroup %>%
    dplyr::mutate(
      ir = exp(irRnd$TE) * 100,
      irLower = irRnd$lower * 100,
      irUpper = irRnd$upper * 100,
      intervalWidth = irRnd$upper * 100 - irRnd$lower * 100
    )
  return(irGroup)
}


computeIrMa <- function(irSummary) {

  irs <- irSummary %>%
    dplyr::select(
      method,
      sourceName,
      targetCohortDefinitionId,
      targetName,
      outcomeCohortDefinitionId,
      outcomeName,
      outcomes,
      personYears
    ) %>%
    dplyr::distinct(
      method,
      sourceName,
      targetCohortDefinitionId,
      outcomeCohortDefinitionId,
      .keep_all = TRUE
    )

  irGroups <- split(
    x = irs,
    f = paste(irs$method, irs$targetCohortDefinitionId, irs$outcomeCohortDefinitionId)
  )

  maResults <- lapply(
    X = irGroups,
    FUN = computeMa
  )
  maResults <- dplyr::bind_rows(maResults)
  return(maResults)
}


computeMa <- function(irGroup) { # irGroup <- irGroups[[1]]

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
    irscale = 100,
    irunit = "person-years"
  )

  irMa <- tibble::tibble(
    method = irGroup$method[1],
    sourceName = "rndMa",
    targetCohortDefinitionId = irGroup$targetCohortDefinitionId[1],
    targetName = irGroup$targetName[1],
    outcomeCohortDefinitionId = irGroup$outcomeCohortDefinitionId[1],
    outcomeName = irGroup$outcomeName[1],
    ir = exp(irRnd$TE.random) * 100,
    irLower = exp(irRnd$lower.predict) * 100,
    irUpper = exp(irRnd$upper.predict) * 100,
    intervalWidth = (exp(irRnd$upper.predict) * 100) - (exp(irRnd$lower.predict) * 100),
    seIr = irRnd$seTE.random,
    irTau2 = irRnd$tau2,
    irTau = irRnd$tau,
    i2 = irRnd$I2,
    k = irRnd$k
  )
  return(irMa)
}
