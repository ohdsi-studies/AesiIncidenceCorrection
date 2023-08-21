library(magrittr)
source("R/MeasErrorCorrection.R")

summaryTable <- readr::read_csv("data/correctedIrSummary.csv", show_col_types = FALSE) %>%
  dplyr::select(-c(targetCohortDefinitionId,
                   outcomeCohortDefinitionId)) %>%
  dplyr::rename(
    source = sourceName,
    tName = targetName,
    oName = outcomeName,
    atRisk = personsAtRisk,
    prev = estimatedPrevalence,
    sens = sensitivity,
    spec = specificity,
    ip100k_p = incidenceProportionP100p,
    ir100k_py = incidenceRateP100py,
    tp = truePositives,
    tn = trueNegatives,
    fp = falsePositives,
    fn = falseNegatives,
    cOutcomes = correctedOutcomes,
    cIp100k_p = correctedIp100p,
    cPersonDays = correctedPersonDays,
    cIr100k_py = correctedIr100py,
    ipDiff = absIpDiff,
    ipRel = relIpDiff,
    irDiff = absIrDiff,
    irRel = relIrDiff
  ) %>%
  dplyr::mutate(
    ip100k_p = round(ip100k_p * 1000, 0),
    ir100k_py = round(ir100k_py * 1000, 0),
    sens = round(sens, 5),
    spec = round(spec, 5),
    ppv = round(ppv, 5),
    npv = round(npv, 5),
    prev = prev / 100,
    cOutcomes = round(cOutcomes, 0),
    cIp100k_p = round(cIp100k_p * 1000, 0),
    ipDiff = round(ipDiff * 1000, 0),
    ipRel = round(ipRel, 3),
    eameIp = round(eameIp, 3),
    cPersonDays = round(cPersonDays, 0),
    cIr100k_py = round(cIr100k_py * 1000, 0),
    irDiff = round(irDiff * 1000, 0),
    irRel = round(irRel, 3),
    eameIr = round(eameIr, 3)
  )

summaryTable$tName[summaryTable$tName == "[IrCorrection] Persons observed on 1 Jan 2017-2019"] <- "1 Jan 2017-2019"
summaryTable$oName <- sub("\\[IrCorrection\\]", "", summaryTable$oName)
summaryTable$source <- sub("cdm_", "", summaryTable$source)
summaryTable$source <- sub("_v.*", "", summaryTable$source)

irTable <- summaryTable %>%
  dplyr::select(
    method,
    source,
    tName,
    oName,
    atRisk,
    personDays,
    outcomes,
    ip100k_p,
    cIp100k_p,
    ipDiff,
    ipRel,
    eameIp,
    ir100k_py,
    cIr100k_py,
    irDiff,
    irRel,
    eameIr,
    cOutcomes,
    cPersonDays
  )

validationTable <- summaryTable %>%
  dplyr::select(
    source,
    oName,
    tp,
    tn,
    fp,
    fn,
    prev,
    sens,
    spec,
    ppv,
    npv
  ) %>%
  unique()

dbMaIrSummary <- readr::read_csv("data/irMaSummary.csv", show_col_types = FALSE)

dbMaIrSummary$targetName[dbMaIrSummary$targetName == "[IrCorrection] Persons observed on 1 Jan 2017-2019"] <- "1 Jan 2017-2019"
dbMaIrSummary$outcomeName <- sub("\\[IrCorrection\\]", "", dbMaIrSummary$outcomeName)
dbMaIrSummary$sourceName <- sub("cdm_", "", dbMaIrSummary$sourceName)
dbMaIrSummary$sourceName <- sub("_v.*", "", dbMaIrSummary$sourceName)

maMetrics <- dbMaIrSummary %>%
  dplyr::select(
    method,
    sourceName,
    targetName,
    outcomeName,
    ir,
    irLowerCi,
    irUpperCi,
    ciIntervalWidth,
    irLowerPredict,
    irUpperPredict,
    predictIntervalWidth,
    seIr,
    irTau2,
    irTau,
    i2,
    k
  )

# user inputs
methods <- unique(maMetrics$method)
databases <- unique(c(irTable$source, maMetrics$sourceName))
targets <- unique(summaryTable$tName)
outcomes <- unique(summaryTable$oName)
intervals <- c("ci", "prediction")

# set inputs
inputSens <- validationTable$sens[validationTable$source == databases[1] & validationTable$oName == outcomes[1]]
inputSpec <- validationTable$spec[validationTable$source == databases[1] & validationTable$oName == outcomes[1]]
inputPpv <- validationTable$ppv[validationTable$source == databases[1] & validationTable$oName == outcomes[1]]
inputNpv <- validationTable$npv[validationTable$source == databases[1] & validationTable$oName == outcomes[1]]

