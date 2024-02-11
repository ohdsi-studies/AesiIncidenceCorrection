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
    IP = incidenceProportionP100p,
    IR = incidenceRateP100py,
    TP = truePositives,
    TN = trueNegatives,
    FP = falsePositives,
    FN = falseNegatives,
    cOutcomes = correctedOutcomes,
    cIP = correctedIp100p,
    cPersonDays = correctedPersonDays,
    cIR = correctedIr100py,
    IPdiff = absIpDiff,
    IPrel = relIpDiff,
    IRdiff = absIrDiff,
    IRrel = relIrDiff
  ) %>%
  dplyr::mutate(
    IP = round(IP * 1000, 0),
    IR = round(IR * 1000, 0),
    sens = round(sens, 5),
    spec = round(spec, 5),
    ppv = round(ppv, 5),
    npv = round(npv, 5),
    prev = prev / 100,
    cOutcomes = round(cOutcomes, 0),
    cIP = round(cIP * 1000, 0),
    IPdiff = round(IPdiff * 1000, 0),
    IPrel = round(IPrel, 3),
    IPeame = round(eameIp, 3),
    cPersonDays = round(cPersonDays, 0),
    cIR = round(cIR * 1000, 0),
    IRdiff = round(IRdiff * 1000, 0),
    IRrel = round(IRrel, 3),
    IReame = round(eameIr, 3)
  ) %>%
  dplyr::relocate(
    source,
    method,
    stratum
  )

summaryTable$sourceOrder <- match(
  summaryTable$source,
  c("optum_extended_dod",
    "truven_ccae",
    "truven_mdcd",
    "truven_mdcr",
    "optum_ehr")
  )
summaryTable$methodOrder <- match(
  summaryTable$method,
  c("sensSpec",
    "ppvNpv",
    "sensPpv",
    "fpRate")
  )

summaryTable$stratumOrder <- match(
  summaryTable$stratum,
  c("ALL",
    "Male",
    "Female",
    "<5",
    "18 - 34",
    "55 - 64",
    "35 - 54",
    "5 - 17",
    "75 - 84",
    "65 - 74",
    ">=85")
  )

summaryTable <- summaryTable[order(summaryTable$sourceOrder,
                                   summaryTable$methodOrder,
                                   summaryTable$stratumOrder,
                                   summaryTable$oName), ]
summaryTable$methodOrder <- NULL
summaryTable$stratumOrder <- NULL
summaryTable$sourceOrder <- NULL


summaryTable$tName[summaryTable$tName == "[IrCorrection] Persons observed on 1 Jan 2017-2019"] <- "1 Jan 2017-2019"
summaryTable$oName <- sub("\\[IrCorrection\\]", "", summaryTable$oName)
summaryTable$source <- sub("cdm_", "", summaryTable$source)
summaryTable$source <- sub("_v.*", "", summaryTable$source)

irTable <- summaryTable %>%
  dplyr::select(
    method,
    source,
    stratum,
    tName,
    oName,
    atRisk,
    personDays,
    outcomes,
    IP,
    cIP,
    IPdiff,
    IPrel,
    IPeame,
    IR,
    cIR,
    IRdiff,
    IRrel,
    IReame,
    cOutcomes,
    cPersonDays
  )

validationTable <- summaryTable %>%
  dplyr::select(
    source,
    stratum,
    oName,
    TP,
    TN,
    FP,
    FN,
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
    source = sourceName,
    stratum,
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

maMetrics$sourceOrder <- match(
  maMetrics$source,
  c("optum_extended_dod",
    "truven_ccae",
    "truven_mdcd",
    "truven_mdcr",
    "optum_ehr")
)
maMetrics$methodOrder <- match(
  maMetrics$method,
  c("uncorrected",
    "sensSpec",
    "ppvNpv",
    "sensPpv",
    "fpRate")
)

maMetrics$stratumOrder <- match(
  maMetrics$stratum,
  c("ALL",
    "Male",
    "Female",
    "<5",
    "5 - 17",
    "18 - 34",
    "35 - 54",
    "55 - 64",
    "65 - 74",
    "75 - 84",
    ">=85")
)

maMetrics <- maMetrics[order(maMetrics$sourceOrder,
                             maMetrics$methodOrder,
                             maMetrics$stratumOrder,
                             maMetrics$outcomeName), ]
maMetrics$methodOrder <- NULL
maMetrics$stratumOrder <- NULL
maMetrics$sourceOrder <- NULL

# user inputs
methods <- unique(maMetrics$method)
databases <- unique(c(irTable$source, maMetrics$source))
strata <- unique(maMetrics$stratum)
targets <- unique(summaryTable$tName)
outcomes <- unique(summaryTable$oName)
intervals <- c("ci", "prediction")

# set inputs
inputSens <- validationTable$sens[validationTable$source == databases[1] & validationTable$oName == outcomes[1] & validationTable$stratum == strata[1]]
inputSpec <- validationTable$spec[validationTable$source == databases[1] & validationTable$oName == outcomes[1] & validationTable$stratum == strata[1]]
inputPpv <- validationTable$ppv[validationTable$source == databases[1] & validationTable$oName == outcomes[1] & validationTable$stratum == strata[1]]
inputNpv <- validationTable$npv[validationTable$source == databases[1] & validationTable$oName == outcomes[1] & validationTable$stratum == strata[1]]

