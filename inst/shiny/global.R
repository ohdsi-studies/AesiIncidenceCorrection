library(magrittr)
source("R/MeasErrorCorrection.R")

summaryTable <- readr::read_csv("data/correctedIrSummary.csv", show_col_types = FALSE) %>%
  dplyr::select(-c(targetCohortDefinitionId,
                   outcomeCohortDefinitionId)) %>%
  dplyr::rename(source = sourceName,
                tName = targetName,
                oName = outcomeName,
                atRisk = personsAtRisk,
                prev = estimatedPrevalence,
                sens = sensitivity,
                spec = specificity,
                ip100p = incidenceProportionP100p,
                ir100py = incidenceRateP100py,
                tp = truePositives,
                tn = trueNegatives,
                fp = falsePositives,
                fn = falseNegatives,
                cOutcomes = correctedOutcomes,
                cIp100p = correctedIp100p,
                cPersonDays = correctedPersonDays,
                cIr100py = correctedIr100py,
                ipDiff = absIpDiff,
                ipRel = relIpDiff,
                irDiff = absIrDiff,
                irRel = relIrDiff) %>%
  dplyr::mutate(ip100p = round(ip100p, 3),
                ir100py = round(ir100py, 3),
                sens = round(sens, 5),
                spec = round(spec, 5),
                ppv = round(ppv, 5),
                npv = round(npv, 5),
                prev = prev / 100,
                cOutcomes = round(cOutcomes, 0),
                cIp100p = round(cIp100p, 3),
                ipDiff = round(ipDiff, 3),
                ipRel = round(ipRel, 3),
                eameIp = round(eameIp, 3),
                cPersonDays = round(cPersonDays, 0),
                cIr100py = round(cIr100py, 3),
                irDiff = round(irDiff, 3),
                irRel = round(irRel, 3),
                eameIr = round(eameIr, 3))

summaryTable$tName[summaryTable$tName == "[kr11] Persons 55-64 with observation period at Jan2019 exit fixed 365d"] <- "Jan2019 55-64y"
summaryTable$tName[summaryTable$tName == "[kr11] Persons 65-75 with observation period at Jan2019 exit fixed 365d"] <- "Jan2019 65-75y"
summaryTable$oName[summaryTable$oName == "[pheWg] acute pancreatitis"] <- "AP"
summaryTable$oName[summaryTable$oName == "[pheWg] acute pancreatitis IP (Razavi)"] <- "AP IP"
summaryTable$source <- sub("cdm_", "", summaryTable$source)
summaryTable$source <- sub("_v.*", "", summaryTable$source)

irTable <- summaryTable %>%
  dplyr::select(method, source, tName, oName,
                atRisk, personDays, outcomes,
                ip100p, cIp100p, ipDiff, ipRel, eameIp,
                ir100py, cIr100py, irDiff, irRel, eameIr,
                cOutcomes, cPersonDays)

validationTable <- summaryTable %>%
  dplyr::select(source, oName, tp, tn, fp, fn, prev, sens, spec, ppv, npv) %>%
  unique()

dbMaIrSummary <- readr::read_csv("data/irMaSummary.csv", show_col_types = FALSE)

maMetrics <- dbMaIrSummary %>%
  dplyr::filter(sourceName == "rndMa") %>%
  dplyr::select(method,
                sourceName,
                targetName,
                outcomeName,
                ir,
                irLower,
                irUpper,
                intervalWidth,
                seIr,
                irTau2,
                irTau,
                i2,
                k)

# user inputs
methods <- unique(maMetrics$method)
databases <- unique(summaryTable$source)
targets <- unique(summaryTable$tName)
outcomes <- unique(summaryTable$oName)

# set inputs
inputSens <- validationTable$sens[validationTable$source == databases[1] & validationTable$oName == outcomes[1]]
inputSpec <- validationTable$spec[validationTable$source == databases[1] & validationTable$oName == outcomes[1]]
inputPpv <- validationTable$ppv[validationTable$source == databases[1] & validationTable$oName == outcomes[1]]
inputNpv <- validationTable$npv[validationTable$source == databases[1] & validationTable$oName == outcomes[1]]
