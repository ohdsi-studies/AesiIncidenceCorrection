# global settings ==============================================================

library(magrittr)
options(fftempdir = "G:/Temp")
baseUrl <- Sys.getenv("BASE_URL")
ROhdsiWebApi::authorizeWebApi(baseUrl, "windows")

outputFolder <- "G:/aesiIncidenceCorrection"
if(!file.exists(outputFolder)){
  dir.create(outputFolder)
}

cohortDatabaseSchema <- "scratch_jweave17"
cohortTable <- "ir_correction"

cohortRefFile <- system.file("settings", "cohortRef.csv", package = "AesiIncidenceCorrection")
cohortRef <- readr::read_csv(cohortRefFile, show_col_types = FALSE)[1:13, ]

targetCohortIds <- 12100
outcomeCohortIds <- cohortRef$cohortId
validationCohortIds <- c(cohortRef$xSpecId, cohortRef$prevId)
cohortIds <- c(targetCohortIds, outcomeCohortIds, validationCohortIds)
cohortIds <- cohortIds[!is.na(cohortIds)]

# database specifications ======================================================

databaseNames <- c(
  "cdm_optum_ehr_v2447",
  "cdm_optum_extended_dod_v2434",
  "cdm_truven_ccae_v2435",
  "cdm_truven_mdcd_v2359",
  "cdm_truven_mdcr_v2433",
  "cdm_cprd_v2358",
  "cdm_ims_germany_v2352",
  "cdm_jmdc_v2432"
)

cdmSources <- ROhdsiWebApi::getCdmSources(baseUrl)
cdmSources <- cdmSources[cdmSources$sourceName != "Default Vocabulary", ]
cdmSources$databaseName <- substr(cdmSources$sourceKey, 5, nchar(cdmSources$sourceKey)-6)
cdmSources$version <- substr(cdmSources$sourceKey, nchar(cdmSources$sourceKey)-3, nchar(cdmSources$sourceKey))
cdmSources <- cdmSources[!is.na(cdmSources$cdmDatabaseSchema), ]
cdmSources <- cdmSources %>% dplyr::filter(.data$cdmDatabaseSchema %in% databaseNames)

cdmSources$order <- match(cdmSources$sourceKey, databaseNames)
cdmSources <- cdmSources[order(cdmSources$order), ]
cdmSources$order <- NULL
# cdmSources <- cdmSources[2:8, ]
databaseIds <- cdmSources$databaseName


for (i in 1:nrow(cdmSources)) { # i = 4

  ROhdsiWebApi::authorizeWebApi(baseUrl, "windows")

  cdmDatabaseSchema <- cdmSources$cdmDatabaseSchema[i]
  databaseId <- cdmSources$databaseName[i]

  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = keyring::key_get("DBMS"),
    server = paste0(keyring::key_get("OHDA_SERVER"), databaseId),
    extraSettings = keyring::key_get("EXTRA_SETTINGS"),
    port = keyring::key_get("port"),
    user = keyring::key_get("OHDA_USER"),
    password = keyring::key_get("OHDA_PASSWORD")
  )

  AesiIncidenceCorrection::execute(
    connectionDetails = connectionDetails,
    baseUrl = baseUrl,
    outputFolder = outputFolder,
    cohortRef = cohortRef,
    databaseId = databaseId,
    cohortIds = cohortIds,
    targetCohortIds = targetCohortIds,
    outcomeCohortIds = outcomeCohortIds,
    validationCohortIds = NULL,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortTable = cohortTable,
    createCohortTable = FALSE,
    createCohorts = FALSE,
    runOutcomeValidation = TRUE,
    runIncidenceAnalysis = FALSE,
    runIncidenceCorrection = FALSE,
    evaluateIrCorrection = FALSE
  )
}


















