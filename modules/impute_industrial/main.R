# Industrial Module

## Load the library
library(faosws)
library(faoswsUtil)
library(faoswsFlag)
library(data.table)
library(magrittr)
library(dplyr)

R_SWS_SHARE_PATH = "//hqlprsws1.hq.un.fao.org/sws_r_share"

if(CheckDebug()){

  message("Not on server, so setting up environment...")

  library(faoswsModules)
  SETTINGS <- ReadSettings("modules/impute_industrial/sws.yml")

  # If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH <- SETTINGS[["share"]]

  # Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])

  # Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])

}

files = dir("modules/impute_industrial/R", full.names = TRUE)
sapply(files, source)

## Extracting data from USDA domain/dataset
yearRange <- as.character(1961:2015)
vegetableOilsDataForIndUses <- getIndustrialData(yearRange)

industrialUsesData <- splitIndustrialData(vegetableOilsDataForIndUses)
industrialUsesData[, measuredElement:= "5165"]
industrialUsesData[group == "official", flagObservationStatus := ""]
industrialUsesData[group == "official", flagMethod:= "p"]
industrialUsesData[group == "estimated", flagObservationStatus := "I"]
industrialUsesData[group == "estimated", flagMethod := "e"]

industrialUsesData[, group := NULL]


setcolorder(industrialUsesData, c("timePointYears", "geographicAreaM49",
                                  "measuredItemCPC", "measuredElement", "Value",
                                  "flagObservationStatus", "flagMethod"))
## Save data
stats <- SaveData(domain = "agriculture", dataset = "aproduction",
                 data = industrialUsesData)

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")
