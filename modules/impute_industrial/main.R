# Industrial Module

## Load the library
library(faosws)
library(faoswsUtil)
library(faoswsFlag)
library(data.table)
library(magrittr)
library(dplyr)
# library(faoswsIndustrial)

# R_SWS_SHARE_PATH = "//hqlprsws1.hq.un.fao.org/sws_r_share"

R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")


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

files = dir("R", full.names = TRUE)
sapply(files, source)

###################################################################################################
## This chunck of code should be delete after Daniele moves the element 5153 to 5165 from 1961-2011
area <- GetCodeList("agriculture", "aproduction", "geographicAreaM49")[, code]
items <- GetCodeList("agriculture", "aproduction", "measuredItemCPC")[, code]
elements <- c("5153", "5165")
years <- as.integer(unique(GetCodeList("agriculture", "aproduction", "timePointYears")[, code]))
years <- as.character(years[years >= 1961L])

raw_data_key <- DatasetKey(domain = "agriculture",
                           dataset = "aproduction",
                           dimensions = list(
                             geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = area),
                             measuredElement = Dimension(name = "measuredElement", keys = elements),
                             measuredItemCPC = Dimension(name = "measuredItemCPC", keys = items),
                             timePointYears = Dimension(name = "timePointYears", keys = years)
                           )
)

raw_data <- GetData(raw_data_key)
raw_data[timePointYears < 2012, measuredElement := "5165"]
raw_data <- raw_data[!(measuredElement == 5153)]

## Let's exclude 0 (M, -)
raw_data <- raw_data[!(flagObservationStatus == "M" & flagMethod == "-" & Value == 0)]

###################################################################################################

## Extracting data from USDA domain/dataset
yearRange <- as.character(1961:2016)
vegetableOilsDataForIndUses <- getIndustrialData(yearRange)

industrialUsesData <- splitIndustrialData(vegetableOilsDataForIndUses)
industrialUsesData[, measuredElement:= "5165"]
industrialUsesData[group == "official", flagObservationStatus := ""]
industrialUsesData[group == "official", flagMethod:= "p"]
industrialUsesData[group == "estimated", flagObservationStatus := "I"]
industrialUsesData[group == "estimated", flagMethod := "e"]

industrialUsesData[, group := NULL]


setcolorder(industrialUsesData, c("geographicAreaM49", "measuredElement",
                                  "measuredItemCPC", "timePointYears", "Value",
                                  "flagObservationStatus", "flagMethod"))

setnames(industrialUsesData, "Value", "value_usda")
setnames(industrialUsesData, "flagObservationStatus", "flagObservationStatus_usda")
setnames(industrialUsesData, "flagMethod", "flagMethod_usda")

## We will merge the data coming from the questionnaire with the data from USDA.
## If there's data for the same cpc in both datasets, we will use data from quest.

merge_data <- merge(raw_data, industrialUsesData,
  by = c("geographicAreaM49", "measuredElement", "measuredItemCPC", "timePointYears"),
  all = T)

merge_data <- merge_data[!is.na(geographicAreaM49)]
merge_data[is.na(Value), `:=` (Value = value_usda,
                               flagObservationStatus = flagObservationStatus_usda,
                               flagMethod = flagMethod_usda)]

# merge_data[geographicAreaM49 == 120 & measuredItemCPC == "21691.14" & timePointYears == 2012]
merge_data[, c("value_usda", "flagObservationStatus_usda", "flagMethod_usda") := NULL]

## Save data
stats <- SaveData(domain = "industrialUse", dataset = "industrialusedata",
                 data = merge_data)

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")
