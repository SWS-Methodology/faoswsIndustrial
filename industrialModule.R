# Industrial Module

## Load the library
library(faosws)
library(faoswsUtil)
library(faoswsFlag)
library(data.table)
library(magrittr)
library(dplyr)


if(CheckDebug()){
  token = "41558a20-c419-4821-8288-2dc7ccbc5ecf"
  GetTestEnvironment("https://hqlqasws1.hq.un.fao.org:8181/sws", token)
  R_SWS_SHARE_PATH = "//hqlprsws1.hq.un.fao.org/sws_r_share"
  files = dir("~/Github/faoswsIndustrial/R", full.names = TRUE)
  sapply(files, source)
}

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
