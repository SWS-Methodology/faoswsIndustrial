# Industrial Module

## Load the library
library(faosws)
library(faoswsUtil)
library(faoswsFlag)
library(data.table)
library(magrittr)
library(igraph)
library(dplyr)


DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
  token = "41558a20-c419-4821-8288-2dc7ccbc5ecf"
  GetTestEnvironment("https://hqlqasws1.hq.un.fao.org:8181/sws", token)
  
}

## Extracting data from USDA domain/dataset

countryCodeInd <- GetCodeList("usda", "usda_psd_nv", "geographicAreaM49nv")
elementInd <- GetCodeList("usda", "usda_psd_nv", dimension = "measuredElementPsd")
itemInd <- GetCodeList("usda", "usda_psd_nv", dimension = "measuredItemPsd")
yearRange <- as.character(1961:2015)

countryDim1 <- Dimension(name = "geographicAreaM49nv", 
                         keys = countryCodeInd[, code])

elementDim2 <- Dimension(name = "measuredElementPsd", 
                         keys = "140.08")

itemDim3 <- Dimension(name = "measuredItemPsd",
                      keys = itemInd[, code])

timePointYearsDim4 <- Dimension(name = "timePointYears",
                                keys = yearRange)

dataKey <- DatasetKey(domain = "usda", dataset = "usda_psd_nv", 
                      dimensions = list(countryDim1, elementDim2, itemDim3, 
                                        timePointYearsDim4))

vegetableOilsDataForIndUses <- GetData(dataKey, flags = FALSE)
vegetableOilsDataForIndUses[, measuredElementPsd := NULL]

setnames(vegetableOilsDataForIndUses, old = c("geographicAreaM49nv", "measuredItemPsd", "timePointYears", "Value"),
         new = c("geographicAreaM49", "measuredItemCPC", "timePointYears", "Value"))

## Pull agricFeedStuffsForBioFuelData
allCPCItem = getCPCTreeItem()
agricFeedStuffsForBioFuelData <- getBioFuelData()


agricFeedStuffsForBioFuelData$geographicAreaM49 <- as.character(agricFeedStuffsForBioFuelData$geographicAreaM49)
agricFeedStuffsForBioFuelData$measuredItemCPC <- as.character(agricFeedStuffsForBioFuelData$measuredItemCPC)
agricFeedStuffsForBioFuelData$timePointYears <- as.character(agricFeedStuffsForBioFuelData$timePointYears)

## Merge the two datasets

industrialUsesData <- merge(agricFeedStuffsForBioFuelData, vegetableOilsDataForIndUses,
                            by = c("geographicAreaM49", "measuredItemCPC", "timePointYears"),
                            all = TRUE)


industrialUsesData$Value_measuredElement_5150 <- as.numeric(industrialUsesData$Value_measuredElement_5150)
industrialUsesData$Value_measuredElement_5150[which(is.na(industrialUsesData$Value_measuredElement_5150))] <- 0
industrialUsesData$Value[which(is.na(industrialUsesData$Value))] <- 0

# Value_measuredElement_5150 is in tonns
# Value is in 1000MT

industrialUsesData[, Value_measuredElement_ind := Value_measuredElement_5150 + Value * 1000]


# Save the data

industrialUsesData[, c("Value", "Value_measuredElement_5150") := NULL]

industrialUsesData[, industrialElement:= "5195"]

setcolorder(industrialUsesData,
            c("timePointYears", "geographicAreaM49", "measuredItemCPC", "industrialElement",
              "Value_measuredElement_ind", "flagObservationStatus_measuredElement_5150", 
              "flagMethod_measuredElement_5150"))


setnames(industrialUsesData,
         old = c("timePointYears", "geographicAreaM49", "measuredItemCPC", "industrialElement",
                 "Value_measuredElement_ind", "flagObservationStatus_measuredElement_5150", 
                 "flagMethod_measuredElement_5150"),
         new = c("timePointYears", "geographicAreaM49", "measuredItemCPC", "measuredElement",
                 "Value", "flagObservationStatus", "flagMethod"))


industrialUsesData[, flagObservationStatus:= "I"]
industrialUsesData[, flagMethod:= "e"]

## Save data
stats = SaveData(domain = "agriculture", dataset = "agriculture", data = industrialUsesData)

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")
