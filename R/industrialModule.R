# Industrial Module

## Load the library
library(faosws)
library(faoswsUtil)
library(faoswsFlag)
library(data.table)
library(magrittr)
library(igraph)
library(dplyr)

## set working diretory 
workingDir = "~/Github/faoswsIndustrial/"

## Read R.Data files
load(file = paste0(workingDir, "Data/vegetableOilsData.RData", na.strings = ""))
load(file = paste0(workingDir, "Data/nutrientData.RData"))

vegetableOilsData <- data.table(vegetableOilsData)
vegetableOilsData$Country_Code <- as.character(vegetableOilsData$Country_Code)
vegetableOilsData$Country_Code[which(is.na(vegetableOilsData$Country_Code) == T)] <- "NA"


## Read functions
source(paste0(workingDir, "R/getBioFuelData.R"))
source(paste0(workingDir, "R/getCPCTreeItem.R"))
source(paste0(workingDir, "R/getCountryCodeSUA.R"))
source(paste0(workingDir, "R/getItemCommSUA.R"))


DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
  token = "41558a20-c419-4821-8288-2dc7ccbc5ecf"
  GetTestEnvironment("https://hqlqasws1.hq.un.fao.org:8181/sws", token)
  
}

## Data manipulation: filter
vegetableOilsDataForIndUses <- vegetableOilsData[Attribute_ID == 140]

## Countries codes
funcCountryCodes <- lapply(vegetableOilsDataForIndUses$Country_Code, 
                                    getCountryCodeSUA)

## Commodities codes
funcComCodes <- lapply(vegetableOilsDataForIndUses$Commodity_Description, 
                       getItemCommSUA)

## Cbind data
vegetableOilsDataForIndUses <- cbind(vegetableOilsDataForIndUses,
                                     geographicAreaM49 = do.call("rbind", funcCountryCodes),
                                     measuredItemCPC = do.call("rbind", funcComCodes))

## Data table

vegetableOilsDataForIndUses <- data.table(vegetableOilsDataForIndUses)

## Selecting some Variables

vegetableOilsDataForIndUses <- vegetableOilsDataForIndUses[, list(measuredItemCPC.V1, Calendar_Year, Value), 
                                                           by = list(geographicAreaM49.V1)]


setnames(vegetableOilsDataForIndUses, c("geographicAreaM49", "measuredItemCPC", "timePointYears", "Value"))

vegetableOilsDataForIndUses$geographicAreaM49 = as.character(vegetableOilsDataForIndUses$geographicAreaM49)
vegetableOilsDataForIndUses$measuredItemCPC = as.character(vegetableOilsDataForIndUses$measuredItemCPC)
vegetableOilsDataForIndUses$timePointYears = as.character(vegetableOilsDataForIndUses$timePointYears)

## Pull agricFeedStuffsForBioFuelData
allCPCItem = getCPCTreeItem()
agricFeedStuffsForBioFuelData <- getBioFuelData()

lapply(agricFeedStuffsForBioFuelData, class)

agricFeedStuffsForBioFuelData$geographicAreaM49 <- as.character(agricFeedStuffsForBioFuelData$geographicAreaM49)
agricFeedStuffsForBioFuelData$measuredItemCPC <- as.character(agricFeedStuffsForBioFuelData$measuredItemCPC)
agricFeedStuffsForBioFuelData$timePointYears <- as.character(agricFeedStuffsForBioFuelData$timePointYears)

## Merge the two data set

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
