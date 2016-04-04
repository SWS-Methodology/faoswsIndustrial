
## This R script does comparisons between sws dataset
## and dataset used in Industrial Module.


## Load required functions
library(faosws)
library(dplyr)
library(reshape2)
library(data.table)
library(faoswsUtil)

DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
  token = "41558a20-c419-4821-8288-2dc7ccbc5ecf"
  GetTestEnvironment("https://hqlqasws1.hq.un.fao.org:8181/sws",token)
  
}

## Pull sws dataset

countryCodeUSDA <- GetCodeList("usda", "usda_psd_nv", "geographicAreaM49nv")
elementUSDA <- GetCodeList("usda", "usda_psd_nv", dimension = "measuredElementPsd")
itemUSDA <- GetCodeList("usda", "usda_psd_nv", dimension = "measuredItemPsd")
yearRange <- as.character(1961:2015)


countryDim1 <- Dimension(name = "geographicAreaM49nv", 
                         keys = countryCodeUSDA[, code])

elementUSDADim2 <- Dimension(name = "measuredElementPsd", 
                             keys = "140.08")

itemUSDADim3 <- Dimension(name = "measuredItemPsd",
                          keys = itemUSDA[, code])

timePointYearsDim4 <- Dimension(name = "timePointYears",
                                keys = yearRange)

USDAKey <- DatasetKey(domain = "usda", dataset = "usda_psd_nv", 
                      dimensions = list(countryDim1, elementUSDADim2, itemUSDADim3, 
                                        timePointYearsDim4))

## Pull the USDA data from the SWS

swsUSDAData <- GetData(USDAKey, flags = FALSE)
head(swsUSDAData)

summary(as.numeric(swsUSDAData$timePointYears))

freqYearUSDA <- swsUSDAData[, .N, timePointYears ]
freqYearUSDA <- freqYearUSDA[order(-freqYearUSDA$timePointYears), ]

validationSwsUSDA <- swsUSDAData[, list(total = sum(Value)),
                                by=list(geographicAreaM49nv, timePointYears)]

validationSwsUSDA[geographicAreaM49nv == 36 & timePointYears == 2014]

dataPerYearSws <- validationSwsUSDA[, list(totalContriesSWS = length(unique(geographicAreaM49nv)),
                                               totalValueSWS = sum(total),
                                           valuePerCountrySWS = sum(total)/length(unique(geographicAreaM49nv))),
                                        by=timePointYears]


dataPerYearSws <- dataPerYearSws[order(-dataPerYearSws$timePointYears)]

setnames(dataPerYearSws, old = c("timePointYears", "totalContriesSWS", "totalValueSWS", "valuePerCountrySWS"),
         new = c("Market_Year", "totalContriesSWS", "totalValueSWS", "valuePerCountrySWS"))

dataPerYearSws$Market_Year <- as.integer(dataPerYearSws$Market_Year)

## Pull Natalia's dataset used in Industrial Module

workingDir = "~/Github/privateFAO/OrangeBook/"

load(file = paste0(workingDir,"vegetableOilsData.RData"))
attach(vegetableOilsData)
dim(vegetableOilsData)

#load(file = paste0(workingDir,"nutrientData.RData"))
## attach(nutrientData)
## dim(nutrientData)

vegetableOilsData <- data.table(vegetableOilsData)

## Filtering 
vegetableDataIndustrial <- vegetableOilsData[Attribute_Description == "Industrial Dom. Cons."]

freqYearDataIndustrial <- vegetableDataIndustrial[, .N, Market_Year ]
freqYearDataIndustrial <- freqDataIndustrial[order(-freqDataIndustrial$Market_Year), ]


validationDataIndustrial <- vegetableDataIndustrial[, list(total = sum(Value)),
                                by=list(Market_Year, Country_Name, Attribute_Description, Unit_Description)]

validationDataIndustrial[Country_Name == "Australia" & Market_Year == "2014"]


dataPerYear <- validationDataIndustrial[, list(totalContries = length(unique(Country_Name)),
                                totalValue = sum(total),
                                valuePerCountry = sum(total)/length(unique(Country_Name))),
                         by=Market_Year]


dataPerYear <- dataPerYear[order(-dataPerYear$Market_Year)]


dataMerge <- merge(dataPerYear, dataPerYearSws, by = c("Market_Year"), all.x = T)

dataMerge[, DiffValues:= totalValue - totalValueSWS]
dataMerge[, DiffCountries:= totalContries - totalContriesSWS]

## write.table(dataMerge, file = "dataMerge.csv", sep = ";", row.names = T)

## 