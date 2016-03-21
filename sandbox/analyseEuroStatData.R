library(data.table)
library(countrycode)

## eurostat data: biodiesel

setwd("~/Github/faoswsIndustrial/Data")

# ratio: the piece of information about "ethanol" came from ebb and for "biodiesel" 
# from one website (http://www.paulchefurka.ca/Biofuels.html).
ratioAdditionalSupplyBiofuelDemand = fread("ratioAdditionalSupplyBiofuelDemand.csv")
ratioAdditionalSupplyBiofuelDemand

# structure : this dataset came from ebb
biofuelStructure = fread("biofuelStructure.csv")
biofuelStructure

# Biodiesel from eurostat
eurostatBiodieselTOE = fread("eurostatBiodieselTOE.csv")
head(eurostatBiodieselTOE)

eurostatBiodieselTOE[, m49 := countrycode(country, "country.name", "un")]
eurostatBiodieselTOE = eurostatBiodieselTOE[!is.na(m49)]

eurostatBiodieselTOE

# melt
eurostatBiodieselTOE = melt.data.table(eurostatBiodieselTOE, 
                                       id.vars = c("country", "m49"), 
                                       measure.vars = paste0("year", 2003:2014))

eurostatBiodieselTOE = eurostatBiodieselTOE[!(value == ":")]
eurostatBiodieselTOE[, year := substr(variable, 5, 8)]
eurostatBiodieselTOE[, c("variable") := NULL]
setcolorder(eurostatBiodieselTOE, c("country", "m49", "year", "value"))
eurostatBiodieselTOE[, biofuel := "biodiesel"]

# Biogasoline from eurostat
eurostatBiogasolineTOE = fread("eurostatBiogasolineTOE.csv")
head(eurostatBiogasolineTOE)

eurostatBiogasolineTOE[, m49 := countrycode(country, "country.name", "un")]
eurostatBiogasolineTOE = eurostatBiogasolineTOE[!is.na(m49)]

eurostatBiogasolineTOE

# melt
eurostatBiogasolineTOE = melt.data.table(eurostatBiogasolineTOE, 
                                       id.vars = c("country", "m49"), 
                                       measure.vars = paste0("year", 2003:2014))

eurostatBiogasolineTOE = eurostatBiogasolineTOE[!(value == ":")]
eurostatBiogasolineTOE[, year := substr(variable, 5, 8)]
eurostatBiogasolineTOE[, c("variable") := NULL]
setcolorder(eurostatBiogasolineTOE, c("country", "m49", "year", "value"))
eurostatBiogasolineTOE[, biofuel := "bioethanol"]

## join both biodiesel and biogasoline data

euroStatBiofuel = rbind(eurostatBiodieselTOE, eurostatBiogasolineTOE)

# merge with biofuelStructure
euroStatBiofuel = merge(euroStatBiofuel, biofuelStructure[, c("commodity",
                                                              "refScenario", 
                                                              "biofuel", 
                                                              "densityG_ml"),
                                                          with = F], by = "biofuel",
                        all.x=T, allow.cartesian=TRUE)

euroStatBiofuel[, value := as.numeric(value)]
euroStatBiofuel[, valueByCommodityTonnes := value * refScenario * 1000]

# merge with ratio table
sapply(euroStatBiofuel, class)
sapply(ratioAdditionalSupplyBiofuelDemand, class)

# merge eurostatBiodieselTOE with the conversion factors
euroStatBiofuel = merge(euroStatBiofuel, ratioAdditionalSupplyBiofuelDemand[, c("commodity",
                                        "conversion_efficiency_litres_tonne"),
                                        with = F], by = "commodity", all.x=T)

# we need to convert from litres to tonnes

euroStatBiofuel[, conversion_litres_tonne := (densityG_ml * conversion_efficiency_litres_tonne)/1000]

## Now we can calculate how many tonnes of each commodity was used
euroStatBiofuel[, prodByCommodity := valueByCommodityTonnes/conversion_litres_tonne]
euroStatBiofuel
euroStatBiofuel[m49 == 250 & year == 2003]



##

library(data.table)
library(faosws)
library(dplyr)
library(faoswsUtil)

## set up for the test environment and parameters
R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE <- Sys.getenv("R_DEBUG_MODE")
overwritableFlags = c("M", "I")

if(!exists("DEBUG_MODE") || DEBUG_MODE == "") {
  if(Sys.info()[7] == "josh"){ # Josh's work computer
    R_SWS_SHARE_PATH <- "/media/hqlprsws1_qa/"
    SetClientFiles(dir = "~/R certificate files/QA/")
    files = dir("~/Documents/Github/faoswsFood/R",
                full.names = TRUE)
    token = "557f0e65-5f84-43b0-a021-fe3bf3f02316"
  } else if(Sys.info()[7] == "caetano"){ # bruno's work computer
    SetClientFiles(dir = "~/.R/QA/")
    R_SWS_SHARE_PATH = "//hqlprsws1.hq.un.fao.org/sws_r_share"
    files = dir("~/Github/faoswsFood/R", full.names = TRUE)
    token = "66a36f31-1a29-4a49-8626-ae62117c251a"
  } else {
    stop("User not yet implemented!")
  }  
  
  GetTestEnvironment(
    baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
    token = token
  )
  #R_SWS_SHARE_PATH <- "/media/hqlprsws1_qa/"
  sapply(files, source)
}

if(swsContext.computationParams$yearToProcess <= 2011)
  stop("This module was designed for imputation on years after 2011 only!")


## This code below is pulling data from the old system for "Other util"

country <- GetCodeList("faostat_one", "FS1_SUA_UPD", "geographicAreaFS")
element <- GetCodeList("faostat_one", "FS1_SUA_UPD", dimension = "measuredElementFS")
item <- GetCodeList("faostat_one", "FS1_SUA_UPD", dimension = "measuredItemFS")
yearRange <- as.character(2003:2010)

countryDim1 <- Dimension(name = "geographicAreaFS", 
                         keys = country[, code])

elementDim2 <- Dimension(name = "measuredElementFS", 
                         keys = "151")

itemDim3 <- Dimension(name = "measuredItemFS",
                      keys = item[, code])

timePointYearsDim4 <- Dimension(name = "timePointYears",
                                keys = yearRange)

dataKey <- DatasetKey(domain = "faostat_one", dataset = "FS1_SUA_UPD", 
                      dimensions = list(countryDim1, elementDim2, itemDim3, 
                                        timePointYearsDim4))

data <- GetData(dataKey, flags = FALSE)
data[, m49 := fs2m49(geographicAreaFS)]
data[, measuredItemFS := formatC(as.numeric(measuredItemFS), width = 4,
                                 flag = "0")]
data[, measuredItemCPC := fcl2cpc(as.character(measuredItemFS))]
data <- nameData("agriculture", "aproduction", data)

## Comparing data

data[grepl("palm", tolower(measuredItemCPC_description))][, .N, measuredItemCPC_description]

data[measuredItemCPC_description == "Oil palm fruit"]

# UK
data[measuredItemCPC_description == "Palm oil" & m49 == 826 & timePointYears == 2005]

euroStatBiofuel[commodity == "Palm Fruit" & m49 == 826 & year == 2005]
