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

# 51 production (Mt)
# 61 imports (Mt)
# 71 stock variation (Mt) 
# 91 exports (Mt)  
# 101 feed (Mt)
# 131 processed (Mt)
# 141 food (Mt)
# 151 other util

elements = c("51", "61", "71", "91", "101", "131", "141", "151")

# As we want to analyse just the results for biodiesel, let's pull just the 
# commodities used to produce it. The names of the commodities are: "Oil palm fruit",
# "Palm oil", "Oil of Palm Kernel", "Cake of  Palm kernel", "Rapeseed or canola oil, crude",
# "Cake of  Rapeseed", "Soya bean oil", "Cake of  Soya beans", "Sunflower-seed oil, crude",
# "Cake of  Sunflower seed"

items = c("254", "257", "258", "259", "271", "272", "237", "238", "268", "269")

key = DatasetKey(domain = "faostat_one", dataset = "FS1_SUA_UPD", dimensions = list(
  Dimension(name = "geographicAreaFS", keys = GetCodeList("faostat_one", "FS1_SUA_UPD", "geographicAreaFS")[, code]),
  Dimension(name = "measuredElementFS", keys = elements),
  Dimension(name = "timePointYears", keys = as.character(2011)),
  Dimension(name = "measuredItemFS", keys = items))
)
data = GetData(key)

# dcast.data.table
data <- dcast.data.table(data,
                         geographicAreaFS + measuredItemFS + timePointYears ~ measuredElementFS,
                         value.var = "Value")


setnames(data, old=c("101", "131", "141", "151", "51", "61", "71", "91"),
         new=c("feed", "processed", "food", "otherUtil", "production", "imports", "stockVariation", "exports"))


data[, geographicAreaM49 := fs2m49(as.character(geographicAreaFS))]

data[, measuredItemFS := formatC(as.numeric(measuredItemFS), width = 4,
                                 flag = "0")]
data[, measuredItemCPC := fcl2cpc(as.character(measuredItemFS))]
data

# names of the commodities and countries
data <- nameData("agriculture", "aproduction", data)

data[grepl("palm", tolower(measuredItemCPC_description))][, .N, c("measuredItemCPC_description", "measuredItemFS")]
data[grepl("palm", tolower(measuredItemCPC_description)), commodity := "Palm Fruit"]


data[grepl("rapeseed", tolower(measuredItemCPC_description))][, .N, c("measuredItemCPC_description", "measuredItemFS")]
data[grepl("rapeseed", tolower(measuredItemCPC_description)), commodity := "Rapeseed"]


data[grepl("soya", tolower(measuredItemCPC_description))][, .N, c("measuredItemCPC_description", "measuredItemFS")]
data[grepl("soya", tolower(measuredItemCPC_description)), commodity := "Soybeans"]

data[grepl("sunflower", tolower(measuredItemCPC_description))][, .N, c("measuredItemCPC_description", "measuredItemFS")]
data[grepl("sunflower", tolower(measuredItemCPC_description)), commodity := "Sunflower"]


data[, .N, commodity]
data[geographicAreaM49 == "250"]

# no NA's
for(cname in c("feed", "processed", "food", "otherUtil", "production", 
               "imports", "stockVariation", "exports")){
  data[is.na(get(cname)), c(cname) := 0]
}

data[, prodNetTrade := production + imports - exports - stockVariation]

data[geographicAreaM49 == "250" & commodity == "Soybeans"]

data[, list(prodNetTradeTotal = sum(prodNetTrade)),
     by=list(geographicAreaM49_description, geographicAreaM49, commodity)]

data[, geographicAreaM49:= as.factor(geographicAreaM49)]

# let's read the table with the flag to EU28
setwd("C:/Users/caetano/Documents/Github/faoswsIndustrial/sandbox")
europeanCountryCode = fread("europeanCountryCode.csv")
europeanCountryCode[, geographicAreaM49 :=  as.factor(countrycode(countryName, "country.name", "un"))]
europeanCountryCode = europeanCountryCode[flagEU28 == 1]
europeanCountryCode

# merge data with the the flag of EU28

class(data$geographicAreaM49)
class(europeanCountryCode$geographicAreaM49)

data = merge(data, europeanCountryCode[, c("geographicAreaM49", "flagEU28"), with = F],
      by = "geographicAreaM49", all.x=T)

data[is.na(flagEU28), flagEU28 := 0]

data[, flagEU28 := as.factor(flagEU28)]

data[, .N, flagEU28]

# by country, flagEU28 and commodity
countryByCommodity = data[, list(prodNetTrade = sum(prodNetTrade)),
     by=list(geographicAreaM49, geographicAreaM49_description, flagEU28, commodity)]

countryByCommodity[geographicAreaM49 == "250"]

countryByCommodity[, biofuelStructByCountry := prodNetTrade/sum(prodNetTrade), 
     by = geographicAreaM49]

# by flagEU28 and commodity
flagEU28Commodity = data[, list(prodNetTrade = sum(prodNetTrade)),
     by=list(flagEU28, commodity)]

flagEU28Commodity[, biofuelStructEU28 := prodNetTrade/sum(prodNetTrade), 
     by = flagEU28]


# Now, we have to merge the table countryByCommodity with  the table 
# euroStatBiofuel.

keys = c("geographicAreaM49", "commodity")

setnames(euroStatBiofuel, "m49", "geographicAreaM49")
class(euroStatBiofuel$geographicAreaM49)
euroStatBiofuel[, geographicAreaM49 := as.factor(geographicAreaM49)]
class(countryByCommodity$geographicAreaM49)

euroStatBiofuel = merge(euroStatBiofuel, countryByCommodity[, c("geographicAreaM49", "commodity",
                                              "biofuelStructByCountry"), with = F],
      by=keys, all.x=T)

# Now, we can apply the same rules that we applied before.

euroStatBiofuel[geographicAreaM49 == "250" & year == 2003 & biofuel == "biodiesel"]

euroStatBiofuel[biofuel == "biodiesel", valueBiodiselByCommodityTonnes := value * biofuelStructByCountry * 1000]

# we need to convert from litres to tonnes

## Now we can calculate how many tonnes of each commodity was used
euroStatBiofuel[biofuel == "biodiesel", prodBiodiselByCommodity := valueBiodiselByCommodityTonnes/conversion_litres_tonne]
euroStatBiofuel


## Comparing data

data[grepl("palm", tolower(measuredItemCPC_description))][, .N, measuredItemCPC_description]

data[measuredItemCPC_description == "Oil palm fruit"]

# UK
data[measuredItemCPC_description == "Palm oil" & m49 == 826 & timePointYears == 2005]

euroStatBiofuel[commodity == "Palm Fruit" & m49 == 826 & year == 2005]
