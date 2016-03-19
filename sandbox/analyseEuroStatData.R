
## eurostat data: biodiesel

setwd("C:\\Users\\caetano\\Desktop\\biofuel data\\biodiesel\\eurostat")

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
