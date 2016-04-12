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

#source("~/Github/faoswsIndustrial/R/getBioFuelData.R")
#source("~/Github/faoswsIndustrial/R/getCPCTreeItem.R")
source("~/Github/faoswsIndustrial/R/getCountryCodeSUA.R")
source("~/Github/faoswsIndustrial/R/getItemCommSUA.R")


## Extracting data from USDA domain/dataset

countryCodeInd <- GetCodeList("usda", "usda_psd", "geographicUsda")
elementInd <- GetCodeList("usda", "usda_psd", dimension = "measuredElementPsd")
itemInd <- GetCodeList("usda", "usda_psd", dimension = "measuredItemPsd")
yearRange <- as.character(1961:2015)

countryDim1 <- Dimension(name = "geographicUsda",
                         keys = countryCodeInd[, code])

elementDim2 <- Dimension(name = "measuredElementPsd",
                         keys = "140.08")

itemDim3 <- Dimension(name = "measuredItemPsd",
                      keys = itemInd[, code])

timePointYearsDim4 <- Dimension(name = "timePointYears",
                                keys = yearRange)

dataKey <- DatasetKey(domain = "usda", dataset = "usda_psd",
                      dimensions = list(countryDim1, elementDim2, itemDim3,
                                        timePointYearsDim4))

vegetableOilsDataForIndUses <- GetData(dataKey, flags = FALSE)
vegetableOilsDataForIndUses[, measuredElementPsd := NULL]
vegetableOilsDataForIndUses[, measuredItemCPC := as.character(getItemCommSUA(measuredItemPsd))]

# Code used to split the data of european union countries from 1991 to 2015

# There are 3 Germanys on USDA ("German Democratic Republic", "Germany, Federal Republic of" and "Germany").
# Let's put all together.
vegetableOilsDataForIndUses[geographicUsda %in% c("GE", "GC"), geographicUsda := "GM"]
vegetableOilsDataForIndUses = vegetableOilsDataForIndUses[, list(Value = sum(Value)),
                                                          by=c("geographicUsda", "measuredItemCPC", "timePointYears")]

# Let's split the data for E2 (EU-15) and for E4 (EU-28) but
# first, we need to calculate the proportion by country
memberStatesEU = fread("Data/memberStatesEU.csv")
europeanCountries = memberStatesEU[, .N, usdaCode][, usdaCode]

# Belgium and Luxembourg provided figures together to USDA. So, basically we need to split the data into these countries.
# If you consider the "Production Capacity" from European Biodiesel Industry, they have values to these countries separated, so
# we just calculate the percent of this piece of information. So we'll consider that 98% of the industrial utilization came from
# Belgium and 2% from Luxembourg.
# http://www.ebb-eu.org/stats.php#

dataEBB = data.table(country = c("Belgium", "Luxembourg"),
                     timePointYears = 2013,
                     prodCapacity = c(991, 20))
dataEBB[, percent := prodCapacity/sum(prodCapacity)]

# we need to split the propotion for the previous years to Belgium and Luxembourg
luxembourg = vegetableOilsDataForIndUses[geographicUsda == "BE"]
luxembourg[, geographicUsda := "LU"]
luxembourg[, Value := Value * 0.01978239]

vegetableOilsDataForIndUses[geographicUsda == "BE", Value := Value * 0.98021761]

# Put "Luxembourg" on the data set
vegetableOilsDataForIndUses = rbind(vegetableOilsDataForIndUses, luxembourg)

# how many european countries are there per year?
vegetableOilsDataForIndUses[geographicUsda %in% europeanCountries, list(length(unique(geographicUsda))),
                            by=timePointYears]

# Let's use the year 1990 to calculate the percent of each country/commodity in the industrial utilization

# Only 2 countries (Czech Republic, Slovakia) that don't have data in 1990. So we need to extrapolate figures for them.
# So we can use the simple linear regression model in order to do that to this year.

# Czech Republic (EZ)
countryEZ = vegetableOilsDataForIndUses[geographicUsda == "EZ"]
fitEZ = lm(data=countryEZ[Value > 0 & measuredItemCPC == "21641.01"],
           Value ~ as.numeric(timePointYears))
fitEZ$coefficients[1] + fitEZ$coefficients[2] * 1990

czechRepublic = data.table(geographicUsda = rep("EZ", 2),
                           measuredItemCPC = c("21641.01", "21611"),
                           timePointYears = rep(1990, 2),
                           Value = c(80.11429, 1))

# Slovakia (LO)
countryLO = vegetableOilsDataForIndUses[geographicUsda == "LO"]
fitLO = lm(data=countryLO[Value > 0 & measuredItemCPC == "21641.01"],
           Value ~ as.numeric(timePointYears))
fitLO$coefficients[1] + fitLO$coefficients[2] * 1990

slovakia = data.table(geographicUsda = "LO",
                      measuredItemCPC = "21641.01",
                      timePointYears = 1990,
                      Value = 6.171429)

# Now we need to aggregate the values to Czech Republic (EZ) and Slovakia (LO)
europeanCountry1990 = vegetableOilsDataForIndUses[geographicUsda %in% europeanCountries &
                                                    timePointYears == 1990]

# So here we are just using rbind to aggregate the datasets to these 2 countries
europeanCountry1990 = rbind(europeanCountry1990, czechRepublic, slovakia)
europeanCountry1990[, .N, geographicUsda]

# We are using the year "1990" to compute the % of industrial utilization to each european country
# according to when it became a member state. This % will be calculated to have information by european country
# from 1991 to 2015

# Calculate the % from 1991 to 1994
tab1991to1994 = europeanCountry1990[geographicUsda %in% unique(memberStatesEU[accession %in% c("before1990")][, usdaCode])]

tab1991to1994[, percent := Value/sum(Value),
              by="measuredItemCPC"]

tab1991to1994[, timePointYears := NULL]

percent1991to1994 = rbind(tab1991to1994, tab1991to1994, tab1991to1994, tab1991to1994)
percent1991to1994 = cbind(percent1991to1994, timePointYears = rep(1991:1994,
                                                                  each = nrow(tab1991to1994)))

# Calculate the % from 1995 to 2003
tab1995to2003 = europeanCountry1990[geographicUsda %in% unique(memberStatesEU[accession %in% c("before1990", "1995")][, usdaCode])]

tab1995to2003[, percent := Value/sum(Value),
              by="measuredItemCPC"]

tab1995to2003[, timePointYears := NULL]

percent1995to2003 = rbind(tab1995to2003, tab1995to2003, tab1995to2003, tab1995to2003, tab1995to2003,
                          tab1995to2003, tab1995to2003, tab1995to2003, tab1995to2003)
percent1995to2003 = cbind(percent1995to2003, timePointYears = rep(1995:2003, each = nrow(tab1995to2003)))

# Calculate the % from 2004 to 2006
tab2004to2006 = europeanCountry1990[geographicUsda %in% unique(memberStatesEU[accession %in% c("before1990", "1995", "2004")][, usdaCode])]

tab2004to2006[, percent := Value/sum(Value),
              by="measuredItemCPC"]

tab2004to2006[, timePointYears := NULL]

percent2004to2006 = rbind(tab2004to2006, tab2004to2006, tab2004to2006)
percent2004to2006 = cbind(percent2004to2006,
                          timePointYears = rep(2004:2006, each = nrow(tab2004to2006)))

# Calculate the % from 2007 to 2012
tab2007to2012 = europeanCountry1990[geographicUsda %in% unique(memberStatesEU[accession %in% c("before1990", "1995", "2004", "2007")][, usdaCode])]

tab2007to2012[, percent := Value/sum(Value),
              by="measuredItemCPC"]

tab2007to2012[, timePointYears := NULL]

percent2007to2012 = rbind(tab2007to2012, tab2007to2012, tab2007to2012,
                          tab2007to2012, tab2007to2012, tab2007to2012)
percent2007to2012 = cbind(percent2007to2012, timePointYears = rep(2007:2012,
                                                                  each = nrow(tab2007to2012)))

# Calculate the % from 2013 to 2015

tab2013to2015 = europeanCountry1990[geographicUsda %in% unique(memberStatesEU[accession %in% c("before1990", "1995", "2004", "2007", "2013")][, usdaCode])]

tab2013to2015[, percent := Value/sum(Value),
              by="measuredItemCPC"]

tab2013to2015[, timePointYears := NULL]

percent2013to2015 = rbind(tab2013to2015, tab2013to2015, tab2013to2015)
percent2013to2015 = cbind(percent2013to2015,
                          timePointYears = rep(2013:2015, each = nrow(tab2013to2015)))

# After calculate the percent for all the countries/commodities/year, we have to put together these data sets
dataPercent = rbind(percent1991to1994, percent1995to2003, percent2004to2006,
                    percent2007to2012, percent2013to2015)
dataPercent[percent == "NaN", percent := 0]
dataPercent[, Value := NULL]
dataPercent[, timePointYears := as.character(timePointYears)]
setnames(dataPercent, "geographicUsda", "usdaCode")

# Values to the European Union (E2 and E4) from 1991 to 2015
e2E4 = vegetableOilsDataForIndUses[geographicUsda %in% c("E4", "E2")]
e2E4

# Final merge
keys = c("measuredItemCPC", "timePointYears")
estimated = merge(e2E4, dataPercent, by=keys,
                  all.x=T)

estimated[, percentValue := Value * percent]

# pulling the description of the commodities
cpcDesc = GetCodeList("agriculture", "aproduction", "measuredItemCPC")
setnames(cpcDesc, "code", "measuredItemCPC")

# merge the description
estimated = merge(estimated, cpcDesc[, c("measuredItemCPC", "description"), with=F], by="measuredItemCPC")
estimated[, .N, description]

estimated[description == "Flours, meals and pellets, inedible, of fish, crustaceans, molluscs or other aquatic invertebrates",
          description := "Meal, Fish"]

estimated[description == "Flours and meals of oil seeds or oleaginous fruits, except those of mustard",
          description := "Flours and meals of oil seeds or oleaginous fruits, \n except those of mustard"]

# map by %
ggplot(data=estimated,
       aes(x=as.numeric(timePointYears), y=percent, group=description, col=description)) +
  geom_line(aes(), stat = "identity", position=position_dodge(), size=1.5) +
  #geom_point(aes(shape = description), size = 3) + scale_shape_identity() +
  facet_wrap(~ usdaCode) +
  scale_x_continuous(limits = c(1991, 2015), breaks=seq(1991, 2015, 4)) +
  scale_y_continuous(lim=c(0, 1),  breaks=seq(0, 1, .2), labels = percent) +
  theme(legend.title=element_blank()) +
  ylab('Industrial Dom.Cons (%)') + xlab('Year') +
  theme_fao(45, size = 10)

ggplot(estimated[timePointYears %in% c(1991, 1992, 1993, 1994)], aes(x = as.numeric(timePointYears), y=percent, fill=usdaCode)) +
  geom_bar(stat='identity') +
  facet_wrap(~ description) +
  #scale_x_continuous(limits = c(1991, 2015), breaks=seq(1991, 2015, 4)) +
  scale_y_continuous(lim=c(0, 1),  breaks=seq(0, 1, .2), labels = percent) +
  theme(legend.title=element_blank()) +
  ylab('Industrial Dom.Cons (%)') + xlab('Year') +
  theme_fao(45, size = 10)

estimated[, c("geographicUsda", "Value", "percent", "description") := NULL]
setnames(estimated, "percentValue", "Value")
setnames(estimated, "usdaCode", "geographicUsda")
setcolorder(estimated, c("geographicUsda", "measuredItemCPC", "timePointYears", "Value"))
estimated = estimated[!is.na(Value)]
estimated[, group := "estimated"]

# we have to exclude the data to E2 and E4 and put together the table "estimated"
vegetableOilsDataForIndUses = vegetableOilsDataForIndUses[!geographicUsda %in% c("E2", "E4")]
vegetableOilsDataForIndUses[, group := "official"]
industrialUsesData = rbind(vegetableOilsDataForIndUses, estimated)
##

# Checking how is the disaggregation
tabValid = industrialUsesData[, list(totValue = sum(Value)),
                          by = c("geographicUsda", "timePointYears", "group")]

tabValidEuro = tabValid[geographicUsda %in% europeanCountries]

library(ggplot2)
library(scales)

theme_fao <- function(angle=90, fonte="calibri", size=14) {
  #theme(panel.grid.major = element_blank()) +
  #theme(panel.background = element_blank())+
  #theme(legend.title = element_text(family=fonte, size = size, face = "bold")) +
  theme(legend.text = element_text(family=fonte, size = size, face = "bold")) +
    theme(plot.title = element_text(family=fonte, size = size, face = "bold")) +
    theme(axis.text.x = element_text(family=fonte, size = size, face = "bold", angle = angle, vjust = .5)) +
    theme(axis.text.y= element_text(family=fonte, size = size, face = "bold")) +
    theme(axis.title.x= element_text(family=fonte, size = size, face = "bold")) +
    theme(axis.title.y= element_text(family=fonte, size = size, face = "bold"))

}

ggplot(data=tabValidEuro,
       aes(x=as.numeric(timePointYears), y=totValue, group=geographicUsda, col=group)) +
  geom_line(aes(), stat = "identity", position=position_dodge(), size=1.5) +
  facet_wrap(~ geographicUsda) +
  scale_x_continuous(limits = c(1961, 2015), breaks=seq(1961, 2016, 5)) +
  scale_y_continuous(lim=c(0, 3500),  breaks=seq(0, 3500, 500), labels = comma) +
  theme(legend.title=element_blank()) +
  ylab('Industrial Dom.Cons (1000 T)') + xlab('Year') +
  theme_fao(45, size = 10)
