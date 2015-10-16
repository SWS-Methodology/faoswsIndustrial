## This script mapping from USDA code to M49 code.

## M49 codes and names:
m49Code <- GetCodeList("agriculture", "agriculutre", "geographicAreaM49")
m49Code <- m49Code[type == "country", ]
setnames(m49Code, "code", "m49")


## Eurostat codes and names:
usdaCode <- GetCodeList("eurostat", "raw_apro_mt_pann", "eurostatRawGeo")
setnames(usdaCode, "code", "usda")

## Eurostat codes -> M49 codes
euroStatM49 <- GetTableData("ess", "eurostat_m49")
setnames(euroStatM49, "eurostat", "usda")


compare <- merge(m49Code, euroStatM49, by = "m49", all = TRUE)
compare <- merge(compare, usdaCode, by = "usda", all = TRUE,
                 suffixes = c(".m49", ".usda"))


compareCountry <- compare[type.m49 == "country"] 

## Unique
compareCountryUSDA <- compareCountry[, list(descriptionM49 = unique(description.m49),
                                      descriptionUSDA = unique(description.usda)),
                               by=list(m49)]

compareCountryUSDA <- compareCountryUSDA[!is.na(descriptionUSDA) == T]

## USDA Data: Pull dataset used in Industrial Module

workingDir = "~/Github/privateFAO/OrangeBook/"

load(file = paste0(workingDir,"vegetableOilsData.RData"))
attach(vegetableOilsData)

vegetableOilsData <- data.table(vegetableOilsData)

## Filtering Attribute_Description == "Industrial Dom. Cons."
vegetableDataIndustrial <- vegetableOilsData[Attribute_Description == "Industrial Dom. Cons."]

## USDA Data
usdaData <- vegetableDataIndustrial[, list(usdaCode = unique(Country_Code)), 
                                    by = list(Country_Name)]

setnames(usdaData, c("descriptionUSDA", "usdaCode"))

usdaData[, descriptionUSDA := as.character(descriptionUSDA)]

## Merge usdaData with compareCountryUSDA
mergeM49USDA <- merge(usdaData, compareCountryUSDA, by = "descriptionUSDA", all.x = T)

## No aggregation
mergeM49USDA[is.na(m49) == T]

noAggreg <- mergeM49USDA[is.na(m49) == T, descriptionUSDA]

compareCountry[grep(paste(noAggreg, collapse="|"), description.usda)]

compareCountry[grep("Other", description.m49)] ## ???

## Not found
# Union of Soviet Socialist Repu 
# Yemen (Aden)
# Yemen (Sanaa)
# European Union
# Other

## Imputation

imputData <- data.table(descriptionUSDA = c("Belgium-Luxembourg", "Bermuda", "Burkina", "Burma", "China", "Congo (Kinshasa)", "Cote d'Ivoire",
                                            "Faroe Islands", "Former Czechoslovakia", "Former Yugoslavia", "German Democratic Republic",
                                            "Germany", "Germany, Federal Republic of", "Korea, North", "Korea, South", "Macedonia", "Netherlands Antilles",
                                            "New Caledonia", "Serbia and Montenegro", "Taiwan", "EU-15"),
                        
                        usdaCode = c("BE", "BD", "UV", "MM", "CN", "CG", "IV", "FO", "CZ", "YO", "GC", "GM", "GE", "KN", "KS", "MK", "NA", "NC", "SR", "TW", "E2"),
                        
                        m49 = c("58", "60", "854", "104", "156", "178", "384", "234", "200", "890", "278", "276", "280", "408", "410", "807", "530", "540", "891", "158", "1095"),
                        
                        descriptionM49 = c("Bel-Lux(-1999)", "Bermuda", "Burkina Faso", "Myanmar", "China", "Congo", "Côte d'Ivoire",
                                           "Faroe Islands", "Czechoslovak(-1992)", "Yugoslav SFR(-1991)", "Germany Nl", "Germany", 
                                           "Germany Fr", "Dem People's Rep of Korea", "Republic of Korea", "TFYR of Macedonia(1992-)", "NethAntilles", 
                                           "New Caledonia", "Serbia-Monte(1992-2005)", "China,Taiwan", "EU (15)"))



mapM49CodeUSDA <- mergeM49USDA[is.na(m49) != T]

mapM49CodeUSDA <- rbind(mapM49CodeUSDA, imputData)
mapM49CodeUSDA

lapply(mapM49CodeUSDA, function(x) length(unique(x)))

usdaMap <- mapM49CodeUSDA[, usdaCode, m49]

## Save data
write.csv(usdaMap, file = "//hqlprsws2.hq.un.fao.org/sws_r_share/caetano/usdaMap.csv", row.names = F)
write.csv(usdaMap, file = "//hqlprsws1.hq.un.fao.org/sws_r_share/caetano/usdaMap.csv", row.names = F)


