##' Split Industrial Data
##'
##' This function splits the industrial data from USDA for member States
##' of European Union.
##'
##' @param usdaData The dataset from USDA.
##'
##' @export
##'


splitIndustrialData <- function(usdaData) {

  data <- copy(usdaData)

  ## Data Quality Checks
  stopifnot(is(data, "data.table"))
  stopifnot(names(data) == c("geographicUsda", "measuredElementPsd", "measuredItemPsd",
                              "timePointYears", "Value"))

  data[, measuredElementPsd := NULL]
  data[, measuredItemCPC := as.character(getItemCommSUA(measuredItemPsd))]
  data[geographicUsda %in% c("GE", "GC"), geographicUsda := "GM"]
  data <- data[, list(Value = sum(Value)),
                       by=c("geographicUsda", "measuredItemCPC", "timePointYears")]

  ## Member States of European Union
  memberStatesEU <- ReadDatatable("member_states_eu")
  europeanCountries <- memberStatesEU[, .N, usda_code][, usda_code]

  ## Belgium and Luxembourg provided figures together to USDA. So, basically we need to split the data into these countries.
  ## If you consider the "Production Capacity" from European Biodiesel Industry, they have values to these countries separated, so
  ## we just calculate the percent of this piece of information. So we'll consider that 98% of the industrial utilization came from
  ## Belgium and 2% from Luxembourg.
  ## http://www.ebb-eu.org/stats.php#

  dataEBB <- data.table(country = c("Belgium", "Luxembourg"),
                        timePointYears = 2013,
                        prodCapacity = c(991, 20))
  dataEBB[, percent := prodCapacity/sum(prodCapacity)]

  ## we need to split the propotion for the previous years to Belgium and Luxembourg
  luxembourg <- data[geographicUsda == "BE"]
  luxembourg[, geographicUsda := "LU"]
  luxembourg[, Value := Value * 0.01978239]

  data[geographicUsda == "BE", Value := Value * 0.98021761]

  ## Put "Luxembourg" on the data set
  data <- rbind(data, luxembourg)

  ## How many european countries are there per year?
  data[geographicUsda %in% europeanCountries, list(length(unique(geographicUsda))),
                              by = timePointYears]

  ## Let's use the year 1990 to calculate the percent of each country/commodity in the industrial utilization

  ## Only 2 countries (Czech Republic, Slovakia) that don't have data in 1990. So we need to extrapolate figures for them.
  ## So we can use the simple linear regression model in order to do that to this year.

  ## Czech Republic (EZ)
  countryEZ <- data[geographicUsda == "EZ"]
  fitEZ <- lm(data=countryEZ[Value > 0 & measuredItemCPC == "21641.01"],
              Value ~ as.numeric(timePointYears))
  fitEZ$coefficients[1] + fitEZ$coefficients[2] * 1990

  czechRepublic <- data.table(geographicUsda = rep("EZ", 2),
                              measuredItemCPC = c("21641.01", "21611"),
                              timePointYears = rep(1990, 2),
                              Value = c(80.11429, 1))

  ## Slovakia (LO)
  countryLO <- data[geographicUsda == "LO"]
  fitLO <- lm(data=countryLO[Value > 0 & measuredItemCPC == "21641.01"],
              Value ~ as.numeric(timePointYears))
  fitLO$coefficients[1] + fitLO$coefficients[2] * 1990

  slovakia <- data.table(geographicUsda = "LO",
                         measuredItemCPC = "21641.01",
                         timePointYears = 1990,
                         Value = 6.171429)

  ## Now we need to aggregate the values to Czech Republic (EZ) and Slovakia (LO)
  europeanCountry1990 = data[geographicUsda %in% europeanCountries &
                                                      timePointYears == 1990]

  ## So here we are just using rbind to aggregate the datasets to these 2 countries
  europeanCountry1990 <- rbind(europeanCountry1990, czechRepublic, slovakia)

  ## We are using the year "1990" to compute the % of industrial utilization to each european country
  ## according to when it became a member state. This % will be calculated to have information by european country
  ## from 1991 to 2015

  europeanCountry1990 <- merge(europeanCountry1990, memberStatesEU[, c("usda_code", "accession"), with = F],
                               by.x = "geographicUsda", by.y = "usda_code")

  memberBefore1990 <- europeanCountry1990[accession  == "before1990"]
  memberBefore1990 <- cbind(memberBefore1990,
                            years = rep(1991:2015, each = nrow(memberBefore1990)))

  member1995 <- europeanCountry1990[accession  == "1995"]
  member1995 <- cbind(member1995,
                      years = rep(1995:2015, each = nrow(member1995)))

  member2004 <- europeanCountry1990[accession  == "2004"]
  member2004 <- cbind(member2004,
                      years = rep(2004:2015, each = nrow(member2004)))

  member2007 <- europeanCountry1990[accession  == "2007"]
  member2007 <- cbind(member2007,
                      years = rep(2007:2015, each = nrow(member2007)))

  member2013 <- europeanCountry1990[accession  == "2013"]
  member2013 <- cbind(member2013,
                      years = rep(2013:2015, each = nrow(member2013)))

  newData <- rbind(memberBefore1990, member1995, member2004, member2007, member2013)
  newData[, c("timePointYears", "accession") := NULL]
  newData[, percent := Value/sum(Value),
          by = list(years, measuredItemCPC)]

  newData[percent == "NaN", percent := 0]
  newData[, Value := NULL]
  setnames(newData, old = c("geographicUsda", "years"),
           new = c("usdaCode", "timePointYears"))
  newData[, timePointYears := as.character(timePointYears)]

  ## Values to the European Union (E2 and E4) from 1991 to 2015
  e2E4 <- data[geographicUsda %in% c("E4", "E2")]

  ## Final merge
  keys <- c("measuredItemCPC", "timePointYears")
  estimated <- merge(e2E4, newData, by = keys,
                     all.x = T)

  estimated[, percentValue := Value * percent]
  estimated[, c("geographicUsda", "Value", "percent") := NULL]
  setnames(estimated, "percentValue", "Value")
  setnames(estimated, "usdaCode", "geographicUsda")
  setcolorder(estimated, c("geographicUsda", "measuredItemCPC", "timePointYears", "Value"))
  estimated <- estimated[!is.na(Value)]
  estimated[, group := "estimated"]

  ## We have to exclude the data to E2 and E4 and put together the table "estimated"
  data <- data[!geographicUsda %in% c("E2", "E4")]
  data[, group := "official"]
  industrialUsesData <- rbind(data, estimated)
  industrialUsesData[, geographicAreaM49 := as.character(getCountryCode(geographicUsda))]
  industrialUsesData[, c("geographicUsda") := NULL]
  setcolorder(industrialUsesData, c("timePointYears", "geographicAreaM49",
                                    "measuredItemCPC", "Value", "group"))
  return(industrialUsesData)


}
