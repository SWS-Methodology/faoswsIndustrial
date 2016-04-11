##' Get Country Code SUA
##'
##' @param country_name The name of the country.
##'
##' @return Codes
##'
##' @export
##'
##' @import countrycode
##'


getCountryCodeSUA = function(country_name){
map = fread("Data/countriesUSDA.csv")
map[, m49 := countrycode::countrycode(usdaCode, "fips104", "un")]
result = merge(map, data.table(usdaCode = country_name, index = 1:length(country_name)),
               by="usdaCode", all.y=T)
setkeyv(result, "usdaCode")
result <- result[order(result$index)]
result[, m49]
}

