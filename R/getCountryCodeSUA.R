##' Get Country Code SUA
##' 
##' @param country_name The name of the country.
##' 
##' @return Codes
##'


getCountryCodeSUA = function(country_name){
map = fread("~/Github/faoswsIndustrial/Data/usdaMap.csv",
                 stringsAsFactors = FALSE, na.strings = "")
result = merge(map, data.table(usdaCode = country_name, index = 1:length(country_name)), 
               by="usdaCode", all.y=T)
setkeyv(result, "usdaCode")
result <- result[order(result$index)]
result[, m49]
}

