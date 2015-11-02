##' Get Country Code SUA
##' 
##' @param country_name The name of the country.
##' 
##' @return Codes
##'


getCountryCodeSUA = function(country_name){
map = read.csv("~/Github/faoswsIndustrial/Data/usdaMap.csv",
                 stringsAsFactors = FALSE, na.strings = "")
  return(map$m49[map$usdaCode == country_name])
}

