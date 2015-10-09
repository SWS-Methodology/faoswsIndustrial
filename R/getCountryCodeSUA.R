##' Get Country Code SUA
##' 
##' @param country_name The name of the country.
##' 
##' @return Codes
##'


getCountryCodeSUA = function(country_name){
map = GetCodeList("agriculture", "agriculture", "geographicAreaM49")
  return(map$code[map$description == country_name])
}

