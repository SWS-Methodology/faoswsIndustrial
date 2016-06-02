##' Get All USDA Countries
##'
##' This function pulls all the USDA countries.
##'
##' @export
##'

getAllUsdaCountries <- function(){
  GetCodeList(domain = "usda",
              dataset = "usda_psd",
              dimension = "geographicUsda")[, code]
}
