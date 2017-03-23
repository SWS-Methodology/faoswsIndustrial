##' Get Country Code
##'
##' @param usdaCountryName The USDA country code of the country.
##'
##' @return Codes
##'
##' @export
##'



getCountryCode = function(usdaCountryName){
map = fread(paste0(R_SWS_SHARE_PATH, "/caetano/industrial/industrialUseCountryMap.csv"))
result = merge(map, data.table(usdaCode = usdaCountryName, index = 1:length(usdaCountryName)),
               by="usdaCode", all.y=T)
setkeyv(result, "usdaCode")
result <- result[order(result$index)]
result[, m49]
}

