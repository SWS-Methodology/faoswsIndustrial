##' Get Item Comm SUA
##' 
##' @param Measured_Item_Psd The USDA item code .
##' 
##' @return Codes
##'
##' @export
##' 

getItemCommSUA = function(Measured_Item_Psd){
  map = fread("~/Github/faoswsIndustrial/Data/industrialUseCommodityMap.csv",
                 stringsAsFactors = FALSE)
  result = merge(map, data.table(measuredItemPsd = as.numeric(Measured_Item_Psd), index = 1:length(Measured_Item_Psd)), 
                 by="measuredItemPsd", all.y=T)
  setkeyv(result, "measuredItemPsd")
  result <- result[order(result$index)]
  result[, measuredItemCPC]
}



