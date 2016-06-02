##' Get Item Comm SUA
##'
##' @param Measured_Item_Psd The USDA item code .
##'
##' @return Codes
##'
##' @export
##'

getItemCommSUA = function(Measured_Item_Psd){
  map = ReadDatatable("commodity_map")
  result = merge(map, data.table(item_psd = as.character(Measured_Item_Psd), index = 1:length(Measured_Item_Psd)),
                 by="item_psd", all.y=T)
  setkeyv(result, "item_psd")
  result <- result[order(result$index)]
  result[, item_cpc]
}



