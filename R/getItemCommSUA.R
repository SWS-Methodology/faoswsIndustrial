##' Get Item Comm SUA
##' 
##' @param Commodity_Description The description of the commodity.
##' 
##' @return Codes
##'

getItemCommSUA = function(Commodity_Description){
  map = read.csv("~/Github/faoswsIndustrial/Data/cpcCodeDescription.csv",
                 stringsAsFactors = FALSE)
  return(map$itemCode[map$commodityDescription == Commodity_Description])
}



