##' Get Item Comm SUA
##' 
##' @param Commodity_Description The description of the commodity.
##' 
##' @return Codes
##'

getItemCommSUA = function(Commodity_Description){
  map = read.csv("~/Github/faoswsIndustrial/Data/industrialUseCommodityMap.csv",
                 stringsAsFactors = FALSE)
  return(map$measuredItemCPC[map$Commodities == Commodity_Description])
}



