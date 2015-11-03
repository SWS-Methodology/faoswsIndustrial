##' Get CPC Item Tree
##' 
##' @param itemVar The name of the "item" dimension.
##' 
##' @return Codes
##' 

getCPCTreeItem = function(itemVar = "measuredItemCPC"){
  ## itemTable =
  ##     GetCodeList(domain = slot(dataContext, "domain"),
  ##                 dataset = slot(dataContext, "dataset"),
  ##                 dimension = itemVar)
  ## HACK (Michael): Since we don't have the columne 'type' ready
  ##                 for selection, we will select all item which
  ##                 are under the CPC heading '0'.
  itemEdgeList =
    adjacent2edge(
      GetCodeTree(domain = "agriculture",
                  dataset = "agriculture",
                  dimension = itemVar)
    )
  
  itemEdgeGraph = igraph::graph.data.frame(itemEdgeList)
  itemDist = shortest.paths(itemEdgeGraph, v = "CPC", mode = "out")
  fbsItemCodes = colnames(itemDist)[is.finite(itemDist) &
                                      colnames(itemDist) != "CPC"]
  fbsItemCodes
}
