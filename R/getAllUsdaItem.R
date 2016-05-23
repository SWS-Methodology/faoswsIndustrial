##' Get All USDA Item
##'
##' This function pulls all the USDA item.
##'
##' @export
##'


getAllUsdaItem <- function(){
  GetCodeList(domain = "usda",
              dataset = "usda_psd",
              dimension = "measuredItemPsd")[, code]
}
