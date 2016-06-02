##' Get Industrial Data
##'
##' This function pulls the industrial data from the SWS
##'
##' @param yearRange Character vector with the range of the years.
##'
##' @return The dataset with the industrial data.
##'
##' @export
##'

getIndustrialData <- function(yearRange) {

  indKey = DatasetKey(
    domain = "usda",
    dataset = "usda_psd",
    dimensions = list(
      Dimension(name = "geographicUsda",
                keys = getAllUsdaCountries()),
      Dimension(name = "measuredElementPsd",
                keys = "140.08"),
      Dimension(name = "measuredItemPsd",
                keys = getAllUsdaItem()),
      Dimension(name = "timePointYears",
                keys = yearRange))
  )

  indData = GetData(
    indKey,
    flags = FALSE)
  return(indData)
}
