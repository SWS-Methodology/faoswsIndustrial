##' Get Bio Fuel Data
##' 
##' @param areaVar The name of the "geographic area" dimension.
##' @param elementVar The name of the "element" dimension.
##' @param itemVar The name of the "item" dimension.
##' @param yearVar The name of the "year" dimension.
##' 
##' @return Codes
##' 



## Function to get bio-fuel utilization
## Data are provided by OECD-FAO Aglink Cosimo.  
## 2000-2012

getBioFuelData <- function(areaVar = "geographicAreaM49", elementVar = "induseElement",  
         itemVar = "measuredItemCPC", yearVar = "timePointYears"){

selectedYear <- as.character(1961:2015)

allCountries =
  GetCodeList(domain = "industrialUse",
              dataset = "biofuel",
              dimension = areaVar)[type == "country", code]


# ## Get all codes in CPC tree (also "CPC", "Tree", ...)
# item = 
#   GetCodeList(domain = "industrialUse", 
#               dataset = "biofuel", 
#               dimension = "measuredItemCPC")
## Get all codes in CPC tree that are descendants (children) of 0
item = getCPCTreeItem()

bioFuelKey = DatasetKey(
  domain = "industrialUse",
  dataset = "biofuel",
  dimensions = list(
    Dimension(name = areaVar,
              keys = allCountries),
    Dimension(name = elementVar,
              keys = "5150"),
    Dimension(name = itemVar,
              keys = item),
    Dimension(name = yearVar,
              keys = selectedYear)
  )
)

## Pivot to vectorize yield computation
bioFuelPivot = c(
  Pivoting(code = areaVar, ascending = TRUE),
  Pivoting(code = itemVar, ascending = TRUE),
  Pivoting(code = yearVar, ascending = FALSE),
  Pivoting(code = elementVar, ascending = TRUE)
)

bioFuelQuery = GetData(
  key = bioFuelKey,
  flags = TRUE,
  normalized = FALSE,
  pivoting = bioFuelPivot
)
setnames(bioFuelQuery,
         old = grep("induseElement", colnames(bioFuelQuery), value = TRUE),
         new = gsub("induseElement", "measuredElement",
                    grep("induseElement", colnames(bioFuelQuery),
                         value = TRUE)))


## Convert time to numeric
bioFuelQuery[, timePointYears := as.numeric(timePointYears)]
bioFuelQuery
}