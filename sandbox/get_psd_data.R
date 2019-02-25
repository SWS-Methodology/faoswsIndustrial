library(data.table)

# Download: https://apps.fas.usda.gov/psdonline/downloads/psd_alldata_csv.zip
# Unzip it to psd_alldata.csv

psd_data <- fread('psd_alldata.csv', colClasses = 'character')

psd_data[, measuredElementPsd := paste(Attribute_ID, Unit_ID, sep = '.')][, `:=`(Attribute_ID = NULL, Unit_ID = NULL, Calendar_Year = NULL, Month = NULL, Commodity_Description = NULL, Country_Name = NULL, Attribute_Description = NULL, Unit_Description = NULL)]

setnames(psd_data, c("Country_Code", "Commodity_Code", "Market_Year"), c("geographicUsda", "measuredItemPsd", "timePointYears"))

psd_data <- psd_data[measuredElementPsd %in% GetCodeList('usda', 'usda_psd', 'measuredElementPsd')$code]

psd_data <- psd_data[geographicUsda %in% GetCodeList('usda', 'usda_psd', 'geographicUsda')$code]

setcolorder(psd_data, c("geographicUsda", "measuredElementPsd", "measuredItemPsd", "timePointYears", "Value"))
