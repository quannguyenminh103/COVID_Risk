LoadNetherlands <- function(){

netherlandsData <- read.csv('https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv', sep = ';',fileEncoding = 'UTF-8') %>%
    select(Date = Date_of_publication, Code = Municipality_code, Municipality = Municipality_name, Province, Cases =Total_reported) %>%
    mutate(Date = as.Date(Date))
  
### Municipalities:
municipality <- unique(netherlandsData$Municipality)
getData <- function(code){
  temp <- netherlandsData %>% filter(netherlandsData$Municipality == municipality[code])
  temp$CumSum <- cumsum(temp$Cases)
  today <- temp$Date[length(temp$Date)]
  past_date <- today - 14
  pastData <- temp[temp$Date <= past_date,]
  ### SOME ROWS DO NOT REPORT MUNICIPALITY NAME
  difference <- (temp$CumSum[length(temp$CumSum)] - pastData$CumSum[length(pastData$CumSum)])/14*10
  vec <- data.frame(Municipality = municipality[code], Code = temp$Code[1], Date = today, Difference = difference)
  return(vec)
}

netherlandsTable <- data.frame()
for (i in 1:length(municipality)){
  vec <- getData(i)
  netherlandsTable <- rbind(netherlandsTable,vec)
}

netherlandsTable$Municipality[netherlandsTable$Municipality == "'s-Gravenhage" ] <- "Des Gravenhage"


### Geojson:
geomDutch <- st_read('https://opendata.arcgis.com/datasets/620c2ab925f64ed5979d251ba7753b7f_0.geojson')
# Note that geomDutch$Bevolkingsaantal is population size.

netherlandsMap <- inner_join(geomDutch, netherlandsTable, by = c("Gemeentecode" = "Code"))
netherlandsMap$RegionName = paste0(netherlandsMap$Municipality,", Netherlands")
netherlandsMap$DateReport = as.character(netherlandsMap$Date)
netherlandsMap$pInf = netherlandsMap$Difference/netherlandsMap$Bevolkingsaantal
NETHERLANDS_DATA = subset(netherlandsMap,select=c("DateReport","RegionName","pInf","geometry"))

return(NETHERLANDS_DATA)
}