LoadChina <- function(){
data <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')

# get updated date:
date <- names(data)[length(names(data))]
date <- strsplit(date,'X')[[1]][2]
date <- as.Date(date, format = "%m.%d.%y")

chineseData <- data[data$Country.Region == 'China',]
len <- length(names(chineseData))
latestData <- chineseData[,c(1,(len-14),len)]
names(latestData) <- c('Province','Past','Today')
latestData[4] <- (latestData[3]-latestData[2])*10/14
latestData <- latestData[,c(1,4)]
names(latestData) <- c('Province','Difference')
## population file
pop <- read.csv('chinaPopulation.csv')
chinesedf <- inner_join(latestData,pop, by = c('Province' = 'Name'))

## geojson file
geomChina <- st_read('https://raw.githubusercontent.com/deldersveld/topojson/master/countries/china/china-provinces.json')
geomChina <- geomChina[,c('NAME_1','geometry')]
geomChina = st_set_crs(geomChina,4326)

ChinaMap <- inner_join(geomChina, chinesedf, by = c("NAME_1" = "Province"))
ChinaMap$RegionName = paste0(ChinaMap$NAME_1,", China")
ChinaMap$DateReport = as.character(date) 
ChinaMap$pInf = ChinaMap$Difference/ChinaMap$Population

CHINA_DATA = subset(ChinaMap,select=c("DateReport","RegionName","pInf","geometry"))
return(CHINA_DATA)
}