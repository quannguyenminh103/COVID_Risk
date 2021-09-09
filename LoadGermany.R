LoadGermany <- function() {

GERMANY_SOURCE_Data <-  data.frame(st_read('https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson'))
germanyData <- GERMANY_SOURCE_Data[,c("Landkreis","AnzahlFall","Meldedatum",'IdLandkreis')]
# rename
names(germanyData) <- c('County', 'Cases', 'Date','Id_County')
germanyData$Date <- as.Date(germanyData$Date)
germanyData <- germanyData[order(germanyData$Date),]

# Geometry Germany
geometryGerman <- st_read('https://public.opendatasoft.com/explore/dataset/covid-19-germany-landkreise/download/?format=geojson&timezone=Europe/Berlin&lang=en')
geomGerman <- geometryGerman[,c('county','geometry')]
# get the county list
County <- unique(germanyData$County)

## find out the difference in 10 days correction for each county from 14 days difference
germany_sortFunc <- function(code){
  dataSet <- germanyData %>% filter(germanyData$County == County[code])
  dataSet$CumSum <- cumsum(dataSet$Cases)
  latestDate <- dataSet$Date[length(dataSet$Date)]
  pastDate <- latestDate - 14
  pastSet <- dataSet[which(dataSet$Date <= pastDate),] # there are some missing days in the past, so we have to choose the close day
  X = as.numeric((latestDate - pastSet$Date[length(pastSet$Date)]))  # get how many days difference
  #print(X) 14 and 15's - must be 14 or more!
  if (X > 2){ #?
    difference <- ((max(dataSet$CumSum) - max(pastSet$CumSum))*10/X)
  } else {
    difference <- NA
  }
  vec <- data.frame(County = County[code],Date = latestDate, Difference = difference, IdLandkreis = dataSet$Id_County[1])
  return(vec)
}

## get the data table contains landkreis, updated date and difference in 14 days
germanyTable <- data.frame()
for (i in 1:length(County)){
  vec <- germany_sortFunc(i)
  germanyTable <- rbind(germanyTable,vec)
}
germanyTable$IdLandkreis <- as.numeric(germanyTable$IdLandkreis)

## POP: https://www.citypopulation.de/en/germany/admin/
### PLEASE CHANGE YOUR DIRECTORY 
pop <- read.csv('germanyPop.csv')
# turn Id Landkreis columns into numeric form:
pop$IdLandkreis <- as.numeric(pop$IdLandkreis)
germanydf <- inner_join(germanyTable,pop, by = 'IdLandkreis') # add pop column into the main germany table

germanyMap <- inner_join(geomGerman, germanydf, by = c("county" = "County"))
germanyMap$DateReport =  as.character(germanyMap$Date)
germanyMap$RegionName = paste0(germanyMap$county,' Germany')
germanyMap$pInf = germanyMap$Difference/germanyMap$Population
GERMANY_DATA = subset(germanyMap,select=c("DateReport","RegionName","pInf","geometry"))

return(GERMANY_DATA)
}