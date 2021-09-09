LoadDanish <- function(){
  ### CHANGE: I just change the geojson file because in the old file, there are 98 municipalities while it has 99 right now
  geomDanish <- st_read('https://raw.githubusercontent.com/magnuslarsen/geoJSON-Danish-municipalities/master/municipalities/municipalities.geojson')
  geomDanish <- geomDanish[,c('label_en','geometry')]
  names(geomDanish)[1] <- 'name'
  geom <<- geomDanish
  webpages<-read_html("https://covid19.ssi.dk/overvagningsdata/download-fil-med-overvaagningdata")
  listLink <- html_attr(html_nodes(webpages, "a"), "href")
  tempDate <- strptime(Sys.Date(), "%Y-%m-%d")
  updatedDate <- format(tempDate, "%d%m%Y")
  linkNeeded <- paste0('https://files.ssi.dk/covid19/overvagning/data/overvaagningsdata-covid19-',updatedDate)
  if (length(listLink[startsWith(listLink,linkNeeded)]) == 0){
    tempDate <- strptime(Sys.Date()-1, "%Y-%m-%d")
    updatedDate <- format(tempDate, "%d%m%Y")  
    linkNeeded <- paste0('https://files.ssi.dk/covid19/overvagning/data/overvaagningsdata-covid19-',updatedDate)
  }
  DOWNLOADLINK <- paste0(listLink[startsWith(listLink,linkNeeded)],'.zip')
  
  # 2.) download and extract data:
  #td <- tempdir()
  temp <- tempfile() #temporary file for download
  temp2 <- tempfile()#temporary file for extraction
  download.file(DOWNLOADLINK,temp)
  unzip(zipfile = temp, exdir = temp2)
  list.files(temp2)
  
  DanishData  <- read.csv(file.path(temp2, "Municipality_cases_time_series.csv"),sep=";",encoding="UTF-8", stringsAsFactors = F)
  unlink(temp)
  unlink(temp2)
  DanishCounty <- names(DanishData)[2:length(names(DanishData))]
  DanishData$SampleDate <- as.Date(DanishData$SampleDate)
  getDanishData <- function(code){
    subdata <- DanishData[,c("SampleDate",DanishCounty[code])]
    subdata$CumCases <- cumsum(subdata[,DanishCounty[code]])
    x <- length(subdata$SampleDate)
    difference <- (subdata[x,'CumCases'] - subdata[x-14,'CumCases'])*10/14
    vec <- data.frame(Municipality = DanishCounty[code], Date = subdata$SampleDate[x], Difference = difference)
    return(vec)
  }
  
  dataTable <- data.frame(Municipality = as.character(), Date = as.character(), Difference = as.numeric())
  for (i in 1:length(DanishCounty)){
    vec <- getDanishData(i)
    dataTable <- rbind(dataTable,vec)
  }
  dataTable <- dataTable %>% mutate(Municipality = as.character(Municipality), Date = as.Date(Date))
  DanishPop <- as.data.frame(read.csv('https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/denmark_pop.csv', encoding="UTF-8", stringsAsFactors = F)) ## get from Statistics Denmark: https://www.statbank.dk/statbank5a/SelectVarVal/saveselections.asp
  names(DanishPop) <- c("Municipality",'Population')
  
  # make the population column as numeric
  DanishPop$Population <- as.numeric(gsub(" ","",DanishPop$Population))
  
  # adjust some municipalities' names so that they match with population file
  dataTable$Municipality[which(dataTable$Municipality == "Høje.Taastrup")] <-  "Høje-Taastrup"
  dataTable$Municipality[which(dataTable$Municipality == "Faaborg.Midtfyn")] <-  "Faaborg-Midtfyn"
  dataTable$Municipality[which(dataTable$Municipality == "Lyngby.Taarbæk")] <-  "Lyngby-Taarbæk"
  dataTable$Municipality[which(dataTable$Municipality == "Ringkøbing.Skjern")] <-  "Ringkøbing-Skjern"
  dataTable$Municipality[which(dataTable$Municipality == "Ikast.Brande")] <-  "Ikast-Brande"
  dataTable$Municipality[which(dataTable$Municipality == "NA.")] <-  "Christiansø"

  danishdf <- inner_join(dataTable, DanishPop, by = "Municipality")
  danishdf$Municipality[which(danishdf$Municipality == 'Nordfyns')] <- "Nordfyn"
  danishdf$Municipality[which(danishdf$Municipality == 'Vesthimmerlands')] <- "Vesthimmerland"
  
  DanishPop$Population <- as.numeric(gsub(" ","",DanishPop$Population))

  DanishMap <- inner_join(geomDanish, danishdf, by = c("name" = "Municipality"))
  DanishMap$RegionName = paste0(DanishMap$name,", Denmark")
  DanishMap$DateReport = as.character(DanishMap$Date) 
  DanishMap$pInf = DanishMap$Difference/as.numeric(DanishMap$Population)
  DANISH_DATA = subset(DanishMap,select=c("DateReport","RegionName","pInf","geometry"))
  # the spatial data has some problem in Z and M coordinators, which simply doesn't let us to draw the map
  # need to use st_zm to modify this error.
  DANISH_DATA <- st_zm(DANISH_DATA, drop = T, what = "ZM")
return(DANISH_DATA)
  
}
