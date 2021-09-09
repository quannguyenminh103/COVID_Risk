LoadBelgium <-  function() {

getDate <- function(x){
  date <- as.character(Sys.Date()-x)
  str <- unlist(strsplit(date,'-'))
  new_date <- sprintf("%s%s%s",str[1],str[2],str[3])
  return(new_date)
}

### I HAVEN'T KNOWN WHAT IS THE UPDATE TIME FRAME OF THIS LINK, HOWEVER, I EXPECT THEY UPDATE THE LATEST DATA BEFORE 2-3 PM ON THE SAME DATE
latest_data <- read.csv(paste0('https://epistat.sciensano.be/Data/',getDate(0),'/COVID19BE_CASES_MUNI_CUM_',getDate(0),'.csv'),fileEncoding = 'UTF-8')
latest_data <- latest_data[order(latest_data$TX_ADM_DSTR_DESCR_NL),]
latest_data <- latest_data[,c('TX_ADM_DSTR_DESCR_NL','TX_ADM_DSTR_DESCR_FR','CASES')]

past_data <- read.csv(paste0('https://epistat.sciensano.be/Data/',getDate(14),'/COVID19BE_CASES_MUNI_CUM_',getDate(14),'.csv'),fileEncoding = 'UTF-8')
past_data <- past_data[order(past_data$TX_ADM_DSTR_DESCR_NL),]
past_data <- past_data[,c('TX_ADM_DSTR_DESCR_NL','TX_ADM_DSTR_DESCR_FR','CASES')]

# get the list of arrondissment:
Arrondissement <- unique(latest_data$TX_ADM_DSTR_DESCR_NL)

# collect data for each arrondissment and assemble them into 1 table
getData <- function(dataSet){
  updated_data <- data.frame(Arrondissement = as.character(), Cases = as.numeric())
  for (i in 1:length(Arrondissement)){
    data <- dataSet %>% filter(dataSet$TX_ADM_DSTR_DESCR_NL == Arrondissement[i])
    if (sum(data$CASES == '<5') > 0){
      data[data$CASES == '<5','CASES'] <- 0 ## assume there is no case when the box shows <5
    }
    temp <- data.frame(Arrondissement = unlist(strsplit(Arrondissement[i],'Arrondissement '))[2],Cases = sum(as.numeric(data$CASES)))
    updated_data <-rbind(updated_data,temp)
  }
  return(updated_data)
}
latest_update <- getData(latest_data)
past_update <- getData(past_data)
difference <- (latest_update$Cases-past_update$Cases)*10/14

### DESIRED TABLE
finalData <- data.frame(Arrondissement = latest_update[,'Arrondissement'], Difference = difference)
# geojson file
geomBelgium <- st_read('BE_geom.geojson')
geomBelgium <- geomBelgium[,c("NameDUT","geometry")]
for (i in 1:length(geomBelgium$NameDUT)){
  geomBelgium$NameDUT[i] <- unlist(strsplit(geomBelgium$NameDUT[i], "Arrondissement "))[2]
}
geomBelgium$NameDUT[28] <- finalData$Arrondissement[22] # La Louviere
geomBelgium$NameDUT[39] <- finalData$Arrondissement[29] # Neufchâteau
finalData <- inner_join(geomBelgium, finalData, by = c('NameDUT' = 'Arrondissement'))
names(finalData)[1] <- 'Arrondissement'
## CHANGE NAMES IN THIS DATASET SO THAT THEY HAVE THE SAME NAME AS POPULATION file:
finalData$Arrondissement[finalData$Arrondissement == "Aarlen"] <- 'Arlon'
finalData$Arrondissement[finalData$Arrondissement == "Aat"] <- 'Ath'
finalData$Arrondissement[finalData$Arrondissement == "Bastenaken"] <- 'Bastogne'
finalData$Arrondissement[finalData$Arrondissement == "Bergen"] <- 'Mons'
finalData$Arrondissement[finalData$Arrondissement == "Borgworm"] <- 'Waremme'
finalData$Arrondissement[finalData$Arrondissement == "Doornik-Moeskroen"] <- 'Tournai-Mouscron'
finalData$Arrondissement[finalData$Arrondissement == "Hoei"] <- 'Huy'
finalData$Arrondissement[finalData$Arrondissement == "Namen"] <- 'Namur'
finalData$Arrondissement[finalData$Arrondissement == "Nijvel"] <- 'Nivelles'
finalData$Arrondissement[finalData$Arrondissement == "Brussel-Hoofdstad"] <- 'Bruxelles-Capitale'
finalData$Arrondissement[finalData$Arrondissement == "Zinnik"] <- 'Soignies'

## POPULATION FILE:
pop <- read.csv('belgiumPopulation.csv')
pop$Name[13] = 'Luik'

belgiumdf <- inner_join(finalData,pop, by = c('Arrondissement' = 'Name'))


## Change names:
nameMuni <- read.csv('belgiumNames.csv')
for (i in 1:length(belgiumdf$Arrondissement)){
  for (j in 1:length(nameMuni$Dutch.name)){
    if (belgiumdf$Arrondissement[i] == nameMuni$Dutch.name[j] & nameMuni$French.name[j] != '-'){
      belgiumdf$NAME[i] <- paste0(belgiumdf$Arrondissement[i],'/',nameMuni$French.name[j])
    }
    else if (belgiumdf$Arrondissement[i] == nameMuni$French.name[j] & nameMuni$Dutch.name[j] != '-'){
      belgiumdf$NAME[i] <- paste0(belgiumdf$Arrondissement[i],'/',nameMuni$Dutch.name[j])
    }
    else if ((belgiumdf$Arrondissement[i] == nameMuni$Dutch.name[j] | belgiumdf$Arrondissement[i] == nameMuni$French.name[j]) & (nameMuni$French.name[j] == '-' | nameMuni$Dutch.name[j] == '-')){
      belgiumdf$NAME[i] <- belgiumdf$Arrondissement[i]
    }
  }
  if (belgiumdf$Arrondissement[i] == 'Tournai-Mouscron'){
    belgiumdf$NAME[i] <- paste0('Tournai/Doornik-Mouscron/Moeskroen')
  }
}


belgiumdf$RegionName = paste0(belgiumdf$NAME, ' Belgium')
belgiumdf$DateReport = as.character(Sys.Date()) 
belgiumdf$pInf = belgiumdf$Difference/belgiumdf$Population

BELGIUM_DATA = subset(belgiumdf,select=c("DateReport","RegionName","pInf","geometry"))
return(BELGIUM_DATA)
}
