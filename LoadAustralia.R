LoadAustralia <- function(){
data <- read.csv('https://raw.githubusercontent.com/M3IT/COVID-19_Data/master/Data/COVID_AU_state_cumulative.csv')
data$date <- as.Date(data$date)
data <- data[rev(order(data$date)),c("date",'state','confirmed')]
stateList <- unique(data$state)
today <- data$date[1]
past <- today - 14
australiaCases <- data.frame(Date = as.character(), state = as.character(),Difference = as.numeric())
for (i in 1:length(stateList)){
  difference <- (data[(data$date == today & data$state == stateList[i]),'confirmed'] - data[(data$date == past & data$state == stateList[i]),"confirmed"])*10/14
  vec <- data.frame(Date = today, state = stateList[i], Difference = difference)
  australiaCases <- rbind(australiaCases,vec)
}


## population
pop <- read.csv('AustraliaPop.csv')
australiadf <- inner_join(australiaCases,pop, by = 'state')

#geom
geomAustralia <- st_read('https://raw.githubusercontent.com/rowanhogan/australian-states/master/states.geojson')
AustraliaMap <- inner_join(geomAustralia, australiadf, by = c("STATE_NAME" = "state"))
AustraliaMap$RegionName = paste0(AustraliaMap$STATE_NAME,", Australia")
AustraliaMap$DateReport = as.character(AustraliaMap$Date)
AustraliaMap$pInf = AustraliaMap$Difference/AustraliaMap$Population
Australia_DATA = subset(AustraliaMap,select=c("DateReport","RegionName","pInf","geometry"))

return(Australia_DATA)
}
