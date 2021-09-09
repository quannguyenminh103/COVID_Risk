LoadBrazil <- function(){

data <- read.csv('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv', encoding = 'UTF-8')
data <- data[rev(order(as.Date(data$date))),c("date","state","totalCasesMS")]
stateList <- unique(data$state)
data$date <- as.Date(data$date)
today <- data$date[1]
past <- today - 14
brazilCases <- data.frame(Date = as.character(), state = as.character(), Difference = as.numeric())
for (i in 1:length(stateList)){
  difference <- (data[(data$date == today & data$state == stateList[i]),'totalCasesMS'] - data[(data$date == past & data$state == stateList[i]),"totalCasesMS"])*10/14
  vec <- data.frame(Date = today, state = stateList[i], Difference = difference)
  brazilCases <- rbind(brazilCases,vec)
}
data[(data$date == today & data$state == stateList[i]),'totalCasesMS']
## population
pop <- read.csv('https://raw.githubusercontent.com/wcota/covid19br/master/cities_info.csv', encoding = 'UTF-8')
pop <- pop[,c("state",'pop2020')]
popState <- as.data.frame(pop %>% group_by(state) %>% summarise(pop2020 = sum(pop2020)))
names(popState) <- c('state','Population')
brazildf <- inner_join(brazilCases,popState, by = c("state"))

## geojson
geomBrazil <- st_read('https://raw.githubusercontent.com/marcioibm/brazil-states/master/br_states.geojson')
BrazilMap <- inner_join(geomBrazil, brazildf, by = c("sigla" = "state"))
BrazilMap$RegionName = paste0(BrazilMap$nome,", Brazil")
BrazilMap$DateReport = as.character(BrazilMap$Date)
BrazilMap$pInf = BrazilMap$Difference/BrazilMap$Population
Brazil_DATA = subset(BrazilMap,select=c("DateReport","RegionName","pInf","geometry"))
return(Brazil_DATA)
}