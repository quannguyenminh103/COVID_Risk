LoadCzechia <- function(){
  czech_geom <<- st_read('https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/distictsCzechiaLow.json') 
  czech_geom <- czech_geom %>% select(name, geometry) %>% mutate(name = as.character(name))
  czech_pop <- read.csv('https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/czech_pop.csv',encoding = 'UTF-8')
  names(czech_pop)[1] <- 'code'
  czechData <- read.csv('https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/kraj-okres-nakazeni-vyleceni-umrti.csv', encoding = 'UTF-8')
  names(czechData) <- c('Date','code','District','Confirmed','Cure','Death')
  czechData$Date <- as.Date(czechData$Date)
  czechData = czechData %>% 
    group_by(District) %>% 
    slice(c(n(), n()-14)) %>% 
    summarize(cases = (Confirmed[1]-Confirmed[2])*10/14, Date = first(Date)) %>% 
    ungroup
  
  czech_data_join <- inner_join(as.data.frame(czechData), czech_pop, by = c("District" = "code"))
  names(czech_data_join) <- c('Code','Difference','Date','name','Population')
  CzechMap <- inner_join(czech_geom,czech_data_join, by = 'name')
  
  CzechMap$RegionName = paste0(CzechMap$name,", Czech")
  CzechMap$DateReport = as.character(CzechMap$Date) 
  CzechMap$pInf = CzechMap$Difference/CzechMap$Population
  
  CZECH_DATA = subset(CzechMap,select=c("DateReport","RegionName","pInf","geometry"))
  return(CZECH_DATA)
}
  