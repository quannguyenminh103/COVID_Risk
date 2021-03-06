LoadIreland <- function() {
  
  geom <<- st_read('https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/Ireland_Counties.geojson')
  
  #Main COVID-19 hub page: https://covid-19.geohive.ie/datasets/d9be85b30d7748b5b7c09450b8aede63_0
  data <- read.csv("https://opendata.arcgis.com/datasets/d9be85b30d7748b5b7c09450b8aede63_0.csv") %>%
    mutate(date = as.Date(TimeStamp)) %>%
    select(CountyName, date, cases=ConfirmedCovidCases, pop = PopulationCensus16) %>%
    arrange(desc(date))
  data_cur <<- data %>%
    group_by(CountyName) %>%
    summarise(CountyName = first(CountyName), cases = first(cases), date = first(date), pop = first(pop)) %>%
    as.data.frame()
  past_date <- data_cur$date[1] - 14
  data_past <- data %>%
    filter(date == past_date) %>%
    group_by(CountyName) %>%
    summarise(CountyName = first(CountyName), cases = first(cases), date = first(date)) %>%
    as.data.frame()
  data_join <<- inner_join(data_cur, data_past, by = "CountyName", suffix=c('', '_past'))
  data_join$Difference <- (data_join$cases - data_join$cases_past)*10/14

  IrelandMap <- inner_join(geom,data_join, by = c('id' = 'CountyName'))
  IrelandMap$RegionName = paste0(IrelandMap$id,", Ireland")
  IrelandMap$DateReport = as.character(IrelandMap$date) 
  IrelandMap$pInf = IrelandMap$Difference/IrelandMap$pop
  
  IRELAND_DATA = subset(IrelandMap,select=c("DateReport","RegionName","pInf","geometry"))
  return(IRELAND_DATA)
}
