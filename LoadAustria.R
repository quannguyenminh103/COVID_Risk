LoadAustria <- function() {
  
  data <- read.csv('https://covid19-dashboard.ages.at/data/CovidFaelle_Timeline_GKZ.csv',sep = ';',encoding = 'UTF-8', stringsAsFactors = FALSE) %>% 
    select(date = Time, name = Bezirk, code = GKZ, population = AnzEinwohner, cases = AnzahlFaelleSum)
  # format the date 
  for (i in 1:length(data$date)){
    data$date[i] <- unlist(strsplit(data$date[i],' '))[1]
  }
  data <- data %>% mutate(date = as.Date(format(strptime(as.character(date),"%d.%m.%Y"), "%Y-%m-%d")), 
                          code = as.character(code)) %>% 
    arrange(desc(date)) %>% filter(!is.na(cases)) 
  geom <<- st_read('https://raw.githubusercontent.com/ginseng666/GeoJSON-TopoJSON-Austria/master/2017/simplified-99.9/bezirke_999_geo.json')
  
  cur_date <- ymd(gsub("-", "", Sys.Date()))-1 
  past_date <- ymd(cur_date) - 14
  data_cur <<- data %>% group_by(code) %>% 
    summarise(code = first(code), cases = first(cases), date = first(date), pop = first(population)) %>% 
    as.data.frame()
  data_past <- data %>% 
    filter(date <= past_date) %>% 
    group_by(code) %>% 
    summarise(code = first(code), cases = first(cases), date = first(date)) %>% 
    as.data.frame()
  data_join <<- data_cur %>%
    inner_join(data_past, by = "code", suffix=c('', '_past')) %>%
    mutate(n = date-date_past) 
  data_join$Difference <- (data_join$cases-data_join$cases_past)*10/14
  AustriaMap <- inner_join(geom,data_join,by = c('iso' = 'code'))
  AustriaMap$RegionName = paste0(AustriaMap$name,", Austria")
  AustriaMap$DateReport = as.character(AustriaMap$date) 
  AustriaMap$pInf = AustriaMap$Difference/AustriaMap$pop
  AUSTRIA_DATA = subset(AustriaMap,select=c("DateReport","RegionName","pInf","geometry"))
  return(AUSTRIA_DATA)
  
}