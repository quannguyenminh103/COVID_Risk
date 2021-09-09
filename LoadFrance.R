LoadFrance <- function() {
  
  data <- read.csv('https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675',sep = ';', stringsAsFactors = FALSE) %>%
    filter(cl_age90 == 0) %>% 
    select(code = dep, date = jour, cases = P) %>%
    mutate(date = as.Date(date)) %>% 
    filter(!is.na(cases)) 
  geom <<- st_read('https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements-avec-outre-mer.geojson')
  pop <- read.csv("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/france_pop.csv", stringsAsFactors = FALSE) %>% select(code = Code, name = Department, pop = Population)
  
  depList <- unique(data$code) # get the list of all department codes
  
  # sort out and calculate the number of cases during two recent weeks
  # depList[code] = corresponding code of a department
  sortFunc <- function(code){
    deptCode <- depList[code] 
    department <- data %>% filter(code == deptCode) %>% distinct(date, .keep_all = TRUE) 
    latestDate <- department$date[length(department$date)]
    pastDate <- latestDate - 14
    difference <- (sum(department[1:which(department$date == latestDate),'cases']) - sum(department[1:which(department$date == pastDate),'cases']))*10/14
    vec <- data.frame(code = depList[code], date = latestDate, n = difference)
    return(vec)
  }
  
  # get the data table that includes department codes, last updated date, difference between 14 days
  frenchTable <- data.frame()
  for (i in 1:length(depList)){
    vec <- sortFunc(i)
    frenchTable <- rbind(frenchTable,vec)
  }
  frenchdf <- inner_join(frenchTable, pop, by = 'code')
  FranceMap <- inner_join(geom,frenchdf, by = 'code')
  FranceMap$RegionName = paste0(FranceMap$nom,", France")
  FranceMap$DateReport = as.character(FranceMap$date) 
  FranceMap$pInf = FranceMap$n/FranceMap$pop
  
  FRANCE_DATA = subset(FranceMap,select=c("DateReport","RegionName","pInf","geometry"))
  return(FRANCE_DATA)
}
