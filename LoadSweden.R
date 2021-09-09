LoadSweden <- function() {
  temp <- tempfile()
  download.file(url = "https://fohm.maps.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data", destfile = temp, mode="wb")
  swedenResource <- as.data.frame(readxl::read_excel(temp,col_names =T))
  unlink(temp)
  names(swedenResource)[1] <- 'date'
  swedenResource$date <- as.Date(swedenResource$date)
  SwedenCounty <- names(swedenResource)[3:length(names(swedenResource))]
  SwedenCounty[SwedenCounty == "Jämtland_Härjedalen"] <- "Jämtland"
  SwedenCounty[SwedenCounty == "Sörmland"] <- "Södermanland"
  SwedenCounty[SwedenCounty == "Västra_Götaland"] <- "Västra Götaland"
  names(swedenResource) = c(names(swedenResource)[1:2],SwedenCounty)
  
  data = swedenResource %>% 
    pivot_longer(3:23, names_to="County", values_to="cases") %>%
    select(-Totalt_antal_fall) %>%
    arrange(desc(date))
  
  geom <<- st_read("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/sweden-counties.geojson")
  pop <- read.csv("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/sweden_pop.csv", encoding = 'UTF-8')
  names(pop)[1] <- 'County'
  data_cur <<- data %>%
    group_by(County) %>%
    summarise(County = first(County), cases = sum(cases), date = first(date)) %>%
    as.data.frame()
  past_date <- data_cur$date[1] - 14
  data_past <- data %>%
    filter(date <= past_date) %>%
    group_by(County) %>%
    summarise(County = first(County), cases = sum(cases), date = first(date)) %>%
    as.data.frame()
  data_join <<- data_cur %>%
    inner_join(data_past, by = "County", suffix = c("", "_past")) %>%
    inner_join(pop, by = c("County")) 
  data_join$Difference <- (data_join$cases - data_join$cases_past)*10/14
  SwedenMap <- inner_join(geom,data_join, by = c("name"='County'))
  SwedenMap$RegionName = paste0(SwedenMap$name,", Sweden")
  SwedenMap$DateReport = as.character(SwedenMap$date) 
  SwedenMap$pInf = SwedenMap$Difference/SwedenMap$Population
  SWEDEN_DATA = subset(SwedenMap,select=c("DateReport","RegionName","pInf","geometry"))
  return(SWEDEN_DATA)

}