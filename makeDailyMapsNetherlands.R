getDataNetherlands <- function(){
  netherlandsData <- read.csv('https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv', sep = ';',fileEncoding = 'UTF-8') %>%
    select(Date = Date_of_publication, Code = Municipality_code, cases =Total_reported) %>%
    mutate(Date = as.Date(Date)) %>% arrange(desc(Date))
  ndf <- netherlandsData %>% group_by(Code) %>% mutate(cases = sum(cases)) %>% filter(Date == Date[1]) %>% distinct() %>% as.data.frame()
  ### Municipalities:
  past_df <- netherlandsData %>% group_by(Code) %>% filter(Date <= (netherlandsData$Date[1]-14)) %>%
    group_by(Code) %>% mutate(cases = sum(cases)) %>% filter(Date == Date[1]) %>% distinct() %>% as.data.frame()
  netherlands_data_join <<- ndf %>% inner_join(past_df, by = 'Code', suffix = c("","_past"))
  netherlands_pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
  netherlands_legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99", "No or missing data")
  ### Geojson:
  geomDutch <<- st_read('https://opendata.arcgis.com/datasets/620c2ab925f64ed5979d251ba7753b7f_0.geojson')%>%
    select(Code = Gemeentecode, municipality = Gemeentenaam, pop = Bevolkingsaantal)
  # Note that geomDutch$Bevolkingsaantal is population size.
}

maplabsNetherlands <- function(riskData) {
  riskData <- riskData %>%
    mutate(risk = case_when(
      risk == 100 ~ '> 99',
      risk == 0 ~ '< 1',
      is.na(risk) ~ 'No data',
      TRUE ~ as.character(risk)
    ))
  labels <- paste0(
    "<strong>", paste0('Municipality of ', riskData$municipality), "</strong><br/>",
    "Current Risk Level: <b>",riskData$risk, ifelse(riskData$risk == "No data", "", "&#37;"),"</b><br/>",
    "Latest Update: ", substr(riskData$date, 1, 10)
  ) %>% lapply(htmltools::HTML)
  return(labels)
}

# Calculate risk
calc_risk <- function(I, g, pop) {
  p_I <- I / pop
  r <- 1 - (1 - p_I)**g
  return(round(r*100, 1))
}


######## Create and save daily map widgets ########
size <- 50
asc_bias <- 5
scale_factor <- 10/14

getDataNetherlands()

netherlands_data_Nr <- netherlands_data_join %>%
  mutate(Nr = (cases - cases_past) * asc_bias * scale_factor) 

netherlands_data_map <- geomDutch %>% inner_join(netherlands_data_Nr, by = 'Code')

netherlands_riskdt <- netherlands_data_map %>% 
  mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))
map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lat = 56, lng = 9.3, zoom = 7) %>%
  addPolygons(
    data = netherlands_riskdt,
    color = "#444444", weight = 0.2, smoothFactor = 0.1,
    opacity = 1.0, fillOpacity = 0.7,
    fillColor = ~ netherlands_pal(risk),
    highlight = highlightOptions(weight = 1),
    label = maplabsNetherlands(netherlands_riskdt)
  )
map
