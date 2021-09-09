getDataBelgium <- function() {
  getDate <<- function(x){
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
    updated_data <- data.frame(Arrondissement = as.character(), cases = as.numeric())
    for (i in 1:length(Arrondissement)){
      data <- dataSet %>% filter(dataSet$TX_ADM_DSTR_DESCR_NL == Arrondissement[i])
      if (sum(data$CASES == '<5') > 0){
        data[data$CASES == '<5','CASES'] <- 0 ## assume there is no case when the box shows <5
      }
      temp <- data.frame(Arrondissement = unlist(strsplit(Arrondissement[i],'Arrondissement '))[2],cases = sum(as.numeric(data$CASES)))
      updated_data <-rbind(updated_data,temp)
    }
    return(updated_data)
  }
  latest_update <- getData(latest_data)
  past_update <- getData(past_data)
  
  belgium_data_join <<- latest_update %>% inner_join(past_update, by = 'Arrondissement', suffix = c("", "_past"))
  belgium_pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
  belgium_legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99", "No or missing data")
  
}
  # Calculate risk
calc_risk <- function(I, g, pop) {
    p_I <- I / pop
    r <- 1 - (1 - p_I)**g
    return(round(r * 100, 1))
}


getDataBelgium()
######## Create and save daily map widgets ########
size <- 50
asc_bias = 3
scale_factor = 10/14
belgium_data_Nr <- belgium_data_join %>%
  mutate(Nr = (cases - cases_past) * asc_bias * scale_factor)

# geojson file
geomBelgium <- st_read('BE_geom.geojson')
geomBelgium <- geomBelgium[,c("NameDUT","geometry")]
for (i in 1:length(geomBelgium$NameDUT)){
  geomBelgium$NameDUT[i] <- unlist(strsplit(geomBelgium$NameDUT[i], "Arrondissement "))[2]
}
geomBelgium$NameDUT[28] <- belgium_data_Nr$Arrondissement[22] # La Louviere
geomBelgium$NameDUT[39] <- belgium_data_Nr$Arrondissement[29] # Neufchâteau
belgium_data_Nr <- inner_join(geomBelgium, belgium_data_Nr, by = c('NameDUT' = 'Arrondissement'))
names(belgium_data_Nr)[1] <- 'Arrondissement'
## CHANGE NAMES IN THIS DATASET SO THAT THEY HAVE THE SAME NAME AS POPULATION file:
belgium_data_Nr$Arrondissement[belgium_data_Nr$Arrondissement == "Aarlen"] <- 'Arlon'
belgium_data_Nr$Arrondissement[belgium_data_Nr$Arrondissement == "Aat"] <- 'Ath'
belgium_data_Nr$Arrondissement[belgium_data_Nr$Arrondissement == "Bastenaken"] <- 'Bastogne'
belgium_data_Nr$Arrondissement[belgium_data_Nr$Arrondissement == "Bergen"] <- 'Mons'
belgium_data_Nr$Arrondissement[belgium_data_Nr$Arrondissement == "Borgworm"] <- 'Waremme'
belgium_data_Nr$Arrondissement[belgium_data_Nr$Arrondissement == "Doornik-Moeskroen"] <- 'Tournai-Mouscron'
belgium_data_Nr$Arrondissement[belgium_data_Nr$Arrondissement == "Hoei"] <- 'Huy'
belgium_data_Nr$Arrondissement[belgium_data_Nr$Arrondissement == "Namen"] <- 'Namur'
belgium_data_Nr$Arrondissement[belgium_data_Nr$Arrondissement == "Nijvel"] <- 'Nivelles'
belgium_data_Nr$Arrondissement[belgium_data_Nr$Arrondissement == "Brussel-Hoofdstad"] <- 'Bruxelles-Capitale'
belgium_data_Nr$Arrondissement[belgium_data_Nr$Arrondissement == "Zinnik"] <- 'Soignies'

## POPULATION FILE:
pop <- read.csv('belgiumPopulation.csv')
pop$Name[13] = 'Luik'

belgiumdf <- inner_join(belgium_data_Nr,pop, by = c('Arrondissement' = 'Name'))


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


belgium_riskdt_map <- belgiumdf %>%
  mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, Population)), 0))
maplabsBelgium <- function(riskData) {
  riskData <- riskData %>%
    mutate(risk = case_when(
      risk == 100 ~ '> 99',
      risk == 0 ~ '< 1',
      is.na(risk) ~ 'No data',
      TRUE ~ as.character(risk)
    ))
  labels <- paste0(
    "<strong>County ", riskData$Arrondissement, "</strong><br/>",
    "Current Risk Level: <b>",riskData$risk, ifelse(riskData$risk == "No data", "", "&#37;"),"</b><br/>",
    "Latest Update: ", Sys.Date()
  ) %>% lapply(htmltools::HTML)
  return(labels)
}


map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lat = 48.6, lng = 7.17, zoom = 4) %>%
  addPolygons(
    data = belgium_riskdt_map,
    color = "#444444", weight = 0.2, smoothFactor = 0.1,
    opacity = 1.0, fillOpacity = 0.7,
    fillColor = ~ belgium_pal(risk),
    highlight = highlightOptions(weight = 1),
    label = maplabsBelgium(belgium_riskdt_map)
  )

