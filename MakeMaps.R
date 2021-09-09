# Load packages
#install.packages(rmapshaper)
#source files
source('libraries.R')
source("LoadCanada.R")
source("LoadEurope.R")
source("LoadMexico.R")
source("LoadNetherlands.R")
source("LoadGermany.R")
source("LoadNorway.R")
source("LoadBelgium.R")
source("LoadJapan.R")
source("LoadChina.R")
source("LoadIndia.R")
source("LoadBrazil.R")
source("LoadAustralia.R")
source("LoadCzechia.R")
source("LoadSpain.R")
source("LoadUK.R")
source("LoadSweden.R")
source("LoadFrance.R")
source("LoadAustria.R")
source("LoadDanish.R")
source("LoadIreland.R")
source("LoadItaly.R")
source("LoadSwiss.R")
source("LoadUS.R")
#Load in data for Canada
CANADA = LoadCanada()
#Load in data for Mexico
MEXICO = LoadMexico()
#Load in data for Europe
EUROPE = LoadEurope()
#Load in data for Netherlands
NETHERLANDS = LoadNetherlands()
#Load in data for Germany
GERMANY = LoadGermany()
#Load in data for Norway
NORWAY = LoadNorway()
#Load in data for Belgium
BELGIUM = LoadBelgium()
#Load in data for Japan
JAPAN = LoadJapan()
#Load in data for China
CHINA = LoadChina()
#Load in data for India
INDIA = LoadIndia()
#Load in data for Brazil
BRAZIL = LoadBrazil()
#Load in data for Australia
AUSTRALIA = LoadAustralia()
#Load in data for Spain
SPAIN = LoadSpain()
#Load in data for UK
UK = LoadUK()
#Load in data for Sweden
SWEDEN = LoadSweden()
#Load in data for France
FRANCE = LoadFrance()
#Load in data for Austria
AUSTRIA = LoadAustria()
#Load in data for Denmark
DENMARK = LoadDanish()
#Load in data for Ireland
IRELAND = LoadIreland()
#Load in data for Czech
CZECH = LoadCzechia()
#Load in data for Italy
ITALY = LoadItaly()
#Load in data for Swiss
SWISS = LoadSwiss()
#Load in data for US
US = LoadUS()
#Combine Spatial Datasets
NEWMAP = rbind(CANADA,MEXICO,EUROPE,NETHERLANDS,GERMANY,NORWAY,BELGIUM,JAPAN,CHINA,INDIA,BRAZIL,AUSTRALIA,SPAIN,
               UK,SWEDEN,FRANCE,AUSTRIA,DENMARK,IRELAND,CZECH,ITALY,SWISS,US)
#NEWMAP = ms_simplify(NEWMAP) #simplifies spatial polygons reducing output file size
#calculate risk
estRisk <- function(Infected,A,G){
  Risk = 100*(1-(1-(A*Infected))^G)
  return(round(Risk,0))
}

#create risk-maps
EventMap <- function(DATA,A,G){ #map data, Ascertainty bias, Group size
  
  DATA$risk <- estRisk(DATA$pInf,A,G) 
  MMap = DATA
  MMap$riskLabels <- MMap$risk
  MMap <- MMap %>%
    mutate(riskLabels = case_when(
      riskLabels > 99 ~ '> 99',
      riskLabels < 1 ~ '< 1',
      riskLabels < 0 ~ 'NA',
      is.na(riskLabels) ~ 'No Data',
      TRUE ~ as.character(riskLabels)
    ))
  bins <- c(0,1,25,50,75,99,100)
  legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99")
  pal <- colorBin("YlOrRd", domain = MMap$risk, bins = bins, na.color = 'grey')
  JAM = leafletOptions(worldCopyJump=TRUE)
  
  labels <- sprintf(
    "<strong>Region: %s</strong><br/>Risk Score: %s%%<br/>Updated Date: %s<br/>",
    MMap$RegionName, MMap$riskLabels,MMap$DateReport
  ) %>% lapply(htmltools::HTML)
  leaflet(MMap,options=JAM) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = MMap,
                fillColor = ~pal(risk),
                weight = 0.1,
                opacity = 1.0,
                color = "black",
                dashArray = "2",
                fillOpacity = 1.0,smoothFactor=0.1,
                highlight = highlightOptions(
                  weight = 0.5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 1.0,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
    addLegend(pal = pal, values = ~risk, opacity = 0.9, title = 'Risk Level (%)', na.label = 'NA',
              position = "bottomright",labFormat = function(type, cuts, p) {
                paste0(legendlabs)
              })
}

#plot map

MapTogether = EventMap(NEWMAP,5,100)
MapTogether
#save as widget
saveWidget(MapTogether,"event5-100.html",selfcontained=F) 
