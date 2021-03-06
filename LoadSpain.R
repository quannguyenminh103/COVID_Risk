LoadSpain <- function(){
#county geojson data
geomSPAIN <- st_read('https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/spain-provinces.geojson')

#Main COVID-19 hub page: https://cnecovid.isciii.es/covid19/#distribuci%C3%B3n-geogr%C3%A1fica
SPAIN<- read.csv("https://cnecovid.isciii.es/covid19/resources/casos_tecnica_provincia.csv",na.strings=FALSE) 

#code link file
Spaincode = read.csv("./spain_codenames.csv",encoding="UTF-8",na.strings=FALSE)
#Population data comes from  Instituto Nacional de Estadística: https://www.ine.es/jaxiT3/Datos.htm?t=2852#!tabs-tabla

DataJoin = c()
Counties <- unique(SPAIN$provincia_iso)
DataJoin$ProvinceName = Counties
for(aa in 1:length(Counties)){
	Subset = SPAIN[SPAIN$provincia_iso==Counties[aa],]
	Dates = as.Date(Subset$fecha)
	LL = length(Dates)
	ConfirmedCovidCases = cumsum(Subset$num_casos)
	#CaseDiff = 10*( Subset$ConfirmedCovidCases[LL] - Subset$ConfirmedCovidCases[LL - 14])/ 14
	CaseDiff  = ConfirmedCovidCases[LL] - ConfirmedCovidCases[LL - 14]
	#Make sure difference in cases is positive. If not set to NA.
	if(CaseDiff<0){
		CaseDiff = NA
	}
	DataJoin$LatestTime[aa] = as.character(Dates[LL])
	DataJoin$CaseDiff[aa] = CaseDiff
}


Spaindata = as.data.frame(DataJoin)
Spaincode = as.data.frame(Spaincode)
Spaindf = inner_join(Spaindata,Spaincode,by=c("ProvinceName"="code"))
SpainMap <- inner_join(geomSPAIN,Spaindf, by = "name")

SpainMap$RegionName = paste0(SpainMap$name,", Spain")
SpainMap$DateReport = as.character(SpainMap$LatestTime) 
SpainMap$pInf = SpainMap$CaseDiff/SpainMap$population2019
SPAIN_DATA = subset(SpainMap,select=c("DateReport","RegionName","pInf","geometry"))
return(SPAIN_DATA)
}