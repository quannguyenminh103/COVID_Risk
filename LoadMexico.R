LoadMexico <- function(){

geomMEX = st_read("MEXICO_municip.geojson")
#for joins see map on Mexican health website: https://datos.covid-19.conacyt.mx/fHDMap/mun.php

CH = st_union( geomMEX[c(1815,1821),] ) %>% st_cast("MULTIPOLYGON")
geomMEX$geometry[1815]  = CH
CH = st_union( geomMEX[c(172,203),] ) %>% st_cast("MULTIPOLYGON")
geomMEX$geometry[172]  = CH
CH = st_union( geomMEX[c(160,199),] ) %>% st_cast("MULTIPOLYGON")
geomMEX$geometry[160]  = CH
CH = st_union( geomMEX[c(200,152),] ) %>% st_cast("MULTIPOLYGON")
geomMEX$geometry[152]  = CH
CH = st_union( geomMEX[c(201,186),] ) %>% st_cast("MULTIPOLYGON")
geomMEX$geometry[186]  = CH
CH = st_union( geomMEX[c(202,107),] ) %>% st_cast("MULTIPOLYGON")
geomMEX$geometry[107]  = CH
CH = st_union( geomMEX[c(933,914),] ) %>% st_cast("MULTIPOLYGON")
geomMEX$geometry[914]  = CH
CH = st_union( geomMEX[c(934,916),] ) %>% st_cast("MULTIPOLYGON")
geomMEX$geometry[916]  = CH


#Main COVID-19 hub page: https://datos.covid-19.conacyt.mx/#DownZCSV
#need to try 2 days if it doesn't work!

flag=0
aa=0
while(flag==0){
	DATE = Sys.Date()-aa
	formDATE = format(DATE, "%Y%m%d")
	STRING = paste0("https://datos.covid-19.conacyt.mx/Downloads/Files/Casos_Diarios_Municipio_Confirmados_",formDATE)
	MEX<- try(read.csv(STRING,encoding="UTF-8"))  #note older files are DELETED.
	if (is.null(dim(MEX)) == FALSE){
		flag=1
	}else{
		aa=aa+1
	}
	if(aa>5){
		warning("no recent data")
		flag=2
	}
}


NUMCHARS = nchar(as.character(MEX$cve_ent))
IND = which(NUMCHARS==4)


DataJoin = c()
SZ = dim(MEX)
DataJoin$Name = MEX$nombre
NAME = strsplit(names(MEX)[SZ[2]],"X")
DataJoin$pop = MEX$poblacion
DataJoin$DateReport = as.Date(NAME[[1]][2],"%d.%m.%Y")
DataJoin$CaseDiff = rowSums(MEX[,(SZ[2]-14):(SZ[2])])/14*10
DataJoin$pInf = DataJoin$CaseDiff/DataJoin$pop
DataJoin$CVE = as.character(MEX$cve_ent)
DataJoin$CVE[IND] =  paste0("0",as.character(MEX$cve_ent[IND]))

DATA = as.data.frame(DataJoin)


HAM = inner_join(geomMEX, DATA, by = c("CVEGEO" = "CVE"))
HAM$RegionName = paste0(HAM$NOMGEO,", ",HAM$estado)
HAM$DateReport = as.character(HAM$DateReport)
MEXICO_DATA = subset(HAM,select=c("DateReport","RegionName","pInf","geometry"))


return(MEXICO_DATA)
}