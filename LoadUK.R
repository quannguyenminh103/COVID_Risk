LoadUK <- function() {
  dataQueryUK <- function(date) {
    dataURL <- paste0("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=utla;date=", date, '&structure={"date":"date","code":"areaCode","cases":"cumCasesBySpecimenDate"}')
    response <- httr::GET(
      url = dataURL,
      timeout(10)
    )
    if (response$status_code >= 400) {
      err_msg <- httr::http_status(response)
      stop(err_msg)
    } else if (response$status_code >= 204){
      cur_date <<- date - 1 
      dataURL <- paste0("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=utla;date=", cur_date, '&structure={"date":"date","code":"areaCode","cases":"cumCasesBySpecimenDate"}')
      response <- httr::GET(
        url = dataURL,
        timeout(10)
      )
      
    }
    # Convert response from binary to JSON:
    json_text <- content(response, "text")
    data <- jsonlite::fromJSON(json_text)$data %>%
      mutate(date = as_date(date))
    return(data)
  }
  cur_date <<- ymd(gsub("-", "", Sys.Date())) - 1
  
  data_cur <- dataQueryUK(cur_date)
  past_date <- ymd(cur_date) - 14
  data_past <- dataQueryUK(past_date)
  for (i in c(1:13)) {
    data_cur <- data_cur %>% rbind(dataQueryUK(cur_date - i))
  }
  data_cur <- data_cur %>%
    group_by(code) %>%
    dplyr::summarise(date = first(date), cases = first(cases), n = n())
  
  geom <<- st_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson", stringsAsFactors = FALSE) %>%
    rename(code = ctyua19cd, name = ctyua19nm)
  pop <- read.csv("https://raw.githubusercontent.com/appliedbinf/covid19-event-risk-planner/master/COVID19-Event-Risk-Planner/map_data/uk_pop.csv", stringsAsFactors = FALSE) %>% select(-c("name"))
  data_join <<- data_cur %>%
    inner_join(data_past, by = "code", suffix = c("", "_past")) %>%
    inner_join(pop, by = c("code"))
  head(data_join)
  data_join$Difference <- (data_join$cases - data_join$cases_past)*10/14
  UKMap <- inner_join(geom,data_join,by = 'code')
  UKMap$RegionName = paste0(UKMap$name,", UK")
  UKMap$DateReport = as.character(UKMap$date) 
  UKMap$pInf = UKMap$Difference/UKMap$pop
  UK_DATA = subset(UKMap,select=c("DateReport","RegionName","pInf","geometry"))
  return(UK_DATA)
}