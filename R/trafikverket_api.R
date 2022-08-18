library(tidyverse)
library(httr)
library(leaflet)

# https://api.trafikinfo.trafikverket.se/Console
# https://developer.trafiklab.se/node/33092/keys

api_key <- read_lines(file = "./R/trafikverket_apikey.txt") # only local key, not commited
url <- "https://api.trafikinfo.trafikverket.se/v2/data.json"

# ------------------------------------------------------------------------------
# download icons from trafikverket

downloadicons <- FALSE

if (downloadicons) {
  icon_query <- query <-
    str_glue(
      '
  <REQUEST>
      <LOGIN authenticationkey="{api_key}" />
      <QUERY objecttype="Icon" schemaversion="1.0">
            <INCLUDE>Deleted</INCLUDE>
            <INCLUDE>Description</INCLUDE>
            <INCLUDE>Id</INCLUDE>
            <INCLUDE>ModifiedTime</INCLUDE>
            <INCLUDE>Url</INCLUDE>
      </QUERY>
 </REQUEST>
')
  resp <- httr::content(httr::POST(url = url,
                                   body = query,
                                   encode = "form",
                                   content_type_xml()
  ), as = "text")
  
  list_json <- jsonlite::fromJSON(resp)
  
  #glimpse(list_json$RESPONSE$RESULT$Situation)
  icons <- list_json$RESPONSE$RESULT$Icon[[1]] %>% 
    as_tibble(validate=F) 
  
  save(icons, file = "./R/RData/icons_trafikverket.RData")
} else {
  load(file = "./R/RData/icons_trafikverket.RData")
}


# ------------------------------------------------------------------------------
# vägrelaterade händelser inom 1 mils radie från specifierad punkt

# y <- "320011"
# x <- "6398983"
get_traffic_info <- function(.x, 
                             .y, 
                             .radius = 10000, # 10 000 meters from coordinates
                             .api_key = api_key, 
                             .url = "https://api.trafikinfo.trafikverket.se/v2/data.json",
                             .preprocess = T) {
  
  query <-
    str_glue(
      '
  <REQUEST>
      <LOGIN authenticationkey="{.api_key}" />
      <QUERY objecttype="Situation" schemaversion="1.2">
            <FILTER>
                  <WITHIN name="Deviation.Geometry.SWEREF99TM" shape="center" value="{.y} {.x}" radius="{.radius}" />
            </FILTER>
            <INCLUDE>Deviation.Header</INCLUDE>
            <INCLUDE>Deviation.IconId</INCLUDE>
            <INCLUDE>Id</INCLUDE>
            <INCLUDE>ModifiedTime</INCLUDE>
            <INCLUDE>PublicationTime</INCLUDE>
            <INCLUDE>Deviation.SeverityCode</INCLUDE>
            <INCLUDE>Deviation.Geometry.SWEREF99TM</INCLUDE>
            <INCLUDE>Deviation.Geometry.WGS84</INCLUDE>
            <INCLUDE>Deviation.CountyNo</INCLUDE>
            <INCLUDE>Deviation.IconId</INCLUDE>
            <INCLUDE>Deviation.SeverityText</INCLUDE>
            <INCLUDE>Deviation.StartTime</INCLUDE>
            <INCLUDE>Deviation.PositionalDescription</INCLUDE>
            <INCLUDE>Deviation.RoadNumber</INCLUDE>
            <INCLUDE>Deviation.Message</INCLUDE>
            <INCLUDE>Deviation.MessageCode</INCLUDE>
            <INCLUDE>Deviation.MessageType</INCLUDE>
            <INCLUDE>Deviation.TemporaryLimit</INCLUDE>
            <INCLUDE>Deviation.TrafficRestrictionType</INCLUDE>
            <INCLUDE>Deviation.ValidUntilFurtherNotice</INCLUDE>
            <INCLUDE>Deviation.WebLink</INCLUDE>
      </QUERY>
 </REQUEST>
')
  
  resp <- httr::POST(url = .url,
                                   body = query,
                                   encode = "form",
                                   content_type_xml()
  )
  
  if (httr::http_status(resp)$category != "Success") {
    print(resp)
    stop("Http status not success")
  }
  
  list_json <- content(resp, as = "text") %>% jsonlite::fromJSON()

  res <- list_json$RESPONSE$RESULT$Situation[[1]] %>%
    as_tibble(validate=F) %>% 
    unnest(Deviation) %>% 
    unnest(CountyNo) %>% 
    distinct() %>% 
    unnest(Geometry)
  
  if (.preprocess & nrow(res) != 0) {
    # extract latitude/longitude and create summary box for each coordinate
    res <- res %>% 
      mutate(WGS84 = str_replace_all(WGS84, "^.*\\((.*)\\)$", "\\1") ) %>% 
      mutate(StartTime = lubridate::as_date(StartTime)) %>% 
      mutate(messagec = ifelse(MessageCode == MessageType, MessageCode, str_glue("{MessageType} - {MessageCode}"))) %>% 
      mutate(content = str_glue("<b>{ifelse(!is.na(RoadNumber),RoadNumber, '')} 
  {messagec} </b></br>
  Started: {StartTime} </b></br>
                            {SeverityText}. 
                            {ifelse(!is.na(TrafficRestrictionType),TrafficRestrictionType,'')}
                            {ifelse(!is.na(TemporaryLimit),TemporaryLimit,'')}</br>
                            <i>{ifelse(!is.na(Message),Message, '')}</i>")) %>% 
      separate(WGS84, into = c("lng", "lat"), sep = " ", convert = T)
    
    icons_tmp <- icons %>% 
      select(Id, Icon_url = Url)
    # join icon-urls
    res <- res %>% 
      left_join(icons_tmp, by = c("IconId" = "Id")) 
  }
  
  return(res)
}

convert_sweref99_to_wgs84 <- function(.x, .y){
  # SWEREF 99
  sweref99 <- sf::st_sfc(sf::st_point(c(.x, .y), dim = "XY"), crs = 3006)
  wgs84 <- sf::st_transform(sweref99, crs = 4326)
  sf::st_coordinates(wgs84)
}
convert_sweref99_to_wgs84(6398983, 320011)[1]


plot_traffic <- function(.data, .x, .y, .icondim = 16) {
  a <- convert_sweref99_to_wgs84(.x = .x, .y = .y) %>% as_tibble()
  
  show_icons <- 
    leaflet::icons(iconUrl = .data$Icon_url, iconWidth = .icondim, iconHeight = .icondim, iconAnchorX = 0, iconAnchorY = 0)
  
    leaflet() %>%
    addTiles() %>%
    addMarkers(data = .data, lng = ~lng, lat = ~lat, popup = ~content, label = ~Message, icon = show_icons) #%>% 
    #addMarkers(data = a, lng = ~X, lat = ~Y) 
}

x <- get_traffic_info(.x = "6398983", .y = "320011")
x %>% 
  plot_traffic(.x = 6398983, .y = 320011)

avkontr <- "L:/Myndigheter/Trafikverket/TRV_5161_Skattning hastighetsindex/Document/Memo/Månadsrapport-Avvikelsekontroll_2022.xlsx"

av <- readxl::read_excel(avkontr)

av <- av %>% 
  filter(Månad == "Jul")

av

x[1,]$SWEREF99TM
x[1,]$lng
x[1,]$lat
# 313975.98 6405213.04
# lng 11.9  lat 57.8

convert_sweref99_to_wgs84(313975.98, 6405213.04) %>% as_tibble()


