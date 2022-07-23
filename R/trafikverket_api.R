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
      mutate(messagec = ifelse(MessageCode == MessageType, MessageCode, str_glue("{MessageType} - {MessageCode}"))) %>% 
      mutate(content = str_glue("<b>{ifelse(!is.na(RoadNumber),RoadNumber, '')} 
  {messagec} </b></br>
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

plot_traffic <- function(.data, .icondim = 16) {
  show_icons <- 
    leaflet::icons(iconUrl = .data$Icon_url, iconWidth = .icondim, iconHeight = .icondim, iconAnchorX = 0, iconAnchorY = 0)
  
  .data %>% 
    leaflet() %>%
    addTiles() %>%
    addMarkers(lng = ~lng, lat = ~lat, popup = ~content, label = ~Message, icon = show_icons)
}


x <- get_traffic_info(.x = "6398983", .y = "320011")
x %>% 
  plot_traffic()

