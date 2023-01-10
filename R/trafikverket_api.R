library(tidyverse)
library(httr)
library(leaflet)

# https://api.trafikinfo.trafikverket.se/Console
# https://developer.trafiklab.se/node/33092/keys

getwd()
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
      mutate(messagec = ifelse(MessageCode == MessageType, MessageCode, str_glue("{MessageType} - {MessageCode}")))
    
    res <- res %>% 
      mutate(content = str_glue("<b>{messagec}</b></br><b>Started: {StartTime} </b></br>"))
    
    
    
    if (any(names(res) %in% c("RoadNumber"))) {
      #res <- res %>% mutate(content = str_c(content, "<b>{RoadNumber}</b>"))
      res <- res %>% mutate(content = ifelse(!is.na(RoadNumber),
                                             str_c(content, "<b>",RoadNumber, "</b>. "), 
                                             content
      )
      )
    }
    if (any(names(res) %in% c("SeverityText"))) {
      #res <- res %>% mutate(content = str_c(content, SeverityText, ". "))
      res <- res %>% mutate(content = ifelse(!is.na(SeverityText),
                                             str_c(content, SeverityText, ". "), 
                                             content
      )
      )
    }
    if (any(names(res) %in% c("TrafficRestrictionType"))) {
      #res <- res %>% mutate(content = str_c(content, TrafficRestrictionType, ". "))
      res <- res %>% mutate(content = ifelse(!is.na(TrafficRestrictionType),
                                             str_c(content, TrafficRestrictionType, ". "), 
                                             content
      )
      )
    }
    if (any(names(res) %in% c("TemporaryLimit"))) {
      #res <- res %>% mutate(content = str_c(content, TemporaryLimit, ".</br>"))
      res <- res %>% mutate(content = ifelse(!is.na(TemporaryLimit),
                                             str_c(content, TemporaryLimit, ".</br>"), 
                                             content
      )
      )
    }
    
    if (any(names(res) %in% c("Message"))) {
      res <- res %>% mutate(content = ifelse(!is.na(Message),
                                             str_c(content, "<i>", Message, "</i>","."), 
                                             content
                                             )
                            )
    }

    res <- res  %>% 
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


plot_traffic <- function(.data, x, y, .icondim = 16) {
  a <- convert_sweref99_to_wgs84(.x = x, .y = y) %>% as_tibble()
  
  show_icons <- 
    leaflet::icons(iconUrl = .data$Icon_url, iconWidth = .icondim, iconHeight = .icondim, iconAnchorX = 0, iconAnchorY = 0)
  
  if (nrow(.data) != 0) {
    leaflet() %>%
    addTiles() %>%
    addMarkers(data = .data, lng = ~lng, lat = ~lat, popup = ~content, label = ~Message, icon = show_icons) %>% 
    addMarkers(data = a, lng = ~X, lat = ~Y, popup = "Slang") 
  } else {
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = a, lng = ~X, lat = ~Y, popup = "Slang") 
  }
    
}

# 
# x <- get_traffic_info(.x = "6232141", .y = "348334")
# x
# x$content
# x$Message %>% enframe

# 
# x %>% 
#   plot_traffic(313975.98, 6405213.04)


# x[1,]$SWEREF99TM
# x[1,]$lng
# x[1,]$lat
# # 313975.98 6405213.04
# # lng 11.9  lat 57.8
# convert_sweref99_to_wgs84(313975.98, 6405213.04) %>% as_tibble()
# 
# leaflet() %>% 
#   addTiles() %>% 
#   addMarkers(data = convert_sweref99_to_wgs84(313975.98, 6405213.04) %>% as_tibble(),
#              lat = ~Y, lng = ~X, popup = "Slang")


# -----



library(shiny)
library(lubridate)

avkontr <- "L:/Myndigheter/Trafikverket/TRV_5161_Skattning hastighetsindex/Document/Memo/Månadsrapport-Avvikelsekontroll_2022.xlsx"

manad <- lubridate::now() %>% lubridate::month()
manad <- (manad-1) %>% month(label = T) %>% str_to_title()


av1 <- readxl::read_excel(avkontr)

av <- av1 %>% 
  filter(Månad == manad) %>% 
  filter(is.na(Bortfallsvikt) | Bortfallsvikt == 0) %>% 
  filter(is.na(Kontrolldatum))


radio_choices <- av$PunktNr %>% unique %>%  set_names()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Hastighetsindex"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio", 
                   h3("Punktnummer"),
                   choices = radio_choices, selected = 9539)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("txt"),
      tableOutput("table"),
      leafletOutput("plotSlang"),
      leafletOutput("trafikPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$txt <- renderText({
    x <- av %>%
      filter(PunktNr == input$radio)
    str_glue("You have selected {input$radio}
             Vägnr: {x$VägNr} 
             Län: {x$Län}
             Hastighet: {x$Hastighet}
             ÅDT: {x$ÅDT}
             
             ")
  })
  
  output$table <- renderTable({
    av1 %>% 
      filter(Månad != manad  & !is.na(Kommentar) & PunktNr == input$radio) %>% 
      select(Månad, Kommentar)
  })
  
  output$plotSlang <- renderLeaflet({
    x <- av %>%
      filter(PunktNr == input$radio)
    
    leaflet() %>% 
      addTiles() %>% 
      addMarkers(data = convert_sweref99_to_wgs84(x$`Y-koord`, x$`X-koord`) %>% as_tibble(),
                 lat = ~Y, lng = ~X, popup = "Slang")
  })
  
  output$trafikPlot <- renderLeaflet({
    # generate bins based on input$bins from ui.R
    x <- av %>%
      filter(PunktNr == input$radio)
    # 
    y <- get_traffic_info(.x = as.character(x$`X-koord`), .y = as.character(x$`Y-koord`))
    if (nrow(y)>0) {
      y %>%
        plot_traffic( x$`Y-koord`,x$`X-koord`)
      
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

# TODO lägg till knapp 'Reload/Refresh' som laddar excelfilen på nytt 

get_traffic_info()

get_polisen(.date = "2022-11", .location = "Stockholm")

av  %>%
  filter(PunktNr == 9539)





