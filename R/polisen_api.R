library(httr)
library(tidyverse)
library(lubridate)
library(jsonlite)
# install.packages("leaflet")
library(leaflet)

get_polisen <- function(.date = NULL, .location=NULL, .events = NULL, .preprocess = T) {
  base_url <- "https://polisen.se/api/events?"
  url <- base_url
  if (!is.null(.events)) {
    events <- str_c(.events, collapse = ";")
    url <- str_c(base_url, "type=", events)
  }
  if (!is.null(.date)) {
    url <- str_c(url, "&DateTime=", .date)
  }
  if (!is.null(.location)) {
    locations <- str_c(.location, collapse = ";")
    url <- str_c(url, "&locationname=", locations)
  }
  
  resp <- httr::GET(as.character(url))
  
  if (httr::http_status(resp)$category != "Success") {
    print(resp)
    stop("Http status not success")
  }
  
  df <- resp %>%
    content(as = "text") %>%
    jsonlite::fromJSON() %>%
    as_tibble()
  
  if (.preprocess & nrow(df) != 0) {
    df <- df %>% 
      mutate(datetime = lubridate::as_datetime(datetime, tz = "Europe/Stockholm"),
             url = str_glue("https://polisen.se{url}"),
             summary2 = str_glue("{summary}\n<a href=\"{url}\">Mer info</a>")
      )
    # separate coordinates into two columns, rename duplicate column name
    df$location <- df$location %>% 
      as_tibble() %>% 
      separate(gps, c("lat", "lng"), sep = ",", convert = T) %>% 
      dplyr::rename(location_name = name) %>% 
      # add some noise to coordinates, since all coordinates for a given location are the same.
      # otherwise the points will overlap on the map.
      # the coordinates only show the coordinate of an area, never an exact location
      mutate(lat = jitter(lat), lng = jitter(lng) )
    
    df <- df %>%
      unnest(location)
  } 
  return(df)
}

# get all events:
get_polisen()
# Filter by a vector of events
get_polisen(.date = "2022-06", .location = "Varberg", .events = c("Rån", "Trafikolycka"))
# filter by a vector of locations
get_polisen(.date = "2022-04", .location = c("Varberg","Stockholm"))
# filter by year, year-month or year-month-day:
get_polisen(.date = "2022-06-05", .location = "Linköping")
# # A tibble: 1 x 9
# id datetime            name                                  summary          url                 type   location_name   lat   lng
# <int> <dttm>              <chr>                                 <chr>            <glue>              <chr>  <chr>         <dbl> <dbl>
# 342083 2022-06-05 11:31:12 05 juni 11:31, Rattfylleri, Linköping Trafikant uppmä~ https://polisen.se~ Rattf~ Linköping      58.4  15.6



get_polisen(.date = "2022-07", .location = "Uppsala")%>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~lng, lat = ~lat, popup = ~summary2, label = ~name)



get_polisen(.date = "2022-07", .location = "Östergötland", .preprocess = F)
get_polisen(.date = "2022-06", .location = "Östergötlands") %>% view
get_polisen(.date = "2022-06", .location = "Upplands")
get_polisen(.date = "2022-06", .location = "Uppsala")




