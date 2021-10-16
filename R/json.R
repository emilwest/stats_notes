library(httr)
library(jsonlite)


url <- "https://pxweb.nordicstatistics.org:443/api/v1/en/Nordic Statistics/Geography and climate/Land use/DENS01.px"
url <- URLencode(url) # convert whitespace to ascii %20

query <- '{
  "query": [
    {
      "code": "time",
      "selection": {
        "filter": "item",
        "values": [
          "2016",
          "2017",
          "2018",
          "2019"
        ]
      }
    }
  ],
  "response": {
    "format": "px"
  }
}'

r <- POST(url, body=query)
px <- content(r, type="text", encoding = "Windows-1252")

library(pxR)
pxR::read.px(px, encoding = "Windows-1252")

