check_plot_type <- function(.xml_node) {
  x <- .xml_node %>%  xml2::xml_child(search = "c:barChart")
  y <- .xml_node %>%  xml2::xml_child(search = "c:lineChart")
  
  if (length(x) > 0) {
    return("barChart")
  } else if (length(y) > 0) {
    return("lineChart")
  } else {
    return(NA)
  }
}




get_diagram_alt_text <- function(.xml) {
  
  if (pluck_exists(.xml, "xdr:graphicFrame")) {
    .xml <- .xml %>%
      pluck("xdr:graphicFrame")
  }
  
  x <- .xml %>%
    pluck("xdr:nvGraphicFramePr")
  
  if (pluck_exists(x, "xdr:cNvPr")) {
    descr <- x  %>%
      pluck("xdr:cNvPr") %>%
      xmlGetAttr("descr")
    n <-  x  %>%
      pluck("xdr:cNvPr") %>%
      xmlGetAttr("name")
    print(n)
    print(descr)
    if (!is.null(descr)) set_names(descr, n) else descr
  } else {
    NA
  }
}
# xml_to_df <- function(path_to_xml) {
#   path_to_xml %>%  xml2::read_xml() %>% as_list() %>% as_tibble %>%  unnest_longer(col = 1)
# }


get_series <- function(.plotarea) {
  
  if (check_plot_type(.plotarea) == "lineChart") {
    .plotarea %>%
      # contains actual values
      xml_child(search = "c:lineChart") %>%
      # one series for each line in the chart
      xml_find_all("c:ser")
    
  } else if (check_plot_type(.plotarea) == "barChart") {
    .plotarea %>%
      # contains actual values
      xml_child(search = "c:barChart") %>%
      # one series for each line in the chart
      xml_find_all("c:ser")
    
  } else if (is.na(check_plot_type(.plotarea))) {
    stop("Unidentified graph")
  }
}



# extract name of variable for the series
get_series_name <- function(.series) {
  .series %>%
    xml_child(search = "c:tx") %>%
    as_list() %>%
    pluck("strRef", "strCache", "pt", "v", 1)
}

get_idx_from_series <- function(.series) {
  .series %>%
    as_list() %>%
    pluck("val", "numRef", "numCache") %>%
    keep(names(.) == "pt") %>%
    map(attributes) %>%
    bind_rows() %>%
    select(idx)
}

get_value_from_series <- function(.series) {
  vals <- .series %>%
    as_list() %>%
    pluck("val") %>%
    as_tibble() %>%
    unnest_longer(col = 1) %>%
    filter(numRef_id == "pt") %>%
    unnest_wider(col = "numRef") %>%
    unnest_longer(col = v) %>%
    select(v)
  
  ids <- get_idx_from_series(.series)
  namn <- get_series_name(.series)
  names(vals) <- namn
  
  bind_cols(vals, ids)
}



#
#
# series[[1]] %>% get_value_from_series()
# series[[2]] %>% get_value_from_series()
# series[[3]] %>% get_value_from_series()
#
# series[[1]] %>% get_idx_from_series()
#
#
# series[[1]]
#
# # extrahera serienamn (tx -> c:v)
# series[[1]] %>%
#   xml_child(search = "c:tx") %>%
#   as_list() %>%
#   pluck("strRef", "strCache", "pt", "v", 1)


get_idx_from_series_cat <- function(.series) {
  tmp <- .series %>%
    xml_child(search = "c:cat") %>%
    as_list()
  
  if (pluck_exists(tmp, "numRef", "numCache")) {
    tmp %>%
      pluck("numRef", "numCache") %>%
      keep(names(.) == "pt") %>%
      map(attributes) %>%
      bind_rows() %>%
      select(idx)
  } else if (pluck_exists(tmp, "strRef", "strCache")) {
    tmp %>%
      pluck("strRef", "strCache") %>%
      keep(names(.) == "pt") %>%
      map(attributes) %>%
      bind_rows() %>%
      select(idx)
    
  }
}


get_sheetname_from_series <- function(.series) {
  tmp2 <- .series %>%
    xml_child(search = "c:cat") %>%
    as_list() %>%
    as_tibble() %>%
    unnest_longer(col = 1) %>%
    filter(if_any(2, is.na)) %>%
    pull(1) %>%
    pluck(1)
  tmp2 %>% str_remove_all("!.*|'")
}

# get_sheetname_from_series(series[[1]])


# extrahera seriekategorier (årtal)
get_cat_from_series <- function(.series) {
  tmpcheck <- .series %>%
    xml_child(search = "c:cat") %>%
    as_list() %>%
    as_tibble()
  
  currname <- get_sheetname_from_series(.series)
  
  
  if (nrow(tmpcheck) == 0) {
    stop(str_glue("Categories do not exist for the current series {currname}. Add them to chart in excel."))
  }
  
  tmpcheck2 <- tmpcheck %>%
    unnest_longer(col = 1) %>%
    filter(if_any(2, ~. == "pt"))
  
  if (nrow(tmpcheck2) == 0) {
    stop(str_glue("pt values does not exist for current series {currname}. This diagram type is currently not supported."))
  }
  
  cats <- tmpcheck %>%
    unnest_longer(col = 1) %>%
    filter(if_any(2, ~. == "pt")) %>%
    unnest_wider(col = 1) %>%
    unnest_longer(col = v) %>%
    select(cat = v)
  
  ids <- get_idx_from_series_cat(.series)
  bind_cols(cats, ids)
}



# get_cat_from_series(series[[1]])


get_table <- function(.series) {
  cats <- get_cat_from_series(.series)
  get_value_from_series(.series) %>%
    left_join(cats, by = "idx")
}





# -----------------------------------------------------------------------------

library(readxl)
library(openxlsx)
library(tidyverse)
library(xml2)
library(XML)

# Load the Excel file
inpath <- ""

wb <- openxlsx::loadWorkbook(inpath)
# wb

# ------------------------------------------------------------------------------
# Extrahera alternativ text för alla grafer i excel

reslista <- list()

for (z in seq_len(length(wb$drawings))[-1]) {
  x <- wb$drawings[[z]] %>%
    XML::xmlParseString()
  print(str_glue("sheet = {z}"))
  tmpres <- list()
  for (k in seq_len(xmlSize(x))) {
    res <- xmlChildren(x)[[k]] %>% get_diagram_alt_text()
    tmpres[[k]] <- res
    print(res)
  }
  
  reslista[[z]]  <- tmpres %>%
    purrr::discard(.p = ~ is.na(.)) %>%
    unlist() %>%
    enframe() %>%
    mutate(sheet = wb$sheet_names[z])
  
}

titlar_df <- reslista[-1] %>%
  map(~ .x %>% mutate(across(everything(), as.character))) %>%
  bind_rows() %>%
  select(sheet, name, value)

# ------------------------------------------------------------------------------
# Extract data from charts in excel 

# alla som har _rels är inte grafer, kan ignoreras
grafer <- wb$charts %>% str_subset("_rels", negate=T)
reslist <- vector("list", length = length(grafer))
#reslist <- list()

for (i in seq_len(length(grafer))) {
  #for (i in 16) {
  
  print(str_glue("{i}/{length(grafer)}"))
  x <- grafer[[i]] %>% xml2::read_xml()
  plotarea <- x %>%
    xml2::xml_child(search = "c:chart") %>%
    xml2::xml_child(search = "c:plotArea")
  
  if (is.na(plotarea)) {
    warning("No plotArea")
    next
  }
  
  series <- get_series(plotarea)
  
  #print("Extracting data from chart")
  tmp <- tibble()
  #i <- 2
  for (j in seq_along(series)) {
    z <- get_table(series[[j]])
    
    if (nrow(tmp)==0 | (nrow(z) > nrow(tmp)) ) {
      tmp <- bind_rows(tmp, z)
    } else {
      tmp <- left_join(tmp, z, by = c("cat", "idx"))
    }
  }
  
  tmp <- tmp %>% arrange(cat)
  if (nrow(tmp) == 0) warning("No rows")
  tmp <- tmp %>% mutate(sheetname = get_sheetname_from_series(series[[1]]))
  
  reslist[[i]] <- tmp
  
  #print("Curr data:")
  #print(reslist[[i]])
}

# reslist
# series[[1]]


df <- titlar_df %>%
  mutate(data = reslist)







