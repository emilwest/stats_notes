library(lubridate)
library(tidyverse)

generate_yearly_dfs <- function(.yr = lubridate::year(lubridate::now()),
                                .by = "1 days") {
  
  if (length(.yr) == 1) {
    first_day <- str_glue("{.yr}-01-01")
    last_day <- str_glue("{.yr}-12-31")
  } else if (length(.yr) > 1) {
    first_day <- str_glue("{first(.yr)}-01-01")
    last_day <- str_glue("{last(.yr)}-12-31")
  }
  
  x <- tibble(
    datum = seq(lubridate::ymd(first_day), lubridate::ymd(last_day), by = .by),
    yr = lubridate::year(datum),
    m = lubridate::month(datum, label = T),
    w = lubridate::isoweek(datum), # week 1 starts on Jan 1st
    isow = lubridate::isoweek(datum), # week 1 starts on first Monday of the year 
    d = lubridate::day(datum),
    day = lubridate::wday(datum, label = T)
  ) 
  
  # weekly summary
  weekly_summary <- x %>%
    filter(!(m == "jan" & w == 52)) %>% 
    filter(!(m == "dec" & w == 1)) %>% 
    filter(!(w == 53)) %>% 
    group_by(yr,  isow) %>%
    mutate(id = row_number()) %>%
    filter(datum == min(datum) | datum == max(datum)) %>%
    mutate(daypart = str_glue("{d} {m}")) %>%
    summarise(period = str_c(daypart, collapse = " - ")) %>%
    ungroup()
  
  
  return(list(
    year_df = x,
    weekly_summary = weekly_summary
  ))
}

# --------------------------------------------
#  generate running training planner template

# generate_yearly_dfs()$year_df %>%
#   filter(!(m == "jan" & w == 52)) %>% 
#   filter(!(w == 53)) %>% 
#   group_by(yr, m,  isow) %>%
#   mutate(id = row_number()) %>%
#   filter(datum == min(datum) | datum == max(datum)) %>%
#   mutate(daypart = str_glue("{d} {m}")) %>%
#   summarise(period = str_c(daypart, collapse = " - "), month = unique(m)) %>%
#   ungroup()


calendar_view <- generate_yearly_dfs(2022:2025)$year_df %>% 
  select(-d,-w) %>% 
  group_by(yr, m, isow) %>% 
  pivot_wider(names_from = day, values_from = datum) %>% 
  select(yr, m, isow, mån:fre, lör, sön)

x <- generate_yearly_dfs(2022:2025)$weekly_summary %>% 
  mutate(`Pass 1` = NA, `Pass 2` = NA, `Pass 3` = NA, `Pass 4` = NA, `Pass 5` = NA, `Summa km` = NA) %>% 
  select(Vecka = isow, `Pass 1`:`Pass 5`, `Summa km`, Period = period, År = yr)


# header style
hs1 <- openxlsx::createStyle(
  fontColour = "#ffffff", fgFill = "#4F80BD",
  halign = "center", valign = "center", textDecoration = "bold",
  border = "TopBottomLeftRight", fontSize = 14
)

curr_sheetname <- str_glue("Weekly plan")
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, curr_sheetname)

# If you want to change base font: 
openxlsx::modifyBaseFont(wb, fontSize = 12, fontColour = "black", fontName = "Arial Narrow")

# Write data
openxlsx::writeData(wb, sheet = curr_sheetname, x, withFilter = T, headerStyle = hs1)
openxlsx::freezePane(wb, sheet = curr_sheetname, firstRow = T)
openxlsx::setColWidths(wb, 1, cols = 1:ncol(x), widths = "auto")

# Optional styling
# Add bgcolor based on cell values. Based on min/max when rule=NULL
conditionalFormatting(wb, 
                      curr_sheetname,
                      cols = ncol(x)-2, 
                      rows = 2:(nrow(x)+1),
                      style = c("lightblue", "darkred"),
                      rule = NULL,
                      type = "colourScale"
)

# Change format of columns
map(2:7, ~openxlsx::addStyle(wb, sheet = curr_sheetname, openxlsx::createStyle(numFmt = "NUMBER"), rows = 2:nrow(x), cols = .x))


curr_sheetname <- str_glue("Calendar view")
openxlsx::addWorksheet(wb, curr_sheetname)

# Write data
openxlsx::writeData(wb, sheet = curr_sheetname, calendar_view, withFilter = T, headerStyle = hs1)
openxlsx::freezePane(wb, sheet = curr_sheetname, firstRow = T)
openxlsx::setColWidths(wb, 1, cols = 1:ncol(calendar_view), widths = "auto")



openxlsx::saveWorkbook(wb, file = str_glue("./Output/date_handling_weekly_summary.xlsx"), overwrite = T)



