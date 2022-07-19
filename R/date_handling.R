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
    w = lubridate::week(datum), # week 1 starts on Jan 1st
    isow = lubridate::isoweek(datum), # week 1 starts on first Monday of the year 
    d = lubridate::day(datum),
    day = lubridate::wday(datum, label = T)
  ) 
  
  # weekly summary
  weekly_summary <- x %>%
    group_by(yr, isow) %>%
    mutate(id = row_number()) %>%
    filter(id == first(id) | id == last(id)) %>%
    mutate(daypart = str_glue("{d} {m}")) %>%
    summarise(period = str_c(daypart, collapse = " - ")) %>% 
    ungroup()
  
  return(list(
    year_df = x,
    weekly_summary = weekly_summary
  ))
}

generate_yearly_dfs()$weekly_summary %>% 
  mutate(`Pass 1` = NA, `Pass 2` = NA, `Pass 3` = NA, `Pass 4` = NA) %>% view



generate_yearly_dfs(2022:2023)$weekly_summary %>% view


generate_yearly_dfs()$year_df %>% 
  select(-d,-w) %>% 
  group_by(yr, isow) %>% 
  pivot_wider(names_from = day, values_from = datum) %>% 
  select(yr, m, isow, mån:fre, lör, sön)

generate_yearly_dfs()$year_df$m

generate_yearly_dfs()$year_df %>%
  group_by(yr, isow) %>%
  mutate(n=n()) %>% 
  mutate(id = row_number()) %>% tail(10)
  filter(id == which.min(id) | id == which.max(id)) %>% tail
  mutate(daypart = str_glue("{d} {m}")) %>%
  summarise(period = str_c(daypart, collapse = " - ")) %>% 
  ungroup()
