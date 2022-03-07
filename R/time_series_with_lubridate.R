library(tidyverse)
library(lubridate)
get_pace <- function(distance, target_duration, decimals = 1, style = "below") {
  
  if (distance == "marathon") dist <- 42.195 # km 
  if (distance == "half marathon") dist <- 21.0975
  if (str_detect(distance, "[0-9.]+")) dist <- as.numeric(distance)
  
  s <- target_duration*60 # convert to seconds
  
  if (style == "below") pace <- (s-1)/dist
  if (style == "exact") pace <- s/dist
  
  
  
  res <- seconds_to_period(pace) 
  return(res %>% round(decimals))
}



tibble(minutes = 100:290,
       time_parsed = seconds_to_period(dminutes(minutes)) %>% as.numeric() %>% duration) %>% 
  mutate(pace = get_pace("marathon", minutes) %>% as.numeric()%>% duration) %>% 
  ggplot(aes(x= time_parsed, y = pace)) +
  geom_line() +
  scale_x_time(breaks = scales::breaks_width("15 min"), labels =scales::label_time(format = "%H:%M")) +
  scale_y_time(breaks = scales::breaks_width("0.25 min"), labels =scales::label_time(format = "%M:%S"))



long_format <- tibble(minutes = 
         c(
           seq(from = 13, to = 25, by = 1),
           seq(from = 27.5, to = 30, by = 2.5),
           seq(from = 35, to = 60, by = 5),
           seq(from = 65, to = 115, by = 5),
           seq(from = 120, to = 360, by = 15)
         )
       ,
       parsed_time = seconds_to_period(duration(minutes, "minutes")-1) %>% as.numeric() %>% duration,
       marathon = get_pace("marathon", minutes) %>% as.duration,
       halfmarathon = get_pace("half marathon", minutes) %>% as.duration,
       `10k` = get_pace(10, minutes) %>% as.duration,
       `5k` = get_pace(5, minutes) %>% as.duration
) %>% 
  mutate(marathon = ifelse(between(minutes, 120, 360), marathon, NA_character_)) %>% 
  mutate(halfmarathon = ifelse(between(minutes, 60, 180), halfmarathon, NA_character_)) %>% 
  mutate(`10k` = ifelse(between(minutes, 25, 80), `10k`, NA_character_)) %>% 
  mutate(`5k` = ifelse(between(minutes, 10, 40), `5k`, NA_character_)) %>% 
  select(-minutes) %>% 
  pivot_longer(-parsed_time, names_to = "race", values_to = "pace") 
  

long_format %>% 
  drop_na() %>% 
  ggplot(aes(x= parsed_time, y = pace, group=race)) +
  geom_line() +
  scale_x_time(breaks = scales::breaks_width("15 min"), labels =scales::label_time(format = "%H:%M")) +
  scale_y_time(breaks = scales::breaks_width("0.5 min"), labels =scales::label_time(format = "%M:%S"))+
  facet_wrap(race ~.,scales = "free")



long_format %>% 
  drop_na() %>% 
  filter(race == "5k") %>% 
  ggplot(aes(x= parsed_time, y = pace, group=race)) +
  geom_line() +
  scale_x_time(breaks = scales::breaks_width("2.5 min"), labels =scales::label_time(format = "%H:%M")) +
  scale_y_time(breaks = scales::breaks_width("0.5 min"), labels =scales::label_time(format = "%M:%S")) +
  theme_light()


long_format %>% 
  drop_na() %>% 
  filter(race == "10k") %>% 
  ggplot(aes(x= parsed_time, y = pace, group=race)) +
  geom_line() +
  scale_x_time(breaks = scales::breaks_width("5 min"), labels =scales::label_time(format = "%H:%M")) +
  scale_y_time(breaks = scales::breaks_width("0.5 min"), labels =scales::label_time(format = "%M:%S")) +
  theme_light()

long_format %>% 
  drop_na() %>% 
  filter(race == "halfmarathon") %>% 
  ggplot(aes(x= parsed_time, y = pace, group=race)) +
  geom_line() +
  scale_x_time(breaks = scales::breaks_width("10 min"), labels =scales::label_time(format = "%H:%M")) +
  scale_y_time(breaks = scales::breaks_width("0.5 min"), labels =scales::label_time(format = "%M:%S")) +
  theme_light()

long_format %>% 
  drop_na() %>% 
  filter(race == "marathon") %>% 
  ggplot(aes(x= parsed_time, y = pace, group=race)) +
  geom_line() +
  scale_x_time(breaks = scales::breaks_width("15 min"), labels =scales::label_time(format = "%H:%M")) +
  scale_y_time(breaks = scales::breaks_width("0.5 min"), labels =scales::label_time(format = "%M:%S")) +
  theme_light()

