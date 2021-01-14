#install.packages("eurostat")
library(eurostat)
library(tidyverse)

query <- search_eurostat(pattern = "fertility rate",
                         type = "table", fixed = F)
query$code
query[,1:2]

# download the table
ct <- c("SE","DK", "FI", "NO") # select certain countries
dat <- get_eurostat(id = query$code[2],
                    time_format = "num",
                    filters = list(geo = ct) )
dat
# converts DK to Denmark, SE to Sweden, etc
dat2 <- label_eurostat(dat)

# plotting
dat2 %>%
  ggplot(aes(x=time,y=values, color=geo, label=geo)) +
  geom_line(alpha = 0.5, show.legend = F) +
  geom_text(  data = dat2 %>% group_by(geo) %>% filter(time == max(time)), size= 3.5 ) +
  theme(legend.position = "none") +
  labs(title = "Total Fertility rate, 2007-2017", x="Year", y = "%")

# maps
mapdata <- get_eurostat_geospatial(nuts_level = 0)
mapdata <- mapdata %>% right_join(dat) %>% mutate(cat = cut_to_classes(values, n = 4, decimals = 1) )
mapdata$cat

mapdata %>% 
  filter(time == 2015) %>%
  ggplot(aes(fill=cat)) +
  scale_fill_brewer(palette = "RdYlBu") +
  geom_sf(color = alpha("white", 1/3), alpha = 0.6 ) +
  xlim(c(5,31)) + ylim(c(55,70)) + 
  labs(title = "Total Fertility rate 2015",
       subtitle = "Avg. number of life births per woman",
       fill = "%") + theme_minimal()
