# Mixed model
https://m-clark.github.io/mixed-models-with-R/random_intercepts.html
ranodm intercepts model: gruppspecifika intercept med egna unika effekter: 
tex (1 | student) interceptet 1 får variera mellan student till student
lägg till random effects till vänster om |


# Check outliers in boxplots: 

```r
is_outlier <- function(x) {
  return(x < quantile(x, 0.25,na.rm = T) - 1.5 * IQR(x, na.rm = T) | x > quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x,na.rm=T))
}
outlier_df <- score_diffs %>% 
  mutate( across( ends_with("_diff"), is_outlier , .names="out_{.col}" ) ) 
outlier_df %>%
  select(pid, contains("b_diff") ) %>% 
  filter(out_b_diff ==T) %>%
  arrange(randgrp, b_diff)
```

Note: outliers in `boxplot()` is computed differently. In that case,
use `boxplot.stats()$out` to see outlier values.

# Table 1 : nice summary tables

```
library(table1) 
# or:
library(tableone)

```

with flextable:

```r
tableone2df <- function(tableone){
  rows <- nrow(tableone)
  cols <- ncol(tableone)
  rowsXcols <- rows*cols
  colnames <- colnames(tableone)
  rownames <- rownames(tableone)

  listoflists <- list()
  for (i in 1:cols){
    start <- (i*rows+1)-rows
    end <- i*rows
    listoflists[[i]] <- tableone[start:end]
  }
  dataframe <- as.data.frame(listoflists, col.names = colnames, row.names = rownames)
  return(dataframe)
}

# then do:
flextable::flextable(tableone2df(table_1)  %>% rownames_to_column("Variable"))
```

# Create contingency tables

```
table()
prop.table() # with proportions
```

## Add totals to table

```r
# 2=sum cols, 1=sum rows, otherwise it sums both
tab <- addmargins(table(df$Company,df$Marital), 2)
```

# Create a sequence of dates 

```r
full_dates <- seq(as.Date("2016-09-01"), by = "month", length.out =39)
full_dates <- tibble(date = full_dates)
full_dates <- full_dates %>% mutate(month = format(date, "%m"), year = format(date, "%Y") )

# you can merge the full dates with a dataframe that also contains year & month. na:s automatically added
merge(full_dates, df, by = c("year", "month"), all.x = TRUE)

# you can combine year & month columns into a single column as:
df %>% 
  mutate(new_d = format( make_date(year, month), "%Y-%m" ) )
```

# Use cut

```r 
cut(tmp2$Antal, breaks = c(-Inf,20,50,Inf), labels = c("\u226420", "21-50", "50-521"))
# will show ≤20    21-50  50-521
```

# Create Swedish map of municipalities (kommuner)


```r
# Data för Sverigekarta
tmp1 <- tempfile()
download.file("http://api.thenmap.net/v2/se-7/geo/2020-08-14", destfile = tmp1)
county <- read_sf(tmp1) # Read simple features or layers from file or database

# Transforms coordinates of object to new projection.
# crs = coordinate reference system: integer with the EPSG code
county <- st_transform(x = county, crs = 3006) 

county <- merge(county, bef, by.x = "id", by.y = "Kommun")

tmp2 <- st_as_sf(kord, coords = c("lat", "long"), crs = 4326, agr = "constant") # Convert foreign object to an sf object
tmp2 <- st_transform(x = tmp2, crs = 3006)

# create circles 
ID_circle <- c("Lund", "Karlskrona", "Linköping", "Uppsala", "Örebro", "Umeå", "Solna", "Gårda")
data_sf_utm <- tmp2[tmp2$Ort %in% ID_circle, ]
circle1 <- sf::st_buffer(data_sf_utm, dist = 100000)
circle2 <- sf::st_buffer(data_sf_utm, dist = 100000/2)
circle3 <- sf::st_buffer(data_sf_utm, dist = 100000/4)

ggplot() +
  geom_sf(data = county, aes(fill = size2)) +
  #geom_sf(data = circle1, fill = "red", alpha = 0.2, inherit.aes = FALSE, lwd = 1) + 
  geom_sf(data = circle2, fill = "yellow", alpha = 0.2, inherit.aes = FALSE, lwd = 1) + 
  geom_sf(data = circle3, fill = "green", alpha = 0.2, inherit.aes = FALSE, lwd = 1) + 
  geom_sf(data = tmp2, aes(size = size)) + 
  theme_void() +
  scale_size_manual(name = "Frequency (events/year)", values = sz) + 
  scale_fill_manual("Municipality size", values = cls_size) 

```



