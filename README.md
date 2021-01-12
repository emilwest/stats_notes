# Mixed model
https://m-clark.github.io/mixed-models-with-R/random_intercepts.html
ranodm intercepts model: gruppspecifika intercept med egna unika effekter: 
tex (1 | student) interceptet 1 får variera mellan student till student
lägg till random effects till vänster om |

# Read excel files in R:

```r
# Excel: 
readxl::read_xlsx(name_of_file)

# Password protected excel:
library("excel.link")
xl.read.file(indata, password = "mypswd", write.res.password="mypswd")

```
# Read files in R

to do

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

## Group boxplot by group

```r
boxplot(temp$num ~ temp$group)
```

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
`right` = indicating if the intervals should be closed on the right (and open on the left) or vice versa
`include.lowest` = indicating if an ‘x[i]’ equal to the lowest (or highest, for right = FALSE) ‘breaks’ value should be included.

```r 
cut(tmp2$Antal, breaks = c(-Inf,20,50,Inf), labels = c("\u226420", "21-50", "50-521"))
# will show ≤20    21-50  50-521

cut(0:10, breaks = c(0,  1, 4, 10, Inf), labels = c("0", "1-3", "4-9", "\u226510"), include.lowest = T,right=F )
# [1] 0   1-3 1-3 1-3 4-9 4-9 4-9 4-9 4-9 4-9 ≥10
#Levels: 0 1-3 4-9 ≥10

# <19, ≥20 :
cut(0:20, breaks = c(0,  19,  Inf),labels = c("<20", "\u226520"),include.lowest = T,right=T )
#[1] <20 <20 <20 <20 <20 <20 <20 <20 <20 <20 <20 <20 <20 <20 <20 <20 <20 <20 <20 <20 ≥20
#Levels: <20 ≥20
```

# Create Swedish map of municipalities (kommuner)

[Kommunkoder](https://www.scb.se/hitta-statistik/regional-statistik-och-kartor/regionala-indelningar/lan-och-kommuner/lan-och-kommuner-i-kodnummerordning/#Stockholms_lan)

```r
library(tidyverse)
library(sf)
library(RColorBrewer)

# Data för Sverigekarta
tmp1 <- tempfile()
download.file("http://api.thenmap.net/v2/se-7/geo/2020-08-14", destfile = tmp1)
county <- read_sf(tmp1)
county <- st_transform(x = county, crs = 3006)
county <- merge(county, antal, by.x = "id", by.y = "Kommun", all.x=T)

county$size <- cut(county$Antal, breaks = c(0, 10, 100, 1000, Inf), 
                    labels = c("1-10", "11-100", "101-1000", ">1000"))
                    
cls_size <- brewer.pal(4, "Reds")
ggplot() +
  geom_sf(data = county, aes(fill = size)) +
  theme_void() +
  scale_fill_manual("Municipality size", values = cls_size) 
```


# Statistical tests

## Calculate power in t-test

```r
# delta: difference in means
power.t.test(delta = 10, sd = 19, power = 0.8, sig.level = 0.05)
```

```r
wilcox.test(df$num ~ temp$group)
t.test(temp$num ~ temp$group)

```

# Survival analysis

### Generate survival curves

```r
library(survival) # survival curves
fit1 <- survfit(Surv( time , survival ) ~ variable, data=df)

# Survival plot:
#install.packages("survminer")
library(survminer)
ggsurvplot(fit1)
```

### Cox regression

```r
library(survival) 
fit1 <- survfit(Surv( time , survival ) ~ variable, data=df)
summary(fit1)
```

# ggplot2


### Arrange plots

```r
library(ggpubr)
ggarrange(p1, p2, p3)
# with common legend:
ggarrange(p1, p2, p3, ncol=3, common.legend = T, legend="right")
```

#### Arrange within arrange

```r
library(tidyverse)
library(ggpubr)
# with common legend
p11 <- iris %>% ggplot(aes(Sepal.Length, Sepal.Width, color=Species)) + geom_point()
# with common legend
p1 <- ggarrange(p11  ,
                p11 ,
                p11 ,
                ncol=1, nrow=3,  legend = "none"
)
p2 <- ggarrange( p11 ,  legend = "none")
p3 <- ggarrange(
  p11,
  p11 ,
  p11 , 
  ncol=1,nrow=3, common.legend = T, legend="right"
)
ggarrange(p1, p2, p3, ncol=3, common.legend = T, legend="right")
```
Generates:

![Arrange example](arrange.png)

### Legend


#### Legend title

```r
library(tidyverse)
p11 <- iris %>% ggplot(aes(Sepal.Length, Sepal.Width, color=Species)) + geom_point()
# change title in legend:
p11 + labs(color = "Hej") # or fill/group
# Alternatively:
p11 + scale_color_discrete("Hej")
p11 + scale_fill_discrete("Hej") # if fill=Species
p11 + scale_color_manual("Hej")  
```

#### Legend colors

```r
library(tidyverse)
library(RColorBrewer)
p11 <- iris %>% ggplot(aes(Sepal.Length, Sepal.Width, color=Species)) + geom_point()

# choose custom color palette 
p11 + scale_color_brewer("Hej",palette = "Reds")

# alternatively:
num_categories <- 3
cls_size <- brewer.pal(num_categories, "Reds") 
p11 +  scale_color_manual("Hej", values = cls_size, drop=FALSE)
# or if fill=Species, use: 
# scale_fill_manual("Hej", values = cls_size, drop=FALSE)
# scale_linetype_manual()

# choose a pre-defined color palette:
p11 + scale_color_viridis_d("Hej")
p11 + scale_color_grey()

```
