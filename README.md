![Stats notes](img/wordcloud1.png) 


This document contains notes I make about R, data science and software engineering in the context of statistics.

How to use this document: search for a keyword with `Ctrl` + `f` or scroll trough. 


# RStudio useful shortcuts

|                                                                            | Command                            |
|----------------------------------------------------------------------------|------------------------------------|
| Restart R-session and unload packages:                                     | `Command`/`Ctrl` + `Shift` + `F10` |
| Comment/uncomment single/multiple lines:                                   |  `Ctrl`+ `Shift` + `c`             |
| Display command palette:                                                   |  `Ctrl`+ `Shift` + `p`             |
| Auto-indent:                                                               | `Ctrl` + `i`                       |
| Edit multiple lines simultaneusly (increase the blinking line vertically): |  `Ctrl` + `Alt` + `↑ / ↓`          |
| Duplicate current line:                                                    | `Alt` + `Shift` + `↑ / ↓`          |
| Insert assignment (`<-`):                                                  |  `Alt` + `-`                       |
| Insert pipe operator (`%>%`):                                              |  `Ctrl` + `Shift` + `m`            |
| Delete current line:                                                       |  `Ctrl` + `d`                      |
| Multiple word selection:                                                   | `Ctrl` + `Alt` + double left click |
| Run code from beginning to line                                            | `Ctrl` + `Alt` + `b`               |

# Read/write excel files in R:

```r
# Read xlsx/xls:
readxl::read_xlsx(name_of_file) 
readxl::read_xls(name_of_file) # or readxl::read_excel(name_of_file) for guessing xls or xlsx
# Read xlsx:
openxlsx::read.xlsx(name_of_file) # only for xlsx

# Password protected excel:
library("excel.link")
xl.read.file(indata, password = "mypswd", write.res.password="mypswd")

# Write excel files:
library(openxlsx)
write.xlsx(df, file = "Output/myfile.xlsx")

# Write multiple data frames as sheets in single excel file:
list_of_datasets <- list("Sheet name 1" = df1, 
                         "Sheet name 2" = df2,
                         "Sheet name 3" = df3)
openxlsx::write.xlsx(list_of_datasets, file = "Output/myfile.xlsx")                    
```

## Read multiple excel sheets from single excel file into a single data frame

Credits to https://dominicroye.github.io/en/2019/import-excel-sheets-with-r/ 

```r
in_path <- "path/to/excelfile.xlsx"

# read multiple sheets and join them into single dataframe:
df <- in_path %>%
  excel_sheets() %>% # vector containing name of sheets
  set_names() %>% # sets the names of vector with names of sheets
  # map_df: a list of tables joined with bind_rows into single dataframe:
  purrr::map_df(readxl::read_excel,
    path = in_path,
    .id = "level" # creates column called level with names of the sheets
  )
```

## Read multiple excel sheets for multiple excel files into a single data frame

```r
read_multiple_excel_sheets <- function(path) {
  path %>%
    excel_sheets() %>% 
    set_names() %>% 
  map_df(read_excel, path = path)
}

df <- dir(pattern = "xlsx") %>% 
           map_df(read_multiple_excel,
                  .id = "level")
```


# Read CSV files in R

Read multiple csv files in directory using summarise.

See https://www.tidyverse.org/blog/2020/03/dplyr-1-0-0-summarise/ 

```r 
library(tidyverse)
# reading all csv files in current directory:
tibble(path = dir(pattern = "\\.csv$")) %>% 
  rowwise(path) %>% 
  summarise(read_csv(path))
```

# System handling in R

## Remove all files and subdirectories under path/* (without deleting path/)

```r
unlink("path/*", recursive = TRUE)
unlink("path/*") # deletes files only, no directories
```

## Remove all files and subdirectories for multiple paths

```r
# set pattern = ".*out.*" to target specific directories named out
out_dirs <- list.files(str_glue("C:/path1/path2"), full.names = T, recursive = T, pattern = ".*out.*", include.dirs = T)
out_dirs <- str_c(out_dirs, "/*")
# out_dirs contains: "C:/path1/path2/ax/out/*" "C:/path1/path2/dk/out/*" "C:/path1/path2/EA/out/*" "C:/path1/path2/EU/out/*" ...
unlink(out_dirs, recursive = T) # removes files and dirs under "C:/path1/path2/{ax,dk,EA,EU,...}/out/*"
```

## Extract filenames or directory names from a path

```r
basename("C:/px/hej.txt")
# hej.txt
dirname("C:/px/hej.txt")
# "C:/px"
```

# Unload library/package from R session without restarting

```r
# unloading library(officer)
detach(package:officer)
```


# Save R results in Word file

```r
library(flextable) # converting dataframes to flextable objects
library(officer) # for adding data to word document, like flextables

# simplest example:
tab1 <- matrix( c(1,2,3,4), ncol=2, nrow=2)
word_export <- read_docx()
word_export <- word_export %>% body_add_flextable( as.data.frame.matrix(tab1) %>% flextable()  )
print(word_export, 'try.docx')
```


```r
# add new page:
mydoc %>% body_add_break()
```

Function for adding R table to word document:

```r
myft <- function(mydoc, tab, title) {
  res <- body_add_par(mydoc, "")
  res <- body_add_par(mydoc, title, style = "Tabellrubrik_")
  res <- body_add_flextable(mydoc, flextable(tab %>% as.data.frame.matrix() %>% rownames_to_column(" ")) %>% autofit(), align = "left") 
  return(res)
}
mydoc <- myft(mydoc, tt0, "Table 1. xxx")
```

Template:

```r
library(flextable)
library(officer)

setwd("<set working dir>")
getwd()

inmall <- "word_template.docx"
utmall <- "out_file.docx"

# RAPPORT: IMPORT --------------------------------------------------------------
mydoc <- read_docx(inmall)

# RAPPORT: TITLE PAGE ----------------------------------------------------------
mydoc <- body_replace_all_text(mydoc, "TITLE", "My title page", only_at_cursor = F)
mydoc <- body_replace_all_text(mydoc, "DATE", as.character(Sys.Date()), only_at_cursor = F)

# RAPPORT: SECTION 1 -----------------------------------------------------------
mydoc <- body_add_par(mydoc, "Descriptive statistics", style = "heading 1", pos = "before")

# RAPPORT: EXPORT --------------------------------------------------------------
print(mydoc, utmall)
shell.exec(utmall)
```

# PX files

## Read a px-file in R

```r
library(pxR)
px_in <- read.px("abcd10.px",  encoding='iso-8859-15')
```

## Write/export a px-file

```r
library(NSDB)
NSDB::write.px(
  px_in,
  "abcd10_new.px",
  fileEncoding = "iso-8859-15"
)
```

## Pxweb

```r
library(pxweb)
```

# Download text files from URL

To do.

```r
```

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

# Merge multiple dataframes

From https://stackoverflow.com/questions/14096814/merging-a-lot-of-data-frames 

```r
Reduce(function(...) merge(..., all=TRUE), list(df1, df2, df3))
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

# Create a Data Frame from All Combinations of Factor Variables

Returns a dataframe (data.frame) from all combinations of the supplied vectors or factors.
Useful if you for ex. want to test a set of hyperparameters without creating nested for loops, just create a dataframe of all combinations and iterate through it.

```r
expand.grid(letters[1:2], 1:3, c("+", "-"))
```

# Use cut

`right` = indicating if the intervals should be closed on the right (and open on the left) or vice versa.
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

# Vectors

## Split vector into chunks given length of each chunk

From https://statisticsglobe.com/split-vector-into-chunks-in-r 

```r
 my_vec <- 2008:2021
  chunk_length <- 5
  split(my_vec,             
        ceiling(seq_along(my_vec) / chunk_length))
```

##  Split vector into chunks given number of chunks

From https://statisticsglobe.com/split-vector-into-chunks-in-r 

```r
chunk_number <- 7   
split(my_vec,             # Applying split() function
      cut(seq_along(my_vec),
          chunk_number,
          labels = FALSE))
```

# Split dataframe into groups

Returns list of dataframes.

```r
library(tidyverse) 
mtcars %>% split(.$cyl)
```

# Lists
+ Do a comparison with python dicts.
## Add element to list
To do 

## Remove element of list
To do 

## Modify element of list
To do 

# Calculating sums

## Calculate sum and return NA if all categories are NA

The problem with sum(x, na.rm = TRUE) is that it will return 0 if all categories are NA. 
Sometimes we want it to be NA, here is how:

```r
sumna <- function(x) {
  if(all(is.na(x))) NA else sum(x, na.rm = TRUE)
}
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
# Number of subjects needed to obtain 80% power
power.t.test(delta = 10, sd = 19, power = 0.8, sig.level = 0.05)
# calculate power from sample size:
power.t.test(delta = 10, sd = 19, n=100, sig.level = 0.05)
```
## Wilcoxon rank sum test

Also known as the Mann-Whitney U test.

```r
wilcox.test(df$num ~ temp$group)
```

## t-test

Tests difference in means.

```r
t.test(temp$num ~ temp$group)
```

## Correlation incl p-values and confidence interval

```r
cor.test()
```

## Fisher's exact test

For testing the null hypothesis of independence of rows and columns in a contingency table with fixed marginals.

```r
fisher.test( matrix(c(2,10,20,3),nrow=2,ncol=2)  )
# odds ratio: 0.035, p=0.00008124
```

In Python: 

```python
import scipy.stats as stats
oddsratio, pvalue = stats.fisher_exact([[2, 10], [20, 3]])
# (0.03, 8.124495626949326e-05)
```

## Cohen's Kappa Coefficient

Kappa measures the agreement between two variables, taking into account the agreement that would happen by chance.
Kappa = (O-E) / (1-E), where O = observed agreement, E = expected agreement assuming independence.


```r
library(psych) 
tab<-matrix(c(12,20,20,12),ncol=2,nrow=2)
#     [,1] [,2]
#[1,]   12   20
#[2,]   20   12
psych::cohen.kappa(tab) # -0.25
```

## Calculate binomial confidence interval

```r 
binom.test(40, 95) # num successes, num trials
```

With CI in parenthesis for.ex. (10.1-15.5%) calculated from percentage of success:

```r 
library(stringr)
calc_conf <- function(perc, n){
  test <- round(perc*n)
  res  <- binom.test(test, n)
  lower <- res$conf.int[1] %>% round(3) *100 
  upper <- res$conf.int[2] %>% round(3) *100
  lower <- format(lower, nsmall = 1)
  upper <- format(upper, nsmall = 1)
  return(str_glue("({lower}-{upper}%)"))
}
calc_conf(0.42, 95) # (32.0-52.7%)
```

## Calculate difference in binomial confidence intervals

```r
library(DescTools)
# x = number of successes in group 1 or 2, n = number of trials in group 1 or 2 
BinomDiffCI(x1=40,n1=95,x2=2,n2=47)
```

Nicely formatted CI calculated from percentage of success:

```r
library(stringr)
bindiff <- function(perc_1, n_1, perc_2, n_2){
  test1 <- round(perc_1*n_1)
  test2 <- round(perc_2*n_2) 
  
  res <- BinomDiffCI(test1,n_1,test2,n_2)
  val <- res[1] %>% round(3) *100
  lower <- res[2] %>% round(3) *100
  upper <- res[3] %>% round(3) *100
  val <- format(val, nsmall = 1)
  lower <- format(lower, nsmall = 1)
  upper <- format(upper, nsmall = 1)
  
  return(
    list(test1=test1,test2=test2, 
    ci = str_glue("{val}% ({lower}-{upper}%)")
    ))
}
bindiff(0.42,95,0.042,47) # 37.8% (24.2-48.0%)
```

## Ordinal logistic regression

Proportinal odds: effekten av de oberoende variablerna på den beroende ordinala variabeln är konstant för alla nivåer i den beroende variabeln.

## Mixed model
https://m-clark.github.io/mixed-models-with-R/random_intercepts.html

Random intercepts model: group-specific intercepts with own unique effects.
E.g. (1 | student) the intercept 1 is allowed to vary between student to student.
The random effect is to the left of |.

<!--
```r

```
-->

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
fit1 <- coxph(Surv( time , survival ) ~ variable, data=df)
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

![Arrange example](img/arrange.png)

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

#### Change the legend label text

```r
+ scale_color_discrete(labels = c("A",  "B", "C")) # for ggplot(aes(color=category))
+ scale_fill_discrete(labels = c("A",  "B", "C")) # for ggplot(aes(fill=category))
```


#### Legend colors

```r
library(tidyverse)
library(RColorBrewer)
p11 <- iris %>% ggplot(aes(Sepal.Length, Sepal.Width, color=Species)) + geom_point()
p11_c <- ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=Sepal.Width)) + geom_point()

# choose custom color palette 
p11 + scale_color_brewer("Hej",palette = "Reds")
# continous:
p11_c + scale_color_gradient(low = "white", high = "red")

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

### Bar plot

```r
# stat=identity: use own y-values, rather than counting the aggregate number of rows for each x (stat=count)
# position=dodge : place bars side by side instead of stacked
 geom_bar(stat="identity", position = "dodge")

# add labels above bar plot, for example percentage value
+ geom_text(aes(label= str_glue("{value*100}%") ),
                position=position_dodge(width=0.9),
            vjust=-0.25) 

```

Dodging overlapping labels:

```r
p <- ggplot(mpg) +
  geom_bar(aes(x = manufacturer)) + 
  theme(axis.text.x = element_text(size = 11))
# Use guide_axis to dodge the labels
p + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
```


### Binning scales

Introduced in ggplot2 3.3.0. A binning scale takes continuous data and makes it discrete by assigning each data point to a bin.
Allows continuous data to be used with discrete palettes

```r
p <- ggplot(mpg) + 
  geom_point(aes(displ, cty, size = hwy, colour = hwy))

p + 
  scale_size_binned()

# show outermost ticks:
p + 
  scale_size_binned(guide = guide_bins(show.limits = TRUE))
  
p + 
  scale_colour_binned()
  
# place data at the center of the bin they belong to:
p + 
  scale_x_binned()
 
# this facilitates to create histograms with tick-marks between the bars:
ggplot(mpg) + 
  geom_bar(aes(displ)) + 
  scale_x_binned()

```


### Plot text column in ggplot in the same order it appears in the data frame

```r
# procedure is a text column
f <- function(x) factor(x, levels = unique(x))
reshape2::melt(tmp, id.vars = "Procedure") %>%
  ggplot(aes(x=f(Procedure), y=value*100, fill=variable)) 
```

# Tidyeval

Code - a sequence of symbols/constants/calls that will return a result if evaluated. Code can be:
(i) Evaluated immediately (Standard Eval), (ii) Quoted to use later (Non-Standard Eval).

### Quote contents as a quosure

Quosure- An expression that has been saved with an environment (aka a closure).  
A quosure can be evaluated later in the stored environment to  return a predictable result.

```r
a <- 1
b <- 2
q <- rlang::quo(a+b)
# <quosure>
#expr: ^a + b
#env:  global
```

Evaluate it with `rlang::eval_tidy(q)`, returning 3.

### Quote within function

Useful when doing ggplot functions. For example with this function you can create a function like this `f(mtcars, disp, mpg )`, where disp and mpg are columns within the dataframe mtcars (included in tidyverse). 
!! uncoutes the symbol.

```r
library(tidyverse)
f <- function(df,a,b){
  a <- rlang::enquo(a)
  b <- rlang::enquo(b)
  # !! unquotes the symbol 
  df %>%
    ggplot(aes(x=!!a,y=!!b)) + 
    geom_point()
}
mtcars
f(mtcars, disp, mpg )
```
# Purr

###  map: apply function to each element of list or vector

```r
# map: apply function to each element of list of vector
mtcars %>% map(., sum) # sum each col in df
pmap_dbl(mtcars,sum) # sum of each row
by_cyl <- mtcars %>% split(.$cyl) # list of dfs
mods <- by_cyl %>% map(~ lm(mpg ~ wt, data = .)) # in each df, create linear model

```

Why use map instead of lapply? https://stackoverflow.com/questions/45101045/why-use-purrrmap-instead-of-lapply

### Nest: split dataframe into list of dataframes

```r
library(tidyverse)
n_iris <- iris %>%  group_by(Species) %>%  nest()
## A tibble: 3 x 2
## Groups:   Species [3]
#  Species    data             
#  <fct>      <list>           
#1 setosa     <tibble [50 x 4]>
#2 versicolor <tibble [50 x 4]>
#3 virginica  <tibble [50 x 4]>
```

### Join each element in nested dataframe with another dataframe

```r
join_df <- function(df_nest, df_other) {
  df_all <- inner_join(df_nest, df_other, by = c("name1" = "name2"))
  return(df_all)
}

tmp_n2 <- tmp_n %>%
  mutate(new_data = map(data, ~ join_df(., df_of_interest)))
```

### Read nested files

Consider the dataframe df:

| matrix | filePathIn  |
|--------|-------------|
| A      | my_dir/A.px |
| B      | my_dir/B.px |
| C      | my_dir/C.px |

```r
inFile_n <- df %>% group_by(matrix) %>% nest()
# läs in alla matriser:
inFile_alla <- inFile_n %>% 
  mutate( rrr = map(data, function(x) read.px(x$filePathIn) ) )
# alternatively:
#inFile_n %>% 
#  mutate( rrr = map(data, ~ read.px(.x$filePathIn)  ) )
```
where:

inFile_n:

| matrix \<chr\> | data \<list\>      |
|----------------|--------------------|
| A              | \<tibble [1 x 2]\> |
| B              | \<tibble [1 x 2]\> |
| C              | \<tibble [1 x 2]\> |


inFile_alla:

| matrix \<chr\> | data \<list\>      | rrr \<list\>             |
|----------------|--------------------|--------------------------|
| A              | \<tibble [1 x 2]\> | \<tibble [27,000 x 7]\>  |
| B              | \<tibble [1 x 2]\> | \<tibble [25,200 x 6]\>  |
| C              | \<tibble [1 x 2]\> | \<tibble [126,000 x 7]\> |


# Stringr string manipulation

See [stringr.R](R/stringr.R)


# Powershell

Not really part of statistics but good to know if you need to do any form of automation on system files. 

### grep -r in powershell

```powershell
dir C:\Users\emiwes\Documents\experiments -Recurse | Select-String -pattern "my search"
```

# Run/execute program/scripts from R using system()

```r
system("powershell ls -rec H:\\Documents\\")
```

### Search and replace multiple files in folder

Add ls -rec for subfolders and for ex. -Include *.txt for including txt files only 

```r
system("powershell (ls H:\\Documents\\multiple_tests\\*) |
       Foreach-Object{ $f=$_; (gc $f.PSPath) |
       Foreach-Object {$_ -replace '\\\"hej', 'hej' } |
       Set-Content $f.PSPath
       }
       ")
```

# Git

Git is a distributed version control system that is really useful to know.


```git
Note: [Using git version 2.30.0.windows.2]

# open global .gitconfig in ~/.gitconfig. To find where ~ is on windows, type echo $HOME
git config --global --edit
# create and switch to branch:
git checkout -b main/emil_westin
# push to your current branch:
git push origin HEAD
# push to main directly:
git push origin HEAD:main 
# show changes:
git diff 
# add everything to staged:
git add -A
# unstage all files:
git reset 
# unstage specific file:
git restore --staged file.txt
# commit:
git commit -m "message"
# to modify previous commit, for ex include a new file, type:
git commit --amend 
# if you already have a folder that you want to track with version conrol and associate with a remote repository:
git remote add origin https://linktoyourremotereporitory
# clone a repository (get a local copy incl all version history):
git clone https://linktoremotereporitory
```

Nice extra things to add in the ~/.gitconfig to add color in the terminal:

```git
[color]
    ui = auto
[color "branch"]
    current = yellow reverse
    local = yellow
    remote = green
[color "diff"]
    meta = yellow bold
    frag = magenta bold
    old = red bold
    new = green bold
[color "status"]
    added = yellow
    changed = green
    untracked = cyan
```

# Creating a virtual environment

To do

# Bash / linux useful commands

To do
