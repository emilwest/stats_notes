README2
================
Emil Westin
2021-02-03

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

# Check outliers in boxplots:

``` r
2+2+2
```

    ## [1] 6

``` r
fisher.test( matrix(c(2,10,20,3),nrow=2,ncol=2)  )
```

    ## 
    ##  Fisher's Exact Test for Count Data
    ## 
    ## data:  matrix(c(2, 10, 20, 3), nrow = 2, ncol = 2)
    ## p-value = 8.124e-05
    ## alternative hypothesis: true odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  0.002550506 0.265899606
    ## sample estimates:
    ## odds ratio 
    ## 0.03534288

``` r
# odds ratio: 0.035, p=0.00008124
```

In Python:

``` python
import scipy.stats as stats
oddsratio, pvalue = stats.fisher_exact([[2, 10], [20, 3]])
# (0.03, 8.124495626949326e-05)
```
