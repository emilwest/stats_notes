Emil Westin

## Statistical tests

### Calculate power in t-test

``` r
# delta: difference in means
# Number of subjects needed to obtain 80% power
power.t.test(delta = 10, sd = 19, power = 0.8, sig.level = 0.05)
# calculate power from sample size:
power.t.test(delta = 10, sd = 19, n=100, sig.level = 0.05)
```

### Wilcoxon rank sum test

Also known as the Mann-Whitney U test.

``` r
wilcox.test(df$num ~ temp$group)
```

### t-test

Tests difference in means (two-sample test). Assumptions: your data
values are independent, are randomly sampled from two normal
populations. The two independent groups are by default treated as not
having equal variances in the t.test function but can be changed.

$H_0: \mu_1 - \mu_2 = 0$

$H_1: \mu_1 - \mu_2 \neq 0$

``` r
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
# sleep data:
# Data which show the effect of two soporific drugs
# (increase in hours of sleep compared to control) on 10 patients.
# extra = numeric increase in hours of sleep 
# group = drug given
sleep %>% group_by(group) %>% dplyr::slice_sample(n = 3)
```

    # A tibble: 6 × 3
    # Groups:   group [2]
      extra group ID   
      <dbl> <fct> <fct>
    1   2   1     10   
    2   0   1     9    
    3  -0.2 1     3    
    4   0.8 2     2    
    5   4.4 2     6    
    6   1.6 2     8    

``` r
t.test(extra ~ group, data = sleep)
```


        Welch Two Sample t-test

    data:  extra by group
    t = -1.8608, df = 17.776, p-value = 0.07939
    alternative hypothesis: true difference in means between group 1 and group 2 is not equal to 0
    95 percent confidence interval:
     -3.3654832  0.2054832
    sample estimates:
    mean in group 1 mean in group 2 
               0.75            2.33 

In this case, $p = 0.079 > 0.05$ indicating no statistical significance.
There is not enough evidence to reject the null hypothesis that there is
zero difference in mean hourly sleep increase between group 1 and group
2. The mean sleep increase in group 2 is however higher, we may need a
larger sample size to increase the power of the test.

### Correlation incl p-values and confidence interval

``` r
cor.test()
```

### Fisher’s exact test

For testing the null hypothesis of independence of rows and columns in a
contingency table with fixed marginals.

``` r
fisher.test( matrix(c(2,10,20,3),nrow=2,ncol=2)  )
# odds ratio: 0.035, p=0.00008124
```

In Python:

``` python
import scipy.stats as stats
oddsratio, pvalue = stats.fisher_exact([[2, 10], [20, 3]])
# (0.03, 8.124495626949326e-05)
```

### Cohen’s Kappa Coefficient

Kappa measures the agreement between two variables, taking into account
the agreement that would happen by chance.

$\kappa = \frac{(O-E)}{(1-E)}, \text{ where 0 = observed agreement, E = expected agreement assuming independence}$

``` r
library(psych) 
tab<-matrix(c(12,20,20,12),ncol=2,nrow=2)
#     [,1] [,2]
#[1,]   12   20
#[2,]   20   12
psych::cohen.kappa(tab) # -0.25
```

### Calculate binomial confidence interval

``` r
binom.test(40, 95) # num successes, num trials
```

With CI in parenthesis for.ex. (10.1-15.5%) calculated from percentage
of success:

``` r
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

### Calculate difference in binomial confidence intervals

``` r
library(DescTools)
# x = number of successes in group 1 or 2, n = number of trials in group 1 or 2 
BinomDiffCI(x1=40,n1=95,x2=2,n2=47)
```

Nicely formatted CI calculated from percentage of success:

``` r
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

### Ordinal logistic regression

Proportional odds: the effect of the independent variables on the
dependent variable is constant for all levels in the dependent variable.

### Mixed model

<https://m-clark.github.io/mixed-models-with-R/random_intercepts.html>

Random intercepts model: group-specific intercepts with own unique
effects. E.g. (1 \| student) the intercept 1 is allowed to vary between
student to student. The random effect is to the left of \|.

### Survival analysis

#### Generate survival curves

``` r
library(survival) # survival curves
fit1 <- survfit(Surv( time , survival ) ~ variable, data=df)

# Survival plot:
#install.packages("survminer")
library(survminer)
ggsurvplot(fit1)
```

### Cox regression

``` r
library(survival) 
fit1 <- coxph(Surv( time , survival ) ~ variable, data=df)
summary(fit1)
```

### Check outliers in boxplots:

``` r
library(tidyverse)
```

    Warning: package 'tidyverse' was built under R version 4.2.1

    ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ✔ tibble  3.1.8     ✔ stringr 1.4.1
    ✔ tidyr   1.2.0     ✔ forcats 0.5.2
    ✔ readr   2.1.2     

    Warning: package 'tibble' was built under R version 4.2.1

    Warning: package 'stringr' was built under R version 4.2.1

    Warning: package 'forcats' was built under R version 4.2.1

    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()

``` r
is_outlier <- function(x) {
  return(x < quantile(x, 0.25,na.rm = T) - 1.5 * IQR(x, na.rm = T) | x > quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x,na.rm=T))
}

# example: number of outlier per variable
mtcars %>% 
  summarise(across(where(is.numeric), is_outlier)) %>% 
  summarise(across(everything(), ~sum(.==TRUE))) %>% 
  t()
```

         [,1]
    mpg     1
    cyl     0
    disp    0
    hp      1
    drat    0
    wt      3
    qsec    1
    vs      0
    am      0
    gear    0
    carb    1

Note: outliers in `boxplot()` is computed differently. In that case, use
`boxplot.stats()$out` to see outlier values.
