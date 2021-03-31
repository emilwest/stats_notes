library(tidyverse)

# Quote contents as a quosure
# Quosure- An expression that has been saved with an environment (aka a closure).  
# A quosure can be evaluated later 
# in the stored environment to  return a predictable result.
a <- 1
b <- 2
q <- rlang::quo(a+b)
q

rlang::eval_tidy(q)
rlang::eval_bare(rlang::parse_expr("1+1"))

#rlang::enquo(arg) 
#Call from within a function to quote 
#what the user passed to an argument as a quosure. Also enquos for multiple args. 
#quote_this < - function(x) enquo(x) 
#quote_these < - function(...) enquos(...) 
# !! unquotes the symbol 
f <- function(df, a, b){
  a <- rlang::enquo(a)
  b <- rlang::enquo(b)
  
  df %>%
    ggplot(aes(x=!!a,y=!!b)) + 
    geom_point()
}
mtcars
f(mtcars, disp, mpg )

named_mean <- function(data, var) {
  var <- rlang::ensym(var)
  data %>%  
    summarise(!!name := mean(!!var))
  } 
named_mean(mtcars, "disp")
