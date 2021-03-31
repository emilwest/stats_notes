library(tidyverse)

# map: apply function to each element of list of vector
mtcars %>% map(., sum) # sum each col in df
pmap_dbl(mtcars,sum) # sum of each row
by_cyl <- mtcars %>% split(.$cyl) # list of dfs
mods <- by_cyl %>% map(~ lm(mpg ~ wt, data = .)) # in each df, create linear model

by_cyl2 <- mtcars %>% group_by(cyl) %>% nest()
by_cyl2$data %>% map(~ lm(mpg ~ wt, data = .))


# map2: apply function to pairs of elements from two lists or vectors
map2(mods, by_cyl, predict) # predict for each element in list
# i.e.:  predict(mods[[1]], by_cyl[[1]]) , predict(mods[[2]], by_cyl[[2]]), ... 
mtcars 


mtcars %>% pluck(.$mpg, )


# run each function in a list:
l <-list(var, sd); invoke_map(l, x = 1:9) 


obj1 <- list("a", list(1, elt = "foo"))
obj2 <- list("b", list(2, elt = "bar"))
x <- list(obj1, obj2)
x
pluck(x, 1) # x[[1]]
pluck(x,1,2) # x[[1]][[2]]
pluck(x, 1, 2, "elt")
x[[1]][[2]][["elt"]]
pluck(x, 2, "elt")

pluck(x, 10) # returns null when element does not exist
try(x[[10]]) # returns Error in x[[10]] : subscript out of bounds
# You can also supply a default value for non-existing elements:
pluck(x, 10, .default = NA)

# # If you prefer to consistently fail for non-existing elements, use
# the opinionated variant chuck():
chuck(x, 1)
try(chuck(x, 10))
try(chuck(x, 1, 10))



?discard
x <- rerun(5, a = rbernoulli(1), b = sample(10))
x
x %>% keep("a")
x %>% discard("a")


?head_while
pos <- function(x) x >= 0
head_while(5:-5, pos)


?reduce
reduce(c(1,2,3,4,5),sum )
# 15

accumulate(c(1,2,3,4,5),sum )
# 1  3  6 10 15


# ------------------------------------------------------------------------------
# modify function behaviour:
?compose

add1 <- function(x) x + 1
compose(add1, add1)(8) #10

add2 <- function(x) x + 2
compose(add1, add2)(8) #10


fn <- compose(~ paste(.x, "foo"), ~ paste(.x, "bar"))
fn("input")



?lift
x <- list(x = c(1:100, NA, 1000), na.rm = TRUE, trim = 0.9)
lift_dl(mean)(x)

?quietly


safe_log <- safely(log)
safe_log(10)
safe_log("a")


safe_log <- quietly(log)
safe_log(10)
safe_log("a")


# Possibly:
# Modify  function to return default value whenever an error occurs (instead of error).
list("a", 10, 100) %>%
  map_dbl(possibly(log, NA_real_))



n_iris <- iris %>% group_by(Species) %>% nest()
# irirs = 150x4
# nested: 3x50x4
n_iris$data
n_iris$data[[1]]























