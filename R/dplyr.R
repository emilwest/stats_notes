library(tidyverse)

mtcars %>% 
  names() %>% 
  purrr::map(~ count(mtcars, .data[[.x]]))

for (var in names(mtcars)) {
  mtcars %>% count(.data[[var]]) %>% print()
}

df <- tibble(
  grp = rep(1:2, each = 5), 
  x = c(rnorm(5, -0.25, 1), rnorm(5, 0, 1.5)),
  y = c(rnorm(5, 0.25, 1), rnorm(5, 0, 0.5)),
)
df
?range
df %>% 
  group_by(grp) %>% 
  summarise(rng = range(x))

#  Extract column values as a vector. Choose by name or index.
mtcars %>% pull(cyl)

# helpers for select() and across()
