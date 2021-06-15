library(shiny)
library(tidyverse)

mtcars
nrow(mtcars)
nested <- mtcars %>%
  mutate(id = row.names(.)) %>%
  mutate_if(is.factor, as.character) %>%
  group_by(id) %>%
  nest()

# for each id, get colnames of data and its values
a <- nested %>% 
  mutate(label =  map(data, ~ str_c(str_glue("{colnames(.)} = {.}"), collapse = ", ")))
a <- a %>% unnest(cols = c(data, label))
a


d <- as_tibble(Titanic)
d <- d %>% mutate_if(is.character, as.factor)
d

Measure <- names(d)[! names(d) %in% c("n")]
levels(d$Class) %>% set_names()

d %>% filter(Sex == "Male", Class == "1st", Age == "Adult", Survived == "Yes")


ui <- fluidPage(
  radioButtons("options", "Class",
               levels(d$Class) %>% set_names()),
  radioButtons("options_sex", "Sex",
               levels(d$Sex) %>% set_names()),
  radioButtons("options_age", "Age",
               levels(d$Age) %>% set_names()),
  radioButtons("options_surv", "Survived",
               levels(d$Survived) %>% set_names()),
  textOutput("txt"),
  plotOutput("plot")
)

server <- function(input, output) {
  output$txt <- renderText({
    #dist <- switch(input$options)
    # print(input$options)
    # print(input$options_sex)
    # print(input$options_age)
    # print(input$options_surv)
    str_glue("You have selected {input$options},{input$options_sex}, {input$options_age}, {input$options_surv}")
  })
  
  output$plot <- renderPlot({
    
  })
}

shinyApp(ui, server)
