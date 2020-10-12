#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(broom)
library(kableExtra)

coffee_raings <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

ui <- dashboardPage(
    dashboardHeader(title="Dashboard Tidymodels"),
    
    dashboardSidebar(
        selectInput("v_country","Country",choices = coffee_raings %>% 
                        select(country_of_origin) %>% 
                        distinct() %>%
                        arrange(country_of_origin) %>% drop_na())
    ),
    dashboardBody(
    fluidRow(box(plotOutput("coffee_flavour")),box(plotOutput("coffee_variety"))),
    fluidRow(box(tableOutput("coffee_table")))
)          
)


server <- function(input, output) {
    
    output$coffee_flavour <- renderPlot({
        
        coffee_raings %>% 
            filter(country_of_origin == input$v_country) %>%
            select(aroma:cupper_points) %>%
            gather() %>%
            group_by(key) %>%
            summarise(value = mean(value)) %>%
            ungroup() %>%
            mutate(key = str_replace(key,"_"," ") %>% str_to_title()) %>%
            mutate(key = fct_reorder(key,value)) %>%
            ggplot(aes(x = key, y = value, color = key)) +
            geom_point(size = 5) +
            geom_segment(aes(x = key, xend = key, y = value, yend = 0)) +
            theme(legend.position = "none") +
            ylab("") +
            xlab("") +
            coord_flip() +
            labs(title="Avg Flavour Profile")
    })
    
    output$coffee_variety <- renderPlot({
        
        coffee_raings %>% 
        filter(country_of_origin == input$v_country) %>%
        select(variety) %>%
        drop_na %>%
        count(variety) %>%
        mutate(variety = fct_reorder(variety, n)) %>%
        ggplot(aes(x = n , y = variety, fill = variety)) +
        geom_col() +
        ylab("") +
        labs(title=" Bean variety") +
        theme(legend.position = "none")

    })
    

    output$coffee_table <- function(){
        
    coffee_raings %>%
        filter(country_of_origin == "Ethiopia") %>%
        select( total_cup_points,species,country_of_origin,region) %>%
        group_by(species) %>%
        kable() %>%
        kable_styling() %>%
        scroll_box(height = "400px",width = "400px")
    
    }

    }

shinyApp(ui, server)

