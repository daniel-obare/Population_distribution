#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(shinydashboard)
require(tidyverse)
require(ggplot2)
require(ggthemes)
theme_set(theme_minimal())

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Population Distribution by Sex in Urban Centres and Status of Centre 2009"),
    dashboardSidebar(),
    dashboardBody()
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    population <- Population_Distribution_by_Sex_in_Urban_Centres_and_Status_of_Centre_2009 %>% 
        select(-OBJECTID) %>% 
        mutate(`Peri-Urban_Male_Population` =
                   replace_na(Population_Distribution_by_Sex_in_Urban_Centres_and_Status_of_Centre_2009$`Peri-Urban_Male_Population`, 
                              mean(Population_Distribution_by_Sex_in_Urban_Centres_and_Status_of_Centre_2009$`Peri-Urban_Male_Population`, 
                                   na.rm = TRUE))) %>% 
        mutate(`Peri-Urban_Female_Population` =
                   replace_na(Population_Distribution_by_Sex_in_Urban_Centres_and_Status_of_Centre_2009$`Peri-Urban_Female_Population`,
                              mean(Population_Distribution_by_Sex_in_Urban_Centres_and_Status_of_Centre_2009$`Peri-Urban_Female_Population`, 
                                   na.rm = TRUE))) %>% 
        mutate(`Total_Peri-Urban_Population` =
                   replace_na(Population_Distribution_by_Sex_in_Urban_Centres_and_Status_of_Centre_2009$`Total_Peri-Urban_Population`,
                              mean(Population_Distribution_by_Sex_in_Urban_Centres_and_Status_of_Centre_2009$`Total_Peri-Urban_Population`, 
                                   na.rm = TRUE))) %>% 
        mutate(Rural_Male_Population = 
                   replace_na(Population_Distribution_by_Sex_in_Urban_Centres_and_Status_of_Centre_2009$Rural_Male_Population,
                              mean(Population_Distribution_by_Sex_in_Urban_Centres_and_Status_of_Centre_2009$Rural_Male_Population, 
                                   na.rm = TRUE))) %>% 
        mutate(Rural_Female_Population = 
                   replace_na(Population_Distribution_by_Sex_in_Urban_Centres_and_Status_of_Centre_2009$`Peri-Urban_Female_Population`,
                              mean(Population_Distribution_by_Sex_in_Urban_Centres_and_Status_of_Centre_2009$Rural_Female_Population, 
                                   na.rm = TRUE))) %>% 
        mutate(Total_Rural_Population = 
                   replace_na(Population_Distribution_by_Sex_in_Urban_Centres_and_Status_of_Centre_2009$Total_Rural_Population,
                              mean(Population_Distribution_by_Sex_in_Urban_Centres_and_Status_of_Centre_2009$Total_Rural_Population, 
                                   na.rm = TRUE))) %>% 
        mutate(Urban_Center = factor(Urban_Center)) %>% mutate(District = factor(District)) %>% 
        mutate(Status = factor(Status)) %>% arrange(Total_Population) %>% 
        janitor::clean_names()
    
    output$total

    ggplot(population, aes(district, Total_Population, fill = status))+
        geom_bar(stat = "identity")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplot(population, aes(core_urban_female_population, color = status))+
        geom_histogram(bins = 10, fill = "white", position = "identity")
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)
