#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
library(forcats)
house_data <- house_data %>% 
    mutate(cenazam_2 = 10.764*price/sqft_living)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Ceny nieruchomosci w Seattle w zaleznosci od roznych czynnikow"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("rokwyb",
                        "Wybierz lata wybudowania:",
                        min = 1900,
                        max = 2015,
                        value = c(1980,2000)),
            selectInput("kol",
                        "Wybierz rodzaj kryterium:
                        (kryterium zmienia trzeci wykres)",
                        c('waterfront','view', 'condition', 'grade'))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("line"),
           plotOutput("piwnica"),
           plotlyOutput("bars")
        )
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$line <- renderPlot({
        house_data %>% 
            group_by(yr_built) %>% 
            summarise(cenasrrok = mean(10.764*price/sqft_living)) %>% 
            ggplot(aes(x = yr_built, y = cenasrrok)) +
            geom_line(color = "royalblue1", size = 1.5)+
            theme(axis.text = element_text(size = 14),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 20))+
            xlim(input$rokwyb)+
            labs(x = "Rok wybudowania",
                 y = "Srednia cena za m^2 (w $)",
                 title = paste0("Srednia cena za metr kwadratowy domow wybudowanych w latach ", input$rokwyb[1], " - ",
                                input$rokwyb[2]))
    })
    output$piwnica <- renderPlot({
        house_data %>% 
            mutate(m2_basement = sqft_basement/10.764) %>%
            ggplot(aes(x = m2_basement, y = cenazam_2))+
            geom_smooth(size = 1.5) +
            theme(axis.text = element_text(size = 14),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 20))+
            labs(x = "Powierzchnia ponizej poziomu gruntu w m^2",
                 y = "Srednia cena za metr kwadratowy",
                 title = "Cena za metr kwadratowy domu w zaleznosci od powierzchni pod poziomem gruntu")
    })
    output$bars <- renderPlotly({
        p <- house_data %>% 
            group_by_at(input$kol) %>% 
            summarise(Srednia_cena = mean(cenazam_2)) %>%
            ggplot(aes_string(x = input$kol, y = "Srednia_cena"))+
            geom_col(color = "royalblue1", fill = "royalblue1", width = 0.6)+
            scale_y_continuous(expand = c(0,0))+
            theme(axis.text = element_text(size = 14),
                  axis.title = element_text(size = 14),
                  plot.title = element_text(size = 14))+
            labs(x = input$kol,
                 y = "Srednia cena",
                 title = paste0("Srednia cena za metr kwadratowy w zaleznosci od ", input$kol))
        ggplotly(p, tooltip = c("x","y","label"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
