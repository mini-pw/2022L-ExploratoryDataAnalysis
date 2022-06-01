#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(plotly)

df2 <- df %>% 
    select(id, price, yr_renovated, lat, long)%>% 
    filter(yr_renovated != 0)%>% 
    mutate(kwartyl = ifelse(price < quantile(price, prob = 0.25), "1 kwartyl",
                            ifelse(price < quantile(price, prob = 0.5), '2 kwartyl',
                                   ifelse(price < quantile(price, prob = 0.75), '3 kwartyl', '4 kwartyl'))))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Mieszkania w Seattle - renowacje i koszta"),
    
    fluidRow(
        column(10,
               sliderInput("grade",
                           label="Wybierz jakosc nieruchomosci:",
                           min = min(df$grade,na.rm=TRUE),
                           max = max(df$grade,na.rm=TRUE),
                           value = c(min(df$grade,na.rm=TRUE),max(df$grade,na.rm=TRUE)),
                           step=1)
        ),
        column(12,
               mainPanel(plotlyOutput("distPlot"))
        )
        ),
    
    fluidRow(
        column(10,
               sidebarPanel(
                   selectInput("kwartyl", "Wybierz kwartyl ceny nieruchomosci", unique(df2$kwartyl))
               ),
        column(12,
               mainPanel(plotlyOutput("mapPlot"))
            )
        )
    )
)

server <- function(input, output) {
    
    output$distPlot <- renderPlotly({
        
        df1 <- df %>% 
            select(yr_built,grade,sqft_living,price) %>% 
            mutate(m2price=10.764*price/sqft_living) %>% 
            select(m2price,yr_built,grade) %>% 
            filter(grade >= input$grade[1], grade <=input$grade[2])
        plot_ly(df1, x=~yr_built,y=~m2price, split=~grade, type = "scatter") %>% 
            layout(title ='Cena mieszkania i rok budowy wzgledem jakosci')
        
        
    })
    
    output$mapPlot <- renderPlotly({
        
        df3 <- df2%>% filter(kwartyl == input$kwartyl)
        
        fig <- df3 %>% 
            plot_ly(
                lat = ~lat,
                lon = ~long,
                type = "scattermapbox",
                size = 1.5,
                color = ~yr_renovated,
                mode= "markers",
                marker = list(color = df3$yr_renovated)) %>% 
            layout(
              title = 'Rozmieszczenie mieszkan w zaleznosci od ceny z oznaczeniem roku ostatniego remontu',
                mapbox = list(
                    style = 'open-street-map',
                    mode = 'markers',
                    zoom =8,
                    center = list(lon = -122.2, lat = 47.54),
                    hovertext = df$yr_renovated))
        fig
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)