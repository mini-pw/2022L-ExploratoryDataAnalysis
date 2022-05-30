library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)

houses <- read.csv("https://raw.githubusercontent.com/MI2-Education/2022L-ExploratoryDataAnalysis/main/PraceDomowe/PracaDomowa1/house_data.csv")
options(scipen = 999)
houses <- houses %>% 
  mutate(sqm = sqft_living/9)

ui <- fluidPage(
  
  titlePanel("Wymarzony dom w Seattle"),
  
  
  fluidRow(
    column(5, offset = 1,
           
           sliderInput(
             "budget",
             label = h5("Zaznacz na suwaku swój budżet"),
             min = min(houses$price),
             max = max(houses$price),
             value = c(min(houses$price), max(houses$price)),
             step = 2000,
             sep = "."
             )
           ),
  column(5, offset = 1,         
           textInput(
             "HouseID",
             label = h5("Podaj ID domu, który cię interesuje.
                        (Możesz je znaleźć najeżdżając myszką na dom na mapce)"),
             placeholder = "ID"
           )
        )
    ),
  fluidRow(
    column(6,
           plotlyOutput("map")
    ),
    
    column(6,
           plotlyOutput("boxplot1"))
  )
  
)


server <- function(input, output) {
  
  output$map <- renderPlotly({
   houses1 <- houses %>% 
      filter(price >= input$budget[1] & price <= input$budget[2]) 
      
      plot_ly(data = houses1, lon = ~long, lat = ~lat, type = "scattermapbox",
              mode = "markers", marker = list(color = ~sqm, colorscale = "Blues",
                            reversescale = TRUE,
                            colorbar = list(title = "Powierzchnia")),
              text = paste0("Id: ", houses1$id, "<br>Powierzchnia: ", 
                            round(houses1$sqm, 2), " m^2",
                            "<br>Cena: ", houses1$price, " $"),
              hoverinfo = 'text'
             ) %>% 
      
      
      layout(title = list(text = "Lokalizacja domów w ograniczeniu budżetowym", 
                          font = list(size = 18)),
             mapbox = list(
               style = "open-street-map",
               zoom = 7.5,
               center = list(lat = (median(houses$lat)), lon = median(houses$long))
              )
            )
  })
  
  output$boxplot1 <- renderPlotly({
    
    houses2 <- houses %>% 
      filter(id == input$HouseID)
    
    annotations = list(
      list( 
        x = 0.5,  
        y = 1.0,  
        text = "Wykres powierzchni mieszkań w m^2",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),  
      
      list( 
        x = 0.5,  
        y = 0.46,  
        text = "Wykres cen mieszkań w $",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      )
    )
    
    vline <- function(x = 0, color = "lime") {
      list(
        type = "line", 
        y0 = 0, 
        y1 = 1, 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color)
      )
    }
    
    hline <- function(x = 0, color = "lime") {
      list(
        type = "line", 
        y0 = 0, 
        y1 = 1, 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color)
      )
    }
    
    
    box1 <- plot_ly(data = houses, x = ~sqm, type = "box", showlegend = FALSE,
                    jitter = 0.7, marker = list( size = 3 ), name = " " ) %>% 
      layout(shapes = hline(houses2$sqm) )
    
    box2 <- plot_ly(data = houses, x = ~price, type = "box", showlegend = FALSE,
                    jitter = 0.7, marker = list( size = 3 ), name = " " ) %>% 
      layout(shapes = list(hline(houses2$price)))
    
    subplot(box1, box2, margin = 0.08, nrows = 2) %>% 
      layout(annotations = annotations)
    
    # przykładowe ID - 9407000600
    
  })
  
}

shinyApp(ui = ui, server = server)