library(shiny)
library(ggplot2)
library(dplyr)
library(PogromcyDanych)
library(plotly)
seattle <- read.csv("house_data.csv") 
ui <- fluidPage(
  
  
  titlePanel("Praca domowa 5"),
  textOutput('wprow'),
  textOutput('wprow1'),
  
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_built",
                  "Rok budowli",
                  min = min(seattle$yr_built),
                  max = max(seattle$yr_built),
                  value = c(min(seattle$yr_built),max(seattle$yr_built))),
      sliderInput("price",
                  "Cena",
                  min = min(seattle$price),
                  max = max(seattle$price),
                  value = c(min(seattle$price),max(seattle$price))),
      checkboxGroupInput("cond",
                  "Stan",
                  selected = 1,
                  choices = sort(unique(seattle$condition),decreasing =  FALSE)
                  )
     
    ),
    
    
    mainPanel(
      plotOutput("wykres_1"),
      plotOutput("wykres_2")
    )
  )
)


server <- shinyServer(function(input, output) {
  
  output$wprow <- renderText({"Aplikacja zawiera interaktywną wizualizację liczby dostępnych mieszkań w Seattle-u w zależności od liczby sypialień lub lazienek."
  })
  output$wprow1 <- renderText({"W aplikacji jest możliwość wybrać zakresy roku budowli i ceny, stan mieszkania"
  })
  
  output$wykres_1 <- renderPlot({
    seattle_bedrooms<-seattle %>% 
      filter(yr_built >= input$year_built[1] & yr_built <= input$year_built[2]) %>% 
      filter(condition == input$cond)%>% 
      filter(price >= input$price[1] & price <= input$price[2])
    ggplot(seattle_bedrooms, aes(x = bedrooms))+
      geom_bar()+
      ggtitle("Liczba dostępnych mieszkań w zależności od liczby sypialień")+
      labs(y = "Liczba dostępnych mieszkań", x ="Liczba sypialeń" )
      
    
    
    
  })
  
  output$wykres_2 <- renderPlot({
    seattle_bathrooms <- seattle %>% 
      filter(yr_built >= input$year_built[1] & yr_built <= input$year_built[2]) %>% 
      filter(condition == input$cond) %>% 
      filter(price >= input$price[1] & price <= input$price[2])
    ggplot(seattle_bathrooms, aes(x = bathrooms))+
      geom_bar()+
      ggtitle("Liczba dostępnych mieszkań w zależności od liczby sypialień")+
      labs(y = "Liczba dostępnych mieszkań", x ="Liczba łazienek")
  })
})


shinyApp(ui = ui, server = server)








