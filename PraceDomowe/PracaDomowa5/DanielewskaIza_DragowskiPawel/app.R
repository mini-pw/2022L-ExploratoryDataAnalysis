library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

data <- read.csv("house_data.csv")
data <- data %>% 
  mutate(Cena_za_m = 10.764*price/sqft_living)
rangey <- c(min(data$Cena_za_m,na.rm=T)-10,
            max(data$Cena_za_m,na.rm=T)+10)

ui <- fluidPage(
  
        titlePanel("Ceny domów marzeń w Seattle"),
    
        fluidRow(
            column(12,
                sidebarLayout(
                    sidebarPanel(
                        radioButtons("woda", 
                                     label = h3("Czy chcesz widok na wodę?"), 
                                     choices = list("Tak" = 1, 
                                                    "Nie" = 2,
                                                    "Obojętnie" = 3),
                                     selected = 3),
                        
                        radioButtons("piwnica", 
                                     label = h3("Czy chcesz mieć piwnice?"),
                                     choices = list("Tak" = 1, 
                                                    "Nie" = 2, 
                                                    "Obojętnie" = 3), 
                                     selected = 3),
                        p(
                          'Wykres przedstawia rozkład ceny za m^2 mieszkań w 
                          zależności od tego czy dane mieszkanie jest położone 
                          nad wodą i czy ma piwnicę.'
                        )
                    ),
                    mainPanel(
                          plotlyOutput("histogram")
                    )
                )
            )
        ),
        
        fluidRow(
            column(12,
                sidebarLayout(
                    sidebarPanel(
                        sliderInput("rok",
                                    label = h3("Określ rok wybudowania:"),
                                    min = min(data$yr_built,na.rm = T), 
                                    max = max(data$yr_built,na.rm = T), 
                                    value = c(1950,1990),
                                    sep="",
                                    step = 1),
             
                        checkboxGroupInput("pietra", 
                                           label = h3("Wybierz liczbę pięter:"),
                                           choices =unique(data$floors), 
                                           selected = c(1,2)),
                        p(
                          'Wykres przedstawia rozkład ceny za m^2 mieszkań w 
                          zależności od roku wybudowania, stanu mieszkania i 
                          liczby pięter.'
                        )
                    ),
                    mainPanel(
                        plotlyOutput("boxplot")
                    )
                )
            )
        )
)


server <- function(input, output) {
    
  
    output$histogram <- renderPlotly({
      data <- (if (input$woda == 1){
        data %>% filter(waterfront==1)
        }
      else if (input$woda == 2) {
        data %>% filter(waterfront == 0)
        }
      else {
        data
        })
      
      data <- (if (input$piwnica == 1){
        data %>% filter(sqft_basement > 0)
      }
      else if (input$piwnica == 2) {
        data %>% filter(sqft_basement == 0)
      }
      else {
        data
      })
      
      data %>% 
        plot_ly(x = ~Cena_za_m, 
                type = "histogram",
                marker = list(color = "#7F85C0",
                              line = list(color = "#3C2986",
                                          width = 0.2)),
                nbinsx = 100,
                hovertemplate = "W zakresie (%{x}) zł za m^2 <br>jest %{y} nieruchomości.<extra>To dużo do wyboru!</extra>") %>% 
        layout(title = "Rozkład ceny nieruchomości za m^2",
               xaxis = list(title = "Cena za m^2", range = rangey),
               yaxis = list(title = "Liczba nieruchomości"))

    })
    
    
    
    output$boxplot <- renderPlotly({
      data2 <- data %>%
        filter(floors==input$pietra & yr_built>=input$rok[1] & 
                 yr_built<=input$rok[2])
      
      fig<-ggplot(data2, aes(y=Cena_za_m, x = factor(condition)))+
        geom_boxplot(color = "#3C2986", fill = "#7F85C0")+
        labs(title = "Cena za m^2 w zależności od stanu mieszkania",
          x = "Stan mieszkania",
             y = "Cena za m^2")+
        scale_y_continuous(limits = rangey)+
      theme_bw()
      ggplotly(fig)

    })
}


shinyApp(ui = ui, server = server)
