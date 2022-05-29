
library(shiny)
library(palmerpenguins)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
data("penguins")

df <- read.csv("house_data.csv")
mozliwe <- c(1,2,3,4,5)
mozliwe2 <- c(0,1)


ui <- fluidPage(
    
    titlePanel("Od czego zalezy cena nieruchomosci?"),
    textOutput("text"),
    
    fluidRow(
        column(6, 
               sliderInput("zakres",
                           "Wybierz zakres lat, w ktorych nieruchomosci zostaly wybudowane",
                           value = c(min(df$yr_built), max(df$yr_built)),
                           min = min(df$yr_built),
                           max = max(df$yr_built),
                           step = 1)
        ),
        column(6,
               selectInput("quality",
                           "Wybierz stan nieruchomosci",
                           mozliwe),
               selectInput("widok",
                           "Wybierz preferowane polozenie nieruchomosci",
                           mozliwe2),
               uiOutput("zmienna2")
        ),
    ),
    fluidRow(
        column(6,
               plotlyOutput("pointPlot")
        ),
        column(6,
               plotlyOutput("boxPlot")
        )
    ),
    fluidRow(
        column(6,
               
        ),
        column(6,
               "0 oznacza brak widoku na nabrzeze, 
                  natomiast 1 to nieruchomosc posiadajaca widok na nabrzeze."  
        )
    ),
    fluidRow(
        column(6, 
               "Z pierwszego wykresu możemy odczytać, ile kosztowało mieszkanie w zaleznosci od
          powierzchni w danym zakresie lat budowy."
        ),
        column(6, 
               "Z drugiego wykresu możemy dowiedzieć się ile mieszkań o danej cenie, w zależności od 
           stanu i widoku na nabrzeze, jest dostepnych."
        ),
        
    )
)


server <- function(input, output) {
    
    output$text <- renderText(({
        
        "Celem ponizszej aplikacji jest wsparcie uzytkownika w zdobyciu wiedzy,
        jakie czynniki maja wplyw na ceny nieruchomosci"
        
    }))
    
    output$pointPlot <- renderPlotly({
        
        plot_ly(df %>% 
                    filter(df$yr_built >= input$zakres[1],
                           df$yr_built <= input$zakres[2]), 
                x = ~sqft_living,
                y = ~price,
                color = ~yr_built) %>% 
            layout(title = "Zaleznosc ceny od powierzchni
            w wybranych latach", 
                   xaxis = list(title = 'Stopa kwadratowa powierzchni '), 
                   yaxis = list(title = 'Cena'))
        
        
    })
    
    output$boxPlot <- renderPlotly({
        
        ggplotly(
            df %>% 
                filter(condition == input$quality, waterfront == input$widok) %>%
                select(price) %>% 
                ggplot( aes(x=price/1000)) +
                geom_histogram()+
                ggtitle("Liczba mieszkań dostępnych w danej cenie")+
                xlab("Cena [w tysiacach]")+
                ylab("Liczba")
            
        )
        
    })
    
}


shinyApp(ui = ui, server = server)