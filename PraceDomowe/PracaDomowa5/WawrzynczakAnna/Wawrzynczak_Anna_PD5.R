#install.packages("shinythemes")
library(shinythemes)
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
library(bslib)

#df <- read.csv("house_data.csv")
#df <- house_data
df <- df %>% 
  mutate(cena_m2 = price/(sqft_living*0.09290304)) %>% 
  mutate(woda_widok = ifelse(waterfront == 1,"Tak","Nie"))
df <- df[order(df$condition),]


ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "united"),
  
  titlePanel("Analiza cen nieruchmości w Seattle w latach 1950-2015 w zależności od różnych czynników"),
  
    
    sidebarLayout(
      sidebarPanel(
      
        sliderInput("rok",
                    "Wybierz rok budowy mieszkania:",
                    min = 1950,
                    max = 2015,
                    value = c(1960, 1990)),
        
        checkboxGroupInput("stan","Wybierz stan mieszkania: ",
                               choices = unique(df$condition),
                               selected = 3),
        
        selectInput("kryt", "Wybierz kryterium:",
                    c(
                      "Liczba łazienek" = "bathrooms",
                      "Liczba sypialni" = "bedrooms"
                      )),
        
        textOutput('opis1'),
        
        ##drugi wykres
        textOutput('drugiwykres'),
        checkboxGroupInput("woda","Czy ma być z widokiem na wodę? ",
                           choices = unique(df$woda_widok),
                           selected = "Tak"),
        sliderInput("pietra","Wybierz liczbę pięter: ",
                  min = min(df$floors),
                  max = max(df$floors),
                  step = 0.5,
                  value = c(min(df$floors)+0.5, min(df$floors)+1)
                  ),
        textOutput('opis2'),
        
        
      
        ),
      
      
      mainPanel(
        plotlyOutput("Plot1"),
        plotlyOutput("Plot2")
      )
    
  )
  
)




server <- function(input, output) {
  
  output$opis1 <- renderText({paste0("Aplikacja w formie wykresu słupkowego przedstawia zależności między ceną za metr kwadratowy a liczbą łazienek i sypialni w mieszkaniu.
    Dane są wybierane względem lat ", input$rok[1],"-", input$rok[2],
                                     " i stanu nieruchomości o zaznaczonych wartości.")
  })
  
  output$opis2 <- renderText({paste0("Wykres liniowy prezentuje w jaki sposób cena za metr kwadratowy zależy od liczby pięter i posiadania widoku na wodę w mieszkaniu.
    Dane są grupowane na podstawie lat ", input$rok[1],"-", input$rok[2],
                                     ", zaznaczonej odpowiedzi przy widoku na wodę oraz liczby pięter od ",input$pietra[1]," do ",input$pietra[2],".")
  })
  
  output$drugiwykres <- renderText({" 
    "})
  
  output$Plot1 <- renderPlotly({
    
    if (input$kryt == "bedrooms"){
      nazwa_OX <- "sypialni"
    }
    if (input$kryt == "bathrooms"){
      nazwa_OX <- "łazienek"
    }
    wykres1 <- df %>% 
      group_by_at(input$kryt) %>% 
      filter(yr_built >= input$rok[1], yr_built <= input$rok[2],
             condition %in% input$stan
            ) %>% 
      na.omit() %>% 
      summarize(srednia_cena_m2 = mean(cena_m2)) %>% 
      
      ggplot(aes_string(x = input$kryt, y = "srednia_cena_m2")
                        )+
          geom_col(aes(text = paste0("Cena za metr: ",
                                     round(srednia_cena_m2),
                                     "
Liczba ", nazwa_OX,": ", get(input$kryt))
                       )
  )+
    labs(x = paste0("Liczba ", nazwa_OX," w mieszkaniu"),
         y = "Średnia cena za metr kwadratowy",
         title = paste0("Średnia cena za metr kwadratowy mieszkania a liczba ", nazwa_OX))

    ggplotly(wykres1, tooltip = c("text"))
    
  })
  
  output$Plot2 <- renderPlotly({
     wykres2<- df %>% 
      filter(yr_built >= input$rok[1], yr_built <= input$rok[2],
             woda_widok %in% input$woda,
             floors >= input$pietra[1],
             floors <= input$pietra[2]) %>%
      group_by(woda_widok,yr_built) %>%
      summarise(srednia_cena_m2 = mean(cena_m2)) %>% 
      plot_ly(
        x =~yr_built,
        y=~srednia_cena_m2,
        type = 'scatter',
        mode = 'lines',
        color =~woda_widok,
        colors = rainbow(2),
        showlegend = TRUE
        
      ) %>% 
      layout(xaxis = list(title = "Rok wybudowy",
        range = c(input$rok[1],input$rok[2])),
        yaxis = list(title = "Średnia cena za metr kwadratowy"),
             title = "
        Średnia cena za metr kwadratowy a rok wybudowania nieruchomości
        ",
        legend = list(title = "Z widokiem na wodę:"))
          
    
  
  })

}







shinyApp(ui, server)
