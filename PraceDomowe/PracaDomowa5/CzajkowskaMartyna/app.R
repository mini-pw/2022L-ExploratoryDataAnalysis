library(shiny)
library(plotly)
library(dplyr)
library(viridis)
df <- read.csv("https://raw.githubusercontent.com/MI2-Education/2022L-ExploratoryDataAnalysis/main/PraceDomowe/PracaDomowa1/house_data.csv") 
df1 <- df %>% 
    rename(Sypialnie = bedrooms) %>% 
    filter(Sypialnie != 33) %>%
    group_by(Sypialnie) %>%
    summarise(Cena = round(mean(price)))
df2 <- df %>% 
    mutate(dekada = floor(yr_built / 10) * 10) %>% 
    group_by(dekada, view) %>%
    summarise(srednia = round(mean(price))) %>% 
    as.data.frame()


ui <- fluidPage(
    
    titlePanel("Cena nieruchomości w Seattle"),
    fluidRow(
        column(6, 
               textOutput("text"),
               HTML("<hr>"),
               p("Obok możesz wybrać swój maksymalny budżet. Poniżej zaznacz, czy chcesz, aby twoj budżet został dodany do wszytkich wykresów."),
               p(),
               HTML("<hr>"),
               p("Poniższy wykres to histogram przedstawiający zależność między liczbą sypialni a średnią ceną nieruchomości."),
               plotlyOutput("pointPlot")
        ),
        column(6,
               HTML("<hr>"),
               sliderInput("budzet", "Zaznacz swój maksymalny budżet", min = min(df$price), max = max(df$price), value = 1000000, step = 25000),
               checkboxInput("liniaBudzetu", "Linia twojego budżetu", TRUE),
               HTML("<hr>"),
               p("Drugi wykres przedstawia zależność między dekadą wybudowania domu a średnią ceną nieruchomości.
                 Poniżej wybierz interesujące Cię widoki na dom."),
               checkboxGroupInput("view", "Wybierz widok na dom", unique(df2$view), 
                                  selected = unique(df2$view), inline =TRUE),
               plotlyOutput("Plot")
        )
    ))

server <- shinyServer(function(input, output) {
    
    output$text <- renderText({
        paste("Aplikacja pomaga w przeanalizowaniu, co wpływa na cenę nieruchomości w Seattle 
              na podstawie zbioru danych, zawierającym informacje o",
              nrow(df), "domach.")
    })
    
    
    
    output$Plot <- renderPlotly({
        p <- df2 %>% 
            filter(view %in% input$view) %>% 
            ggplot(aes(x = dekada, y = srednia, 
                        color = as.factor(view)))+
            geom_line(size = 1)+
            scale_color_viridis(discrete = TRUE, option = "D")+
            xlim(min(df2$dekada), max(df2$dekada))+
            scale_y_continuous(labels = scales::label_number(suffix = " M", scale = 1e-6),
                               limits = c(min(df2$srednia), max(df2$srednia)))+
            labs(y = "Średnia cena", x = "Dekada wybudowania domu",
                 color = "Widok na dom",
                 title = "Zależność ceny od dekady wybudowania nieruchomości")+
            theme_minimal()+
            theme(plot.title = element_text(size=9))
        if (input$liniaBudzetu){
            p <- p +
                geom_hline(yintercept=input$budzet, 
                           color = "red")+
                geom_text(aes(x=1920,
                              y=input$budzet+50000,label="Twój budżet"),hjust=-0.5, size=3,
                          color = "red")+
                scale_y_continuous(labels = scales::label_number(suffix = " M", scale = 1e-6),
                                   limits = c(min(df2$srednia), max(df2$srednia, input$budzet+100000)))
        }
        ggplotly(p)
        
    })
    
    output$pointPlot <- renderPlotly({
        
        p <- df1 %>% 
            ggplot(aes(x = Sypialnie, y = Cena)) +
            geom_col(fill = "Navy")+
            labs(y = "Średnia cena", x = "Liczba sypialni",
                 title = "Średnia cena w zależnosci od liczby sypialni")+
            scale_y_continuous(labels = scales::label_number(suffix = " M", scale = 1e-6))+
            theme_minimal()+
            theme(plot.title = element_text(size=10))
        
        if (input$liniaBudzetu){
            p <- p +
                geom_hline(yintercept=input$budzet, 
                           color = "red")+
                geom_text(aes(x=2,
                              y=input$budzet+50000,label="Twój budżet"),hjust=-0.5, size=3,
                          color = "red")
        }
        ggplotly(p) 
    })
    
})


shinyApp(ui, server)