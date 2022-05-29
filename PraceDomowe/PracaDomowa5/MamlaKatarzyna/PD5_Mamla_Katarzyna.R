
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(bslib)
thematic::thematic_shiny(font = "auto")
df <- read.csv("house_data.csv") 
ui <- shinyUI(fluidPage(
  theme = bs_theme( bg = "#FBFDFF", fg = "#797979"),
                
  
  
    titlePanel(h1("Nieruchomosci w Seattle", align = "center")),
    textOutput("text"),
    sidebarLayout(
        sidebarPanel(
            selectInput("year",
                        "Rok budowy",
                        sort(unique(df$yr_built)))),
        sidebarPanel(
        selectInput("cond", 
                    "Stan nieruchomosci",
                    sort(unique(df$condition))),
        helpText("1 - bardzo slaby,
                  2 - slaby,
                  3 - dostateczny,
                  4 - dobry,
                  5 - bardzo dobry"))),

        mainPanel(
            plotOutput("boxPlot"),
            tableOutput("table"),
            plotOutput('linePlot')
           
    )
))

server <- shinyServer(function(input, output) {
 
  
  output$text <- renderText(({
    
    paste("Aplikacja zawiera analize cen nieruchomosci wystawionych na sprzedaz w Seattle.", 
          "Dostepnych jest", 
          nrow(df), 
          "posiadlosci.",
          "Dostepne nieruchomosci zostaly zbudowane w latach",
          min(df$yr_built, na.rm = TRUE), "-", max(df$yr_built,na.rm = TRUE))
    
  }))
    
    output$boxPlot <- renderPlot({
        
        df %>%  select(sqft_living, price, yr_built) %>%
            mutate(sqm_living = sqft_living/10.764) %>% 
            mutate(Cost_per_sq_meter =price/sqm_living ) %>%
            select(- c(sqft_living,price,sqm_living)) %>%  
            arrange(-desc(yr_built))-> tab 
        tab %>%  group_by(yr_built) -> p 
        
        
        p <- ggplot(p[p$yr_built == input$year, ], aes(x = factor(0), y = Cost_per_sq_meter)) +
            geom_boxplot() +
            xlab(NULL)+
            labs(x = "",
                 y = "Cena za metr kwadratowy ($)",
                 title = paste0("Ceny za metr kwadratowy dla nieruchomosci wybudowanych w roku", ' ', input$year, ''))+
            theme_bw() + 
          geom_boxplot(fill="#CCCCD9") +
          theme(plot.title = element_text(size = 20),
                axis.title.y = element_text(size=16),
                axis.text.y = element_text(size=14),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank())+
          scale_x_discrete(breaks = NULL) 
        
        p
    
})
    
    output$table <- renderTable({
        
     tab %>% 
            filter(yr_built == input$year) %>% 
            group_by(yr_built) %>% rename("Rok budowy"= 'yr_built') %>% 
            summarise('Srednia Cena' = mean(Cost_per_sq_meter, na.rm = TRUE),
                      'Cena Minimalna' = min(Cost_per_sq_meter, na.rm = TRUE),
                      'Cena Maksymalna'= max(Cost_per_sq_meter, na.rm = TRUE))
        
    })
    
    output$linePlot <- renderPlot({
      
      
      df %>%  select( price, yr_built,condition) %>%
        group_by(yr_built,condition, .add = TRUE) %>% 
        mutate(srednia = mean(price))-> p1
  
      
      p1 <- ggplot(p1[p1$condition == input$cond, ], aes(x = yr_built, y = srednia)) +
        geom_line() +
          
        labs(x = 'Rok zbudowania',
             y = "Cena nieruchomosci ($)",
             title = paste0("Srednia cena nieruchomosci w stanie", ' ', input$cond, ''))+
        theme_bw() +
        theme(plot.title = element_text(size = 20),
              axis.title = element_text(size=16),
              axis.text = element_text(size=14))
      
      p1
      
    })

})


shinyApp(ui, server)