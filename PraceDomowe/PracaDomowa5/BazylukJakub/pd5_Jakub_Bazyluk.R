library(shiny)
library(ggplot2)
library(dplyr)
library(intrval)
df <- read.csv("house_data.csv")

ui <- shinyUI(fluidPage(
  titlePanel("Mieszkania w Seattle"),
  sidebarLayout(
    sidebarPanel(
      selectInput("bedroom", "Podaj liczbę sypialni", sort(unique(df$bedrooms))),
      checkboxInput("liniaTrendu", "Linia trendu", FALSE)
    ),
    mainPanel(
      plotOutput("pointPlot"),
      h4("Wykres przedstawia ceny mieszkań w Seattle w zależności od liczby sypialni, która można zmienić na pasku.
         Aby zobaczyć jak zmienia się trend należy zaznaczyć przycisk z linią trendu."),
      plotOutput("pointPlot2"),
      sliderInput("price", "Cena +-100:",
                  min=0, max=3000, value=1000, step=100,
                  animate=animationOptions(1000)),
      h4("Wykres przedstawia ilość mieszań o danym stanie, w zależności od podanej ceny plus/minus 100 000 dolarów. Aby zobaczyć
         ciągłą zmianę należy kliknąć w animację.")

      
    )
  )
))
server <- shinyServer(function(input, output) {
  df <- df %>% 
    mutate(sqm = sqft_living* 0.09290304, price_k = price/1000)
  output$pointPlot <- renderPlot({
    p <- ggplot(df[df$bedrooms == input$bedroom,], aes(x = sqm, y = price_k, color = cut(waterfront, c(-Inf, 0.5, Inf)))) +
      geom_point(size = 3, shape = 16) +
      labs(x = "Powierzchnia w metrach kwadratowych",
           y = "Cena w w tysiącach dolarów",
           color = "Widok na ocean",
           title = paste0("Ceny mieszkań z ", input$serial, "sypialniami"))+
      scale_color_manual(values = c("(-Inf,0.5]" = "blue",
                                    "(0.5, Inf]" = "green"),
                         labels = c("Nie", "Tak"))
    
    if (input$liniaTrendu){
      p <- p + geom_smooth(se = FALSE)
    }
    
    p
    
  })
  output$pointPlot2 <- renderPlot({
    p2 <- ggplot(df[df$price_k <= input$price+100 & df$price_k >= input$price-100,], aes(condition)) +
      geom_histogram(fill = "magenta", binwidth = 1, bins = 5) +
      labs(x =  "Stan mieszkania",
           y = "Ilość mieszkań",
           title = paste0("Stan mieszkania w zależności od ceny"))
    p2
  })

})
shinyApp(ui, server)