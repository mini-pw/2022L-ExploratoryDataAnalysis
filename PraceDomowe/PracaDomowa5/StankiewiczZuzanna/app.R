library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
df <- read.csv("house_data.csv")
df$priceinthousands = df$price/1000
df <- df %>%
  rename('construction year' = yr_built) %>%
  mutate(waterfront = case_when(waterfront == "1" ~ "Yes",
            waterfront == "0" ~ "No"))
variables <- df[, c('bedrooms', 'bathrooms', 'floors')]

ui <- fluidPage(
  
  titlePanel("Prices in Seattle"),
  
  fluidRow(
    column(6, 
           selectInput("factor", "Choose factor:", names(variables))
    ),
    column(6, sliderInput("price_range",
                          "Price range (in thousands):",
                          min = min(df$priceinthousands),
                          max = max(df$priceinthousands),
                          value = c(min(df$priceinthousands), max(df$priceinthousands)))
    )
  ),
  fluidRow(
    column(6,
           plotOutput("DistPlot")
    ),
    column(6,
           plotOutput("boxPlot")
    )
  )
)


server <- function(input, output) {
  
  output$boxPlot <- renderPlot({
    df2 <- df[df$priceinthousands %in% min(range(input$price_range)):max(range(input$price_range)),]
    ggplot(df2, aes(x = factor(condition), y = priceinthousands, fill = factor(waterfront))) +
      geom_boxplot() +
      facet_wrap(~waterfront) +
      expand_limits(x = 0, y = 0)+
      scale_y_continuous(expand = c(0, 0)) +
      labs(title = "Prices distribution depending on condition level and waterfront presence", fill = "waterfront") +
      xlab(label = "condition") +
      ylab(label = "price in thousands of dollars") 
      
    
    #input$price_range, [df$priceinthousands %in% c(700,5000)
  })
  
  output$DistPlot <- renderPlot({
    price_by_rooms <- df %>%
      group_by(zmienna = df[,colnames(df) == input$factor]) %>%
      summarise('meanprice' = mean(price)/1000) %>%
      mutate(liczba_obserwacji = count(df,df[,colnames(df) == input$factor])[,2])
    
    ggplot(price_by_rooms, aes(x = factor(zmienna), y = meanprice, fill = liczba_obserwacji)) +
      geom_col() +
      scale_fill_gradient(low = "#9ecae1", high = "#3182bd") +
      labs(title = paste("Average prices in Seattle depending on number of", input$factor), 
           fill = "number of houses") +
      xlab(label = paste("number of", input$factor)) +
      ylab(label = "price in thousands of dollars") +
      expand_limits(x = 0, y = 0)+
      scale_y_continuous(expand = c(0, 0))
    
  })
  
}


shinyApp(ui = ui, server = server)