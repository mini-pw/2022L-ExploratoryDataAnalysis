library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinydashboard)
library(shinythemes)

house_data <- read.csv("https://raw.githubusercontent.com/MI2-Education/2022L-ExploratoryDataAnalysis/main/PraceDomowe/PracaDomowa1/house_data.csv")
house_data$bathrooms <- cut(house_data$bathrooms, breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 
                       labels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9"))
house_data$condition <- case_when(
    house_data$condition == 1 ~ "1 - bad",
    house_data$condition == 2 ~ "2 - mediocre",
    house_data$condition == 3 ~ "3 - good",
    house_data$condition == 4 ~ "4 - very good",
    house_data$condition == 5 ~ "5 - excellent"
)

data2 <- house_data %>% 
    filter(bathrooms ==  c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6"))

case_type <- function(value) {
    case_when(
        value == 0  ~ value == 0,
        value != 0 ~ value == 1
    )}

data1 <- house_data %>% 
    filter(yr_built >= 1950) %>% 
    mutate(yr_renovated = case_type(yr_renovated)) %>% 
    group_by(yr_built, condition, yr_renovated) %>% 
    summarise(mean_price = mean(price)) %>% 
    select(`Built year` = yr_built, `Mean price` = mean_price, condition, yr_renovated)

ui <- fluidPage(
    theme = shinytheme("cosmo"),
    
    navbarPage("Analysis of house prices in Seattle",
               tabPanel("Rooms and floors",
                        fluidRow(
                            column(12,
                                   h4(
                                       "This plot shows distribution of house prices based on their number of bathrooms (0 - 6), bedrooms(1 - 9) or floors(1 - 3,5)."
                                   ))
                        ),
                        fluidRow(
                            column(
                                4,
                                title = "Inputs",
                                selectInput("room", "Choose room or floor:", c("bedrooms", "bathrooms", "floors"))
                            )
                        ),
                        fluidRow(
                            column(
                                8,
                                title = "Plot 1",
                                plotlyOutput("roomsplot", height = 500)
                            )
                        )
                    ),
               tabPanel("Area",
                        fluidRow(
                            column(12,
                                   h4(
                                       "The plot shows house prices based on both living space and lot area of each property."
                                   ))
                        ),
                        fluidRow(
                            column(
                                4,
                                title = "Inputs",
                                numericInput("year1", "Choose built year (from 1900 to 2015):", 2000, min = 1900, max = 2015, step = 1)
                            )
                        ),
                        fluidRow(
                            column(
                                8,
                                title = "Plot 2",
                                plotlyOutput("areaplot", height = 500)
                            )
                        )
               ),
               tabPanel("House condition",
                        fluidRow(
                            column(12,
                                   h4(
                                       "The plot shows the average price of renovated houses for each condition of the house (1 - 5) based on  a year of construction."
                                   ))
                        ),
                        fluidRow(
                            column(
                                4,
                                selectInput("condition", "Choose condition of house:", unique(data1$condition))
                            )
                        ),
                        fluidRow(
                            column(
                                8,
                                title = "Plot 3",
                                plotlyOutput("linePlot", height = 500)
                            )
                        )
               )
    )
    
    

)

server <- function(input, output) {

    output$roomsplot <- renderPlotly({
        fig <- ggplot(data2, aes(x = as.character(data2[, input$room]), y = price)) +
            geom_boxplot(color = "lightpink4", fill = "lightpink2") +
            labs(title = paste("House prices depending on number of", input$room, ""),
                 x = paste("Number of", input$room, ""),
                 y = "Price")
        ggplotly(fig)
    })
    
    output$areaplot <- renderPlotly({
        fig <- plot_ly(
            data2[data2$yr_built == input$year1, ],
            x = ~sqft_living,
            y = ~sqft_lot, 
            type = 'scatter', 
            mode = 'markers',
            text = ~paste('Price: ', price),
            marker = list(size = ~price/50000, opacity = 0.5, color = rgb(0.8, 0.4, 0.50), 
                          line = list(color = rgb(0.8, 0.4, 0.50)))
        )
        fig <- fig %>% layout(title = paste("Prices of houses built in ", input$year1, " depending on property area", sep = ""),
                                plot_bgcolor = "#FBFBFB", 
                                xaxis = list(title = 'living area in square foots'), 
                                yaxis = list(title = 'lot area in square foots')
                              )
        fig
    })
    
    output$linePlot <- renderPlotly({
        fig <- ggplot(data1[data1$condition == input$condition & data1$yr_renovated == T, ], aes(x = `Built year` , y = `Mean price`)) +
            geom_line(linetype = "dashed", color = "indianred4", size = 0.4, alpha = 0.5) +
            geom_point(color="indianred4", size = 1, shape = 19) +
            scale_y_continuous(n.breaks = 7) +
            labs(x = "Built year ",
                 y = "Mean price",
                 title = paste0("Mean renovated house price for condition ",  input$condition))    
        ggplotly(fig)
    })
}

shinyApp(ui = ui, server = server)
