

library(shiny)
library(ggplot2)
library(dplyr)
ramka <- read.csv("https://raw.githubusercontent.com/MI2-Education/2022L-ExploratoryDataAnalysis/main/PraceDomowe/PracaDomowa1/house_data.csv")


ui <- fluidPage(


    titlePanel("Two boxplots for houses in Seatlle"),


    sidebarLayout(
        sidebarPanel(
            sliderInput("powierzchnia",
                        "Area of the house",
                        min = min(ramka$sqft_living),
                        max = max(ramka$sqft_living),
                        value = c(min(ramka$sqft_living), max(ramka$sqft_living))),
            checkboxInput("woda", "Waterfront", TRUE),
            sliderInput("budowa",
                        "Year of built",
                        min = min(ramka$yr_built),
                        max = max(ramka$yr_built),
                        value = c(min(ramka$yr_built), max(ramka$yr_built)))
        ),


        mainPanel(
           plotOutput("wykres"),
           plotOutput("wykresik")
        )
    )
)


server <- function(input, output) {

    output$wykres <- renderPlot({
        pow <- ramka %>% 
            filter(sqft_living > input$powierzchnia[1],
                   sqft_living < input$powierzchnia[2],
                   waterfront == input$woda,
                   yr_built > input$budowa[1],
                   yr_built < input$budowa[2])
        ggplot(pow, aes(y = price))+
            geom_boxplot()+
            ggtitle("Price")+
            labs(y = "Price ($)", x = NULL)+
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())

        
        
    })
    
    output$wykresik <- renderPlot({
        ren <- ramka %>% 
            filter(sqft_living > input$powierzchnia[1],
                   sqft_living < input$powierzchnia[2],
                   waterfront == input$woda,
                   yr_built > input$budowa[1],
                   yr_built < input$budowa[2])
        ren$floors <- as.character(ren$floors)
        ggplot(ren, aes(x = floors, y = price))+
            geom_boxplot()+
            ggtitle("Number of floors and the price")+
            labs(y = "Price ($)", x = "Number of floors")
    })
}


shinyApp(ui = ui, server = server)
