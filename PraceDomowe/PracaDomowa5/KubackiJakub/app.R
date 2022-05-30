#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library("scales")

df <- read.csv("E:/RStudio/eskplo/Jakub_Kubacki_PD5/house_data.csv")

# source for distance calculations:
# https://www.movable-type.co.uk/scripts/latlong.html
radius <- 6371
distance <- function(lat1,lat2,long1,long2){
    phi1 <- lat1 * (pi/180)
    phi2 <- lat2 * (pi/180)
    dphi <- phi2 - phi1
    dlam <- (long2-long1)*(pi/180)
    
    a <- sin(dphi/2)*sin(dphi/2) + cos(phi1)*cos(phi2)*sin(dlam/2)*sin(dlam/2)
    c <- 2*atan2(sqrt(a),sqrt(1-a))
    c*radius
}

ui <- fluidPage(

    # Application title
    titlePanel("House prices in Seattle"),

    sidebarLayout(
        sidebarPanel(
            
            sliderInput(
                "latit",
                "Select the desired latitude:",
                min = 47.15,
                max = 47.78,
                value = 47.62
            ),
            sliderInput(
                "longi",
                "Select the desired longitude:",
                min = -122.52,
                max = -121.31,
                value = -122.35
            ),
            sliderInput(
                "rad",
                "Select the desired radius (in km):",
                min = 0.1,
                max = 20.0,
                value = 2.0
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("count"),
            plotOutput("locToPrice"),
            plotOutput("pricePerSqm")
        )
    )
)


server <- function(input, output) {
    output$count <- renderText({
        paste("Number of housing offers found: ",
                df %>%
                    select(lat,long) %>%
                    filter(distance(lat,input$latit,long,input$longi) < input$rad) %>%
                    count())
    })
    output$locToPrice <- renderPlot({
        df %>%
            select(price,bedrooms,lat,long) %>%
            filter(distance(lat,input$latit,long,input$longi) < input$rad) %>%
            mutate("beds" = factor(bedrooms)) %>%
            ggplot(aes(x = beds, y=price)) +
            geom_boxplot(fill = "light blue") +
            scale_y_log10(labels = comma_format(big.mark = " ",
                                                decimal.mark = ".")) +
            labs(x = "Number of bedrooms", y = "Price in USD") +
            coord_flip()
    })
    output$pricePerSqm <- renderPlot({
        df %>%
            select(price,sqft_living,lat,long) %>%
            filter(distance(lat,input$latit,long,input$longi) < input$rad) %>%
            mutate("sqm_price" =price/(sqft_living/10.76)) %>%
            ggplot(aes(x = sqm_price)) +
            labs(x = "Price per m^2", y = "") +
            scale_y_continuous(labels = NULL) +
            geom_density(fill = "light green")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
