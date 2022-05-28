# Załadowanie bibliotek i danych
library(ggplot2)
library(shiny)
library(dplyr)
library(bslib)
library(thematic)
data <- read.csv("house_data.csv")

# Dostosowanie ramki danych do potrzeb wykresów
data <- data %>% 
  mutate(bathrooms = ceiling(bathrooms), 
         price_sqm = round(price / (0.0929 * sqft_living), 1), 
         waterfront = as.factor(waterfront)) %>% 
  select(bedrooms, bathrooms, price_sqm, lat, long,
         waterfront, yr_built, condition)

# Ustawienie tematu
my_theme <- bs_theme(bootswatch = "darkly",
                     base_font = font_google("Righteous"))
thematic_shiny(font = "auto")


ui <- fluidPage(
    titlePanel("Wymarzony dom w Seattle"),
    theme = my_theme,
    fluidRow(column(6, 
                    selectInput("n_bath", 
                                label = h3("Wybierz liczbę łazienek"), 
                                choices = sort(unique(data$bathrooms)), 
                                selected = 2), 
                    selectInput("n_bed", 
                                label = h3("Wybierz liczbę sypialni"), 
                                choices = sort(unique(data$bedrooms)), 
                                selected = 3)),
             column(6,
                    sliderInput("yr_built", 
                                label = "Wybierz zakres roku budowy", 
                                min = min(data$yr_built), 
                                max = max(data$yr_built), 
                                value = c(min(data$yr_built), 
                                          max(data$yr_built)), 
                                sep = ""), 
                    sliderInput("condition", 
                                label = "Wybierz zakres stanu budynku", 
                                min = min(data$condition), 
                                max = max(data$condition), 
                                value = c(min(data$condition), 
                                          max(data$condition)), 
                                step = 0.1))),
    fluidRow(column(6, 
                    plotOutput("Plot1")), 
             column(6, 
                    plotOutput("Plot2")))
)


server <- function(input, output){
  output$Plot1 <- renderPlot({
    data %>% filter(bathrooms == input$n_bath, 
                    bedrooms == input$n_bed, 
                    yr_built >= input$yr_built[1], 
                    yr_built <= input$yr_built[2], 
                    condition >= input$condition[1], 
                    condition <= input$condition[2]) %>% 
      ggplot(aes(x = long, y = lat, colour = waterfront)) +
      geom_point(shape = 15, size = 1) +
      coord_cartesian(xlim = c(min(data$long), max(data$long)), 
                      ylim = c(min(data$lat), max(data$lat))) +
      scale_colour_manual(values = c("sienna3", "dodgerblue2"), 
                          name = "Widok na zatokę lub jezioro", 
                          labels = c("nie", "tak")) +
      guides(colour = guide_legend(override.aes = list(size = 7))) +
      labs(title = "Położenie") +
      theme(axis.title = element_blank(), 
            axis.text = element_blank(), 
            plot.title = element_text(size = 20, hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_text(size = 16), 
            legend.text = element_text(size = 14))
  })
  output$Plot2 <- renderPlot({
    data %>% filter(bathrooms == input$n_bath, 
                    bedrooms == input$n_bed, 
                    yr_built >= input$yr_built[1], 
                    yr_built <= input$yr_built[2], 
                    condition >= input$condition[1], 
                    condition <= input$condition[2]) %>% 
      ggplot(aes(x = price_sqm)) +
      geom_histogram(binwidth = 250) +
      coord_cartesian(xlim = c(0, 10000), ylim = c(0, 510)) +
      labs(title = "Cena nieruchomości", x = "cena", y = "liczność") +
      theme(plot.title = element_text(size = 20, hjust = 0.5), 
            axis.title = element_text(size = 14))
  })
}


shinyApp(ui = ui, server = server)