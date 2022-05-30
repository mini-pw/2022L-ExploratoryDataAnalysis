library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

house_data <- read.csv("house_data.csv")

house_data$Sector[house_data$lat >= mean(house_data$lat) & house_data$long >= mean(house_data$long)] <- '1'
house_data$Sector[house_data$lat >= mean(house_data$lat) & house_data$long <= mean(house_data$long)] <- '2'
house_data$Sector[house_data$lat <= mean(house_data$lat) & house_data$long <= mean(house_data$long)] <- '3'
house_data$Sector[house_data$lat <= mean(house_data$lat) & house_data$long >= mean(house_data$long)] <- '4'


ui <- shinyUI(fluidPage(
  titlePanel("Co wp³ywa na cenê nieruchomoœci?"),
  textOutput("text"),
  sidebarLayout(
    sidebarPanel(
      selectInput("kategoria", "Kategoria", c("³azienki", "sypialnie", "rok budowy", "powierzchnia[sqft]")),
      checkboxInput("s1", "Sektor 1", FALSE),
      checkboxInput("s2", "Sektor 2", FALSE),
      checkboxInput("s3", "Sektor 3", FALSE),
      checkboxInput("s4", "Sektor 4", FALSE)
    ),
    mainPanel(
      plotOutput("pointPlot")
    )
  )
))

plot_bathrooms <- function(sectors){
  p <- ggplot(house_data[is.element(house_data$Sector, sectors),], aes(x = bathrooms, y = price, color = Sector))+
    geom_point() +
    scale_y_continuous(labels = comma)+
    labs(x= "liczba ³azienek", y = "cena")
  p
}
plot_bedrooms <- function(sectors){
  p <- ggplot(house_data[is.element(house_data$Sector, sectors),], aes(x = bedrooms, y = price, color = Sector))+
    geom_point() +
    scale_y_continuous(labels = comma)+
    labs(x= "liczba sypialni", y = "cena")
  p
}
plot_year_built <- function(sectors){
  p <- ggplot(house_data[is.element(house_data$Sector, sectors),], aes(x = yr_built, y = price, color = Sector))+
    geom_point() +
    scale_y_continuous(labels = comma)+
    labs(x= "rok budowy", y = "cena")
  p
}
plot_sqft <- function(sectors){
  p <- ggplot(house_data[is.element(house_data$Sector, sectors),], aes(x = sqft_living, y = price, color = Sector))+
    geom_point() +
    scale_y_continuous(labels = comma)+
    labs(x= "powierzchnia[sqft]", y = "cena")
  p
}


server <- shinyServer(function(input, output) {
  output$text <- renderText({ "Miasto zosta³o podzielone przeze mnie na cztery sektory wzglêdem œredniej szerokoœci i d³ugoœci geograficznej. Wybieraj¹c konkretny sektor mo¿na 
             zauwa¿yæ jak wygl¹daj¹ ceny w ró¿nych czêœciach miasta." })
  
  output$pointPlot <- renderPlot({
    sectors <- c()
    if(input$s1) sectors<-append(sectors, '1')
    if(input$s2) sectors<-append(sectors, '2')
    if(input$s3) sectors<-append(sectors, '3')
    if(input$s4) sectors<-append(sectors, '4')
    if(input$kategoria == "³azienki")
    {
      p<- plot_bathrooms(sectors)
    }
    else if(input$kategoria == "sypialnie")
    {
      p<- plot_bedrooms(sectors)
    }
    else if(input$kategoria == "rok budowy")
    {
      p<- plot_year_built(sectors)
    }
    else
    {
      p<- plot_sqft(sectors)
    }
    p
    
  })
  
})


shinyApp(ui, server)
