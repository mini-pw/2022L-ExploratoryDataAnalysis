#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)
library(readr)
df <- read_csv("house_data.csv")
df <- df %>% filter(bedrooms != 33)
df$waterfront[df$waterfront == 1] <- "Tak"
df$waterfront[df$waterfront == 0] <- "Nie"
df$price <- df$price/1000


df_bathroom <- df %>% 
  select(bathrooms, price) %>% 
  group_by(bathrooms) %>% 
  summarise(mean = mean(price),
            sd = sd(price)) %>% 
  mutate(udogodnienie = 'bathrooms')
df_bathroom <- na.omit(df_bathroom)
colnames(df_bathroom)[1] <- 'ilość'
df_bedrooms <- df %>% 
  select(bedrooms, price) %>% 
  group_by(bedrooms) %>% 
  summarise(mean = mean(price),
            sd = sd(price)) %>% 
  mutate(udogodnienie = 'bedrooms') 
df_bedrooms <- na.omit(df_bedrooms)
colnames(df_bedrooms)[1] <- 'ilość'
df_floors <- df %>% 
  select(floors, price) %>% 
  group_by(floors) %>% 
  summarise(mean = mean(price),
            sd = sd(price)) %>% 
  mutate(udogodnienie = 'floors')
df_floors <- na.omit(df_floors)
colnames(df_floors)[1] <- 'ilość'
ud <- rbind(df_bathroom,df_bedrooms, df_floors)


# Define UI for application
ui <- fluidPage(


    # Application title
    titlePanel("Ceny nieruchomości w Seattle"),

    #sidebarLayout(
        #sidebarPanel(
           # selectInput("yr_built", "Wybierz rok budowy:", unique(df[order(df$yr_built), ]$yr_built) ),
            #selectInput("udogodnienie", "Wybierz udogodnienie:", unique(ud$udogodnienie)),
            #checkboxInput("point_plot", "Zamień barplot na pointplot", FALSE)
            #),
              
        
      #  mainPanel(
          
           # plotOutput("powierzchniaPlot"),
          #  p("Ceny większości mieszkań znajdują się w przedziale 500 - 2000 tys. Cena mieszkanie z widokiem na nabrzeże jest przeważnie droższa."),
           # plotOutput("udogodnieniaPlot"),
            #p("Wraz ze wzrostem liczby łazienke rośnie cena mieszkania. 
            #Jednak, szerokie 'wąsy' wychodzące ze słupków oraz wykres kropkowy 
             # pokazują nam, że jeżeli zależy nam na mieszkaniu o dużej liczbie łazienek,
            #  ale jednocześnie mamy ograniczony budżet to nadal możemy znaleść takie mieszkanie.
             # Patrząc na liczbę sypialni widzimy, że mieszkania posiadające 5-6 takich pokoi są najdroższe.
              #Liczba pięter nie wpływa znacząco na cenę mieszkania, jednak te o 2 piętrach mają tendencję do występowania po droższej cenie.",
              #span("Chcąc wybrać najtańsze mieszkanie powinniśmy się rozglądać za mieszkaniem mającym 0-1 sypialnię oraz 0-1 łazienkę.", style = "color:blue"),
              #)
            
#)
    #)
    fluidRow(
      column(3, 
             selectInput("yr_built", "Wybierz rok budowy:", unique(df[order(df$yr_built), ]$yr_built) )
                
      ),
      
      column(8,
             plotOutput("powierzchniaPlot", height = 360),
             p("Ceny większości mieszkań znajdują się w przedziale 500 - 2000 tys. 
               Rok budowy nie wpływa istotnie na cenę. 
               Cena mieszkań o większej powierzchni jest droższa niż 
               cena mniejszych mieszkań. Dodatkowo, jeśli chcemy mieć widok 
               na nabrzeże to musimy przewżnie więcej zapłacić.")
             
      )
    ),
    fluidRow(
      column(3,
             selectInput("udogodnienie", "Wybierz udogodnienie:", unique(ud$udogodnienie)),
             checkboxInput("point_plot", "Zamień barplot na pointplot", FALSE) ),
      
     
     column(8,
            plotOutput("udogodnieniaPlot", height = 390) ,
      p("Wraz ze wzrostem liczby łazienke rośnie cena mieszkania. 
            Jednak, szerokie 'wąsy' wychodzące ze słupków oraz wykres kropkowy 
              pokazują nam, że jeżeli zależy nam na mieszkaniu o dużej liczbie łazienek,
              ale jednocześnie mamy ograniczony budżet to nadal możemy znaleźć takie mieszkanie.
              Patrząc na liczbę sypialni widzimy, że mieszkania posiadające 5-6 takich pokoi są najdroższe, ale tu także znajdziemy coś w tańszych cenach.
              Liczba pięter nie wpływa znacząco na cenę mieszkania, jednak te o 2 piętrach mają tendencję do występowania po droższej cenie.",
        span("Chcąc wybrać najtańsze mieszkanie powinniśmy się rozglądać za mieszkaniem mającym 0-1 sypialnię oraz 0-1 łazienkę.", style = "color:blue"),
      ))
     
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$powierzchniaPlot <- renderPlot({
        ggplot(df[df$yr_built == input$yr_built, ], aes(y = price, x = sqft_living, color = waterfront))+
           geom_point()+
            ylab("Cena w tysiącach")+
            xlab("Powierzchnia mieszkalna w stopach kwadratowych")
      
 })
    
    output$udogodnieniaPlot <- renderPlot({
        p <- ggplot(ud[ud$udogodnienie == input$udogodnienie,])+
            geom_col(aes(x = ilość, y = mean))+
        geom_errorbar(aes(x = ilość, ymin = mean, ymax = mean + sd))+
        xlab("Ilość udogodnienia występująca w domu")+
        ylab("Cena w tysiącach")
      
      if(input$point_plot){
        p <- ggplot(df, aes_string(x = input$udogodnienie, y = 'price'))+
          geom_point()
      }
      p
    })
    output$value <- renderText({ input$caption })
}



#unique(factor(df$bathrooms))
# Run the application 
shinyApp(ui = ui, server = server)
