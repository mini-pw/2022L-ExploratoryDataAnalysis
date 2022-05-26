library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)


ui <- fluidPage(
    
    titlePanel("Nieruchomoœci w Seattle"),

    fluidRow(
        sidebarLayout(
            sidebarPanel(
                textOutput("opis1"),
                sliderInput("year",
                        "Rok budowy:",
                        min = 1900,
                        max = 2015,
                        value = c(1950, 2000)),
                selectInput("column", "Wybierz Dane:",
                        c("Cena" = "price", 
                          "Powierzchnia Mieszkania" = "sqft_living",
                          "Powierzchnia Gruntu" = "sqft_lot")),
                tableOutput("tableAvr")
                ),
        
        
            mainPanel(
                plotlyOutput("distPlot")
            )
        )
    ),
    
    fluidRow(
        sidebarLayout(
            sidebarPanel(
                textOutput("opis2"),
                sliderInput("stan",
                            "Stan:",
                            min = 1,
                            max = 5,
                            value = c(2, 3)),
                checkboxGroupInput('floors', 
                                   "Liczba Piêter:",
                                   c(1, 1.5, 2, 2.5, 3, 3.5)),
                tableOutput("tableNum")
                
            ),
            
            mainPanel(
                plotlyOutput("plot2")
            )
        )
    )
    
)


server <- function(input, output) {
    
    df <- read.csv('house_data.csv')
    

    output$distPlot <- renderPlotly({
        
        t <- case_when(input$column == 'price' ~ "Cena",
                       input$column == 'sqft_living' ~ "Powierzchnia Mieszkania",
                       input$column == 'sqft_lot' ~ "Powierzchnia Gruntu")
        
        df %>% 
            filter(yr_built >= input$year[1], yr_built <= input$year[2]) %>% 
            mutate(condition = factor(condition)) %>% 
            rename(kolumna = input$column) %>% 
            plot_ly(x = ~condition,
                    y = ~kolumna,
                    type = "box",
                    line = list(color = "#6a3b80"),
                    marker = list(color = "#6a3b80"),
                    fillcolor = "#e4bff5") %>% 
            layout(title = paste(t, "- Rozk³ad Wed³ug Stanu Mieszkania"),
                   xaxis = list(title = "Stan Mieszkania"),
                   yaxis = list(title = t))
    
    })
    
    output$tableAvr <- renderTable({
        
        df %>% 
            filter(yr_built >= input$year[1], yr_built <= input$year[2]) %>% 
            rename(kolumna = input$column) %>% 
            group_by(condition) %>% 
            summarise(n = n(), sr = mean(kolumna, na.rm = TRUE)) %>% 
            rename(Stan = condition, Iloœæ = n, Œrednia = sr)
    })
    
    output$opis1 <- renderText({
        
        t <- case_when(input$column == 'price' ~ "cen",
                       input$column == 'sqft_living' ~ "powierzchni mieszkania",
                       input$column == 'sqft_lot' ~ "powierzchni gruntu")
            
        paste("Graf przedstawia rozk³ad", t, "dla nieruchomoœci wybudowanych w latach", input$year[1], "-",
              input$year[2], ". Nieruchomoœci s¹ pogrupowane wed³ug ich stanu.")
        
    })
    
    output$tableNum <- renderTable({
        df %>% 
            filter(floors %in% input$floors, condition >= input$stan[1], condition <= input$stan[2]) %>% 
            mutate(renovated = ifelse(yr_renovated != 0, "Tak", "Nie")) %>% 
            group_by(renovated) %>% 
            summarise(n = n()) %>% 
            rename(Wyremontowany = renovated, Iloœæ = n)
    })
    
    output$plot2 <- renderPlotly({
        tmp <- df %>% 
            filter(yr_renovated != 0)
        
        yr_renovated <- min(tmp$yr_renovated):max(tmp$yr_renovated)
        tmp <- data.frame(yr_renovated)
        
        df %>% 
            filter(floors %in% input$floors, condition >= input$stan[1], condition <= input$stan[2], yr_renovated != 0) %>% 
            group_by(yr_renovated) %>% 
            summarise(n = n()) %>% 
            right_join(tmp) %>% 
            mutate(n = ifelse(is.na(n), 0, n)) %>% 
            arrange(yr_renovated) %>% 
            plot_ly(x = ~yr_renovated,
                    y = ~n, 
                    type = 'scatter',
                    mode = 'lines+markers',
                    line = list(color = "#6a3b80"),
                    marker = list(color = "#6a3b80")) %>% 
            layout(title = "Liczba Mieszkañ Wyremontowanych w Danym Roku",
                   xaxis = list(title = "Rok"),
                   yaxis = list(title = "Liczba"))
    })
    
    output$opis2 <- renderText({
        paste("Graf przedstawia ile nieruchomoœci o stanie", input$stan[1], "-", input$stan[2], 
              "oraz", paste(input$floors, collapse = ", "), "piêtrach zosta³o wyremontowane w danym roku.")
    })
}

 
shinyApp(ui = ui, server = server)
