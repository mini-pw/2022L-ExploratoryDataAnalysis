library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

dane <- read.csv("house_data.csv")
dane <- dane %>%
  mutate(price_for_square_feet = price / sqft_living)

ui <- fluidPage(
  titlePanel("Dom Marzeń"),
  textOutput("text"),
  tags$head(tags$style("#text{color: black; font-size: 20px; }") ),
  textOutput("text1"),
  tags$head(tags$style("#text1{color: black; font-size: 20px; }") ),

  fluidRow(
    column(6 ,
           plotlyOutput('map'))
    ,
    
    column(
      6 ,
      sliderInput(
        'money' ,
        'Jaką kwotą dysponujesz?' ,
        min = 100000 ,
        max = max(dane$price) ,
        value = max(dane$price)  ,
        step = 100000 ,
        round = 0
      )
    ),
    column(2,
           checkboxInput("woda", "Widok na wodę", FALSE)),
    column(6,
    textOutput("text2")),
    tags$head(tags$style("#text2{color: black; font-size: 20px; }") ),
    column(6 ,
           plotlyOutput('Price'))
    ,
    column(6 ,
           textOutput("text3"))
    ,
    tags$head(tags$style("#text3{color: black; font-size: 20px; }") ),
    column(6 ,
           textOutput("text4"))
    ,
    tags$head(tags$style("#text4{color: black; font-size: 20px; }") ),
    column(6 ,
           plotlyOutput('Grade'))
    ,
    column(6 ,
           plotlyOutput('Condition')),

    column(6 ,
           textOutput("text5"))
    ,
    tags$head(tags$style("#text5{color: black; font-size: 20px; }") ),
    column(6 ,
           textOutput("text6"))
    ,
    tags$head(tags$style("#text6{color: black; font-size: 20px; }") ),
    column(6 ,
           plotlyOutput('Bathrooms')),
    column(6 ,
           plotlyOutput('Bedrooms')),
    column(12 ,
           plotlyOutput('table'))
    
    
  )
)


server <- function(input, output, session) {
  output$text <- renderText({ "Nasza aplikacja pomoże Ci wybrać twój wymarzony dom z Seattle." })
  output$text1 <- renderText({ "Najedz kursorem na mapę, aby rozwinąć menu. Korzystając z 'Box Select' zaznacz obszar, w którym chcesz mieszkasz. Skorzystaj z 'Autoscale' aby zresetować mapkę." })
  output$text2 <- renderText({ "Zaznacz na wykresie ile pięter chcesz mieć." })
  output$text3 <- renderText({ "Zaznacz na wykresie jakość konstrukcji, która Cię interesuje." })
  output$text4 <- renderText({ "Zaznacz na wykresie stan mieszkania, który Cię interesuje." })
  output$text5 <- renderText({ "Zaznacz na wykresie ile łazienek potrzebujesz." })
  output$text6 <- renderText({ "Zaznacz na wykresie ile sypialni potrzbujesz." })
  
    output$map <- renderPlotly({
    

    if (input$woda){
      dane<-dane %>% 
        filter(waterfront==1)
    }
    
    pen <- png::readPNG("C:/Users/bnano/Desktop/WielgoszMichal/mapa3.png")
    fig <-
      plot_ly(
        dane[dane$price <= input$money,],
        x = ~ long,
        y = ~ lat,
        source = 'map',
        type = 'scatter',
        mode = 'markers'
      )
    
    fig <- fig %>%
      layout(
        title = "Mapa Seattle",
        yaxis = list(title = "Szerokość geograficzna", range = list(47.138, 47.779)),
        xaxis = list(title = "Długość geograficzna", range = list(-122.545, -121.315)),
        images = list(
          source = raster2uri(as.raster(pen)),
          x = -122.545,
          y = 47.138,
          sizex = 122.545 - 121.315,
          sizey = 47.779 - 47.138,
          xref = "x",
          yref = "y",
          xanchor = "left",
          yanchor = "bottom",
          sizing = "stretch",
          layer = "below"
        )
      )
  })
  
  output$Price <- renderPlotly({
    if (input$woda){
      dane<-dane %>% 
        filter(waterfront==1)
    }
    fig <-
      plot_ly(
        dane[dane$price <= input$money,],
        x = ~ floors,
        y = ~ price_for_square_feet ,
        source = "Price" ,
        split = ~ floors ,
        hoverinfo = 'skip',
        type = 'violin'
      )
    fig <-
      fig %>% layout(
        title = "Wartość stopy kwadratowej",
        xaxis = list(title = 'Iość pięter'),
        yaxis = list(title = 'Cena za stopę kwadratową')
      )
    zaznacz1 <- event_data("plotly_selected", source = "map")$x
    zaznacz2 <- event_data("plotly_selected", source = "map")$x
    zaznacz3 <- event_data("plotly_selected", source = "map")$y
    zaznacz4 <- event_data("plotly_selected", source = "map")$y
    if (is.null(zaznacz1) ||
        is.null(zaznacz2) ||
        is.null(zaznacz3) || is.null(zaznacz4)) {
      return(fig)
    }
    else{
      if (input$woda){
        dane<-dane %>% 
          filter(waterfront==1)
      }
      df1 <- dane %>%
        filter(
          long <= max(zaznacz1),
          long >= min(zaznacz2),
          lat <= max(zaznacz3),
          lat >= min(zaznacz4)
        )
      
      fig <-
        plot_ly(
          df1[dane$price <= input$money,],
          x = ~ floors,
          y = ~ price_for_square_feet ,
          source = "Price" ,
          split = ~ floors ,
          hoverinfo = 'skip',
          type = 'violin'
        )
      fig <-
        fig %>% layout(
          title = "Wartość stopy kwadratowej",
          xaxis = list(title = 'Iość pięter'),
          yaxis = list(title = 'Cena za stopę kwadratową')
        )
    }
    
    
  })
  
  output$Grade <- renderPlotly({
    if (input$woda){
      dane<-dane %>% 
        filter(waterfront==1)
    }
    df2 <- dane %>%
      filter(price <= input$money) %>%
      group_by(grade) %>%
      summarise(Count = n())
    
    fig2 <- plot_ly(
      df2,
      x = ~ grade,
      y = ~ Count,
      source = "Grade",
      name = "Jakość konstrukcji i wykończenia",
      type = "bar"
    )
    fig2 <- fig2 %>% layout(
      title = "Jakość konstrukcji i wykończenia",
      xaxis = list(title = 'Jakoś'),
      yaxis = list(title = 'Ilość dostępnych domów')
    )
    klik <- event_data("plotly_click", source = "Price")$x
    zaznacz1 <- event_data("plotly_selected", source = "map")$x
    zaznacz2 <- event_data("plotly_selected", source = "map")$x
    zaznacz3 <- event_data("plotly_selected", source = "map")$y
    zaznacz4 <- event_data("plotly_selected", source = "map")$y
    if (is.null(klik) ||
        is.null(zaznacz1) ||
        is.null(zaznacz2) ||
        is.null(zaznacz3) || is.null(zaznacz4)) {
      return(fig2)
    }
    else{
      if (input$woda){
        dane<-dane %>% 
          filter(waterfront==1)
      }
      df2 <- dane %>%
        filter(
          price <= input$money,
          floors == klik[1],
          long <= max(zaznacz1),
          long >= min(zaznacz2),
          lat <= max(zaznacz3),
          lat >= min(zaznacz4)
        ) %>%
        group_by(grade) %>%
        summarise(Count = n())
      fig2 <- plot_ly(
        df2,
        x = ~ grade,
        y = ~ Count,
        source = "Grade",
        name = "Jakość konstrukcji i wykończenia",
        type = "bar"
      )
      fig2 <- fig2 %>% layout(
        title = "Jakość konstrukcji i wykończenia",
        xaxis = list(title = 'Jakoś'),
        yaxis = list(title = 'Ilość dostępnych domów')
      )
    }
  })
  output$Condition <- renderPlotly({
    if (input$woda){
      dane<-dane %>% 
        filter(waterfront==1)
    }
    df3 <- dane %>%
      filter(price <= input$money) %>%
      group_by(condition) %>%
      summarise(Count = n())
    
    fig3 <- plot_ly(
      df3,
      x = ~ condition,
      y = ~ Count,
      source = "Condition",
      name = "Stan mieszkania",
      type = "bar"
    )
    fig3 <- fig3 %>% layout(
      title = "Stan mieszkania",
      xaxis = list(title = 'Stan'),
      yaxis = list(title = 'Ilość dostępnych domów')
    )
    klik1 <- event_data("plotly_click", source = "Price")$x
    klik2 <- event_data("plotly_click", source = "Grade")$x
    zaznacz1 <- event_data("plotly_selected", source = "map")$x
    zaznacz2 <- event_data("plotly_selected", source = "map")$x
    zaznacz3 <- event_data("plotly_selected", source = "map")$y
    zaznacz4 <- event_data("plotly_selected", source = "map")$y
    if (is.null(klik1) ||
        is.null(klik2) ||
        is.null(zaznacz1) ||
        is.null(zaznacz2) ||
        is.null(zaznacz3) || is.null(zaznacz4)) {
      return(fig3)
    }
    else{
      if (input$woda){
        dane<-dane %>% 
          filter(waterfront==1)
      }
      df3 <- dane %>%
        filter(
          price <= input$money,
          floors == klik1[1],
          grade == klik2[1],
          long <= max(zaznacz1),
          long >= min(zaznacz2),
          lat <= max(zaznacz3),
          lat >= min(zaznacz4)
        ) %>%
        group_by(condition) %>%
        summarise(Count = n())
      fig3 <- plot_ly(
        df3,
        x = ~ condition,
        y = ~ Count,
        source = "Condition",
        name = "Stan mieszkania",
        type = "bar"
      )
      fig3 <- fig3 %>% layout(
        title = "Stan mieszkania",
        xaxis = list(title = 'Stan'),
        yaxis = list(title = 'Ilość dostępnych domów')
      )
    }
    
  })
  output$Bathrooms <- renderPlotly({
    if (input$woda){
      dane<-dane %>% 
        filter(waterfront==1)
    }
    df4 <- dane %>%
      filter(price <= input$money) %>%
      mutate(bathrooms = round(bathrooms, digits = 0)) %>%
      group_by(bathrooms) %>%
      summarise(Count = n())
    
    fig4 <- plot_ly(
      df4,
      x = ~ bathrooms,
      y = ~ Count,
      source = "Bathrooms",
      name = "Ilość łazienek",
      type = "bar"
    )
    fig4 <- fig4 %>% layout(
      title = "Ilość łazienek",
      xaxis = list(title = 'Łazienki'),
      yaxis = list(title = 'Ilość dostępnych domów')
    )
    klik1 <- event_data("plotly_click", source = "Price")$x
    klik2 <- event_data("plotly_click", source = "Grade")$x
    klik3 <- event_data("plotly_click", source = "Condition")$x
    zaznacz1 <- event_data("plotly_selected", source = "map")$x
    zaznacz2 <- event_data("plotly_selected", source = "map")$x
    zaznacz3 <- event_data("plotly_selected", source = "map")$y
    zaznacz4 <- event_data("plotly_selected", source = "map")$y
    if (is.null(klik1) ||
        is.null(klik2) ||
        is.null(klik3) ||
        is.null(zaznacz1) ||
        is.null(zaznacz2) ||
        is.null(zaznacz3) || is.null(zaznacz4)) {
      return(fig4)
    }
    else{
      if (input$woda){
        dane<-dane %>% 
          filter(waterfront==1)
      }
      df4 <- dane %>%
        filter(
          price <= input$money,
          floors == klik1[1],
          grade == klik2[1],
          condition == klik3[1],
          long <= max(zaznacz1),
          long >= min(zaznacz2),
          lat <= max(zaznacz3),
          lat >= min(zaznacz4)
        ) %>%
        mutate(bathrooms = round(bathrooms, digits = 0)) %>%
        group_by(bathrooms) %>%
        summarise(Count = n())
      fig4 <- plot_ly(
        df4,
        x = ~ bathrooms,
        y = ~ Count,
        source = "Bathrooms",
        name = "Ilość łazienek",
        type = "bar"
      )
      fig4 <- fig4 %>% layout(
        title = "Ilość łazienek",
        xaxis = list(title = 'Łazienki'),
        yaxis = list(title = 'Ilość dostępnych domów')
      )
    }
    
  })
  output$Bedrooms <- renderPlotly({
    if (input$woda){
      dane<-dane %>% 
        filter(waterfront==1)
    }
    df5 <- dane %>%
      filter(price <= input$money) %>%
      mutate(bathrooms = round(bathrooms, digits = 0)) %>%
      group_by(bedrooms) %>%
      summarise(Count = n())
    
    fig5 <- plot_ly(
      df5,
      x = ~ bedrooms,
      y = ~ Count,
      source = "Bedrooms",
      name = "Ilość sypialni",
      type = "bar"
    )
    fig5 <- fig5 %>% layout(
      title = "Ilość sypialni",
      xaxis = list(title = 'Sypialnie'),
      yaxis = list(title = 'Ilość dostępnych domów')
    )
    klik1 <- event_data("plotly_click", source = "Price")$x
    klik2 <- event_data("plotly_click", source = "Grade")$x
    klik3 <- event_data("plotly_click", source = "Condition")$x
    klik4 <- event_data("plotly_click", source = "Bathrooms")$x
    zaznacz1 <- event_data("plotly_selected", source = "map")$x
    zaznacz2 <- event_data("plotly_selected", source = "map")$x
    zaznacz3 <- event_data("plotly_selected", source = "map")$y
    zaznacz4 <- event_data("plotly_selected", source = "map")$y
    if (is.null(klik1) ||
        is.null(klik2) ||
        is.null(klik3) || is.null(klik4) ||  is.null(zaznacz1) ||
        is.null(zaznacz2) ||
        is.null(zaznacz3) || is.null(zaznacz4)) {
      return(fig5)
    }
    else{
      if (input$woda){
        dane<-dane %>% 
          filter(waterfront==1)
      }
      df5 <- dane %>%
        mutate(bathrooms = round(bathrooms, digits = 0)) %>%
        filter(
          price <= input$money,
          floors == klik1[1],
          grade == klik2[1],
          condition == klik3[1],
          bathrooms == klik4[1],
          long <= max(zaznacz1),
          long >= min(zaznacz2),
          lat <= max(zaznacz3),
          lat >= min(zaznacz4)
        ) %>%
        group_by(bedrooms) %>%
        summarise(Count = n())
      fig5 <- plot_ly(
        df5,
        x = ~ bedrooms,
        y = ~ Count,
        source = "Bedrooms",
        name = "Ilość sypialni",
        type = "bar"
      )
      fig5 <- fig5 %>% layout(
        title = "Ilość sypialni",
        xaxis = list(title = 'Sypialnie'),
        yaxis = list(title = 'Ilość dostępnych domów')
      )
    }
    
  })
  output$table <- renderPlotly({
    klik1 <- event_data("plotly_click", source = "Price")$x
    klik2 <- event_data("plotly_click", source = "Grade")$x
    klik3 <- event_data("plotly_click", source = "Condition")$x
    klik4 <- event_data("plotly_click", source = "Bathrooms")$x
    klik5 <- event_data("plotly_click", source = "Bedrooms")$x
    zaznacz1 <- event_data("plotly_selected", source = "map")$x
    zaznacz2 <- event_data("plotly_selected", source = "map")$x
    zaznacz3 <- event_data("plotly_selected", source = "map")$y
    zaznacz4 <- event_data("plotly_selected", source = "map")$y
    if (is.null(klik1) ||
        is.null(klik2) ||
        is.null(klik3) ||
        is.null(klik4) || is.null(klik5) || is.null(zaznacz1) ||
        is.null(zaznacz2) ||
        is.null(zaznacz3) || is.null(zaznacz4)) {
      return()
    }
    else{
      if (input$woda){
        dane<-dane %>% 
          filter(waterfront==1)
      }
      df <- dane %>%
        select(
          id,
          date,
          price,
          bedrooms,
          bathrooms,
          sqft_living,
          sqft_lot,
          floors,
          waterfront,
          condition,
          grade,
          yr_built,
          yr_renovated,
          zipcode,
          lat,
          long
        ) %>%
        filter(price <= input$money) %>%
        mutate(bathrooms = round(bathrooms, digits = 0)) %>%
        filter(
          floors == klik1[1],
          grade == klik2[1],
          condition == klik3[1],
          bathrooms == klik4[1],
          bedrooms == klik5[1],
          long <= max(zaznacz1),
          long >= min(zaznacz2),
          lat <= max(zaznacz3),
          lat >= min(zaznacz4)
        )
      
      fig <- plot_ly(
        type = 'table',
        header = list(
          values = c("<b>Domy</b>", names(df)),
          align = c('left', rep('center', ncol(df))),
          line = list(width = 1, color = 'black'),
          fill = list(color = 'rgb(114, 201, 247)'),
          font = list(
            family = "Arial",
            size = 14,
            color = "white"
          )
        ),
        cells = list(
          values = rbind(rownames(df),
                         t(as.matrix(unname(
                           df
                         )))),
          align = c('left', rep('center', ncol(dane))),
          line = list(color = "black", width = 1),
          fill = list(color = c(
            'rgb(114, 201, 247)', 'rgba(167, 221, 250, 0.65)'
          )),
          font = list(
            family = "Arial",
            size = 12,
            color = c("black")
          )
        )
      )
    }
  })
  
  
  
}

shinyApp(ui = ui, server = server)
