library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel(
    'Bezsenność w Seattle (o cenach mieszkań w największym mieście Waszyngtonu)'
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4('Wykres ceny mieszkań w zależności od roku wybudowania'),
      selectInput(
        'woda',
        'Wybierz rodzaj widoku z posiadłości:',
        c(
          'Wszystkie mieszkania',
          'Widok na wodę',
          'Brak widoku na wodę',
          'Z widokiem + bez widoku'
        )
      ),
      
      checkboxInput('liniatrendu',
                    'Linia trendu',
                    FALSE),
      p(
        'Wykres przedstawia średnią cenę mieszkań dla danego roku budowy
 w zależności od tego czy dane mieszkanie jest położone nad wodą.'
      ),
      p(
        'W powyższym menu możemy wybrać jaki rodzaj posiadłości nas interesuje, a w ostatniej opcji możemy zobaczyć porównanie obu z nich.'
      ),
      br(),
      br(),
      br(),
      h4('Wykres ceny mieszkań w zależności od liczby sypialni'),
      selectInput('cond',
                  'Wybierz stan mieszkania:',
                  c(1:5)),
      p(
        'Ten wykres pokazuje rozkład cen mieszkań w zależności od liczby sypialni dla mieszkań posiadających od jednej sypialni do pięciu.'
      ),
      p(
        'Menu pozwala nam wybrać mieszkania o konkretnym stanie (skala od 1 do 5).'
      )
    ),
    mainPanel(plotOutput("distPlot"),
              plotOutput('boxplot'))
  )
)

server <- function(input, output) {
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(readr)
  
  output$distPlot <- renderPlot({
    house_data <- read_csv("house_data.csv")
    
    
    house_data %>%
      filter(yr_built >= 1990 & yr_built <= 2010) %>%
      group_by(yr_built) %>%
      summarise(srednia_cena = mean(price / 1000000))  %>%
      ggplot(aes(x = yr_built, y = srednia_cena)) +
      geom_col(color = 'gold3', fill = 'purple4') +
      labs(title = 'Cena mieszkań w zależności od roku wybudowania',
           x = 'Rok wybudowania', y = 'Średnia cena w milionach dolarów') -> p
    
    house_data %>%
      filter(yr_built >= 1990 & yr_built <= 2010) %>%
      group_by(yr_built, waterfront) %>%
      summarise(srednia_cena = mean(price / 1000000)) -> house_filtered_mean
    
    house_added <-
      data.frame(
        yr_built = c(1994, 1995, 1997, 2003, 2008, 2009,
                     2010),
        waterfront = 1,
        srednia_cena = 0
      )
    
    hd <- rbind(house_filtered_mean, house_added)
    
    hd %>%
      group_by(yr_built) %>%
      arrange(yr_built) %>%
      ggplot(aes(
        x = yr_built,
        y = srednia_cena,
        fill = factor(waterfront)
      )) +
      geom_col(color = 'gold3', position = 'dodge') +
      labs(title = 'Cena mieszkań w zależności od roku wybudowania',
           x = 'Rok wybudowania', y = 'Średnia cena w milionach dolarów') +
      scale_fill_manual(
        values = c('purple4', 'royalblue2'),
        name = 'Widok na wodę',
        labels = c(' - ', ' + '),
        guide = guide_legend(reverse = TRUE)
      ) -> nww
    
    hd %>%
      filter(waterfront == 1) %>%
      group_by(yr_built) %>%
      ggplot(aes(x = yr_built, y = srednia_cena)) +
      geom_col(color = 'gold3', fill = 'purple4') +
      labs(title = 'Cena mieszkań w zależności od roku wybudowania',
           x = 'Rok wybudowania', y = 'Średnia cena w milionach dolarów') -> w
    
    hd %>%
      filter(waterfront == 0) %>%
      group_by(yr_built) %>%
      ggplot(aes(x = yr_built, y = srednia_cena)) +
      geom_col(color = 'gold3', fill = 'purple4') +
      labs(title = 'Cena mieszkań w zależności od roku wybudowania'
           , x = 'Rok wybudowania', y = 'Średnia cena w milionach dolarów') -> nw
    
    
    
    (if (identical(input$woda, 'Wszystkie mieszkania')) {
      p
    }
      else if (identical(input$woda, 'Widok na wodę')) {
        w
      }
      else if (identical(input$woda, 'Brak widoku na wodę')) {
        nw
      }
      else {
        nww
      }) -> k
    if (input$liniatrendu) {
      k <- k + geom_smooth(se = FALSE, color = 'gold3')
    }
    k
    
  })
  output$boxplot <- renderPlot({
    house_data <- read_csv("house_data.csv")
    house_data %>%
      filter(bedrooms >= 1 & bedrooms <= 5) %>%
      filter(input$cond == condition) %>%
      ggplot(aes(x = price / 1000000, y = factor(bedrooms))) +
      coord_flip() +
      geom_boxplot(color = 'gold3', fill = 'purple4') +
      labs(title = 'Cena mieszkań w zależności od liczby sypialni',
           y = 'Liczba sypialni', x = 'Cena w milionach dolarów')
    
    
  })
  
}

shinyApp(ui = ui, server = server)