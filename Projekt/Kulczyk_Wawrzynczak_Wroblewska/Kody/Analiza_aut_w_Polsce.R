library(shinythemes)
library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
library(bslib)
library(readxl)

# do wykresu Pauliny
samochody_liniowy <- read_excel("C:/Users/Ania/Documents/R/projektWDED2/Zeszyt_pd.xlsx")
samochody_liniowy$suma_w_roku <- samochody_liniowy$suma_w_roku/1000

#do wykresu Ani
zanieczyszczenie <- read_csv("C:/Users/Ania/Documents/R/projektWDED2/data1.csv", col_types = cols(VFN = col_skip(), Mp = col_skip(),  Man = col_skip(), 
                                                                                   Tan = col_skip(), T = col_skip(), 
                                                                                   Va = col_skip(), Ve = col_skip(), Ct = col_skip(), 
                                                                                   Cr = col_skip(), r = col_skip(), `m (kg)` = col_skip(), 
                                                                                   Mt = col_skip(), `Enedc (g/km)` = col_skip(), 
                                                                                   `W (mm)` = col_skip(), `At1 (mm)` = col_skip(), 
                                                                                   `At2 (mm)` = col_skip(), `ec (cm3)` = col_skip(), 
                                                                                   `ep (KW)` = col_skip(), `z (Wh/km)` = col_skip(), 
                                                                                   IT = col_skip(), `Ernedc (g/km)` = col_skip(), 
                                                                                   `Erwltp (g/km)` = col_skip(), De = col_skip(), 
                                                                                   Vf = col_skip(), Status = col_skip(), 
                                                                                   year = col_skip(), `Electric range (km)` = col_skip()))




df <- zanieczyszczenie %>% 
  na.omit() %>% 
  rename(Spaliny = `Ewltp (g/km)`) %>% 
  filter(Spaliny > 0) 
data_excel <- zanieczyszczenie
data_excel <- data_excel %>% 
  rename(Ewltp = `Ewltp (g/km)`)
fuel_kinds <- data_excel %>% distinct(Ft) %>% select(Ft)
Car_Brand <- data_excel %>% distinct(Mk) %>% select(Mk)
data_grouped <- data_excel %>% group_by(Mk, Cn, Ft) %>% summarize(mean_Ewltp = mean(na.omit(Ewltp)))


ui <- fluidPage(
  ### wykres Pauliny
  titlePanel("Zanieczyszczenia w Polsce produkowane przez samochody"),
  tabsetPanel(
 # h4("Poniższy wykres przedstawia liczbę zarejestrowanych aut w poszczególnych województwach w Polsce w wybranych latach."),
  
  tabPanel("Analiza rejestrowanych samochodów w Polsce",
           
  sliderInput("wybierz_rok", "Rok:", value = c(2015,2021), min = 2015, max = 2021, step =1)
,
  plotlyOutput("pointPlot"),

  textOutput('wniosekwykres1'),
  ),
  
  ### Wykres Ani
tabPanel("Analiza modeli aut pod względem spalania w Polsce",
  h4(textOutput('opiswykres2')),
  sidebarLayout(
    position = "left",
    sidebarPanel(width = 7,
  selectInput("paliwo", "Wybierz rodzaj napędu aut: ",
              choices = unique(as.character(df$Ft)),
              multiple = FALSE,
              selected = c((unique(as.character(df$Ft)))[1], (unique(as.character(df$Ft)))[2])
              
  ),
 
  
  uiOutput("secondSelection"),
  plotlyOutput("Plot1"),
  tableOutput("table")),
 
    mainPanel(width = 5,
    textOutput('wniosekwykres2'))
  
  ),
),
tabPanel("Analiza wpływu rodzaju paliwa na ilość produkowanych zanieczyszczeń",
  ##wykres Miry
  h4(textOutput("wykres3opis")),
  sidebarLayout(
    sidebarPanel(
      selectInput("Brand", "Marka", Car_Brand,
                  selected = 2),
      checkboxInput("Petrol", "PETROL", TRUE),
      checkboxInput("Diesel", "DIESEL", FALSE),
      checkboxInput("Petrolelectric", "PETROL/ELECTRIC", TRUE),
      checkboxInput("Dieselelectric", "DIESEL/ELECTRIC", FALSE),
      checkboxInput("LPG", "LPG", FALSE),
      checkboxInput("NG_Biometane", "NG_BIOMETANE", FALSE),
      checkboxInput("NG", "NG", FALSE),
      textOutput("wykres3wnioski")
    ),
    mainPanel(
      plotOutput("Plot")
    )
  )
)
))
draw_plot <- function(data, Brand, Fuels){
  data<-data[is.element(data$Ft, Fuels), ] %>% filter(Mk==Brand)
  p<-ggplot(data, aes(x=data$Cn, y=data$mean_Ewltp, fill=Ft)) + geom_col(position='dodge',width=0.5, alpha=0.5) + coord_flip() + labs(y="Spalanie [g/km]", x="Model") + scale_fill_discrete(name = "Rodzaj Paliwa")
  p
}


server <- function(input, output) {
  ##wykres Pauliny
  output$wniosekwykres1 <- renderText({paste0("Na podstawie wykresu widzimy, że województwem dominującym w liczbie rejestrowanych",
  " samochodów jest prawie co roku województwo mazowieckie, jedyny wyjątek stanowił rok 2018.",
  "  W województwie mazowieckim liczba ludności wynosi ponad 5mln osób. W 2021r zarejestrowano ponad 350 tys samochodów w tym województwie.",
  " Stąd wynika, że około 7 na 100 osób postanowiło w tym roku zarejestrować nowy samochód w województwie mazowieckim. ",
  " Przeanalizujmy tak samo województwo, które plasuje się na drugim miejscu pod względem rejestrowanych samochód w Polsce -",
  " województwo Wielkopolskie. W tym województwie mieszka około 3.5 mln mieszkańców.",
  " Liczba zarejestrowanych samochodów w 2021r w tym województwie wyniosła ponad 220tys. Stąd  około 6 na 100 osób podjęło",
  " decyzję o rejestracji nowego auta w tymże roku. W pozostałych województwach liczba osób rejestrujących nowe auta",
  " w tym roku wynosiła 4 na 100 osób. Patrząc na to, że w poprzednich latach statystyki rejestrowanych samochodów na 100 osób były podobne,",
  " można dojść do wniosku, że Polacy albo bardzo często zmieniają samochody, albo, że mają ich coraz więcej.",
  " Druga opcja niestety nie wydaje się dobrą wiadomością dla środowiska.",
  "
Porównajmy rozkład rejestrowanych samochodów w poszczególnych województwach z analizą zanieczyszczenia powietrza przeprowadzoną przez Airly.org w okresie 01.08.2020 - 01.08.2021.",
  " Z tej analizy wynika, że najbardziej zanieczyszczone województwa to śląskie, małopolskie oraz łódzkie.",
  " W klasyfikacji pod względem liczby zarejestrowanych samochodów w tym okresie województwo śląskie zajęło 3 miejsce, małopolskie 4,",
  " a łodzkie dopiero 7 miejsce. Wg statystyki Airly.org 3 najmniej zanieczyszcone województwa w tym okresie to warmińsko-mazurskie,",
  " pomorskie i zachodniopomorskie. Te województwa mają co prawda mniej zarejestrowanych samochodów niż top 3, jednak nie są na samym dole klasyfikacji.",
  " Stąd wynika, że nie tylko samochody wpływają na zanieczyszczenie powietrza, muszą istnieć także inne bardzo istotne czynniki.",
  " Jak wiadomo w województwie śląskim oraz małopolskim działa bardzo dużo przedsiębierstw, hurtowni oraz kopalni. Większość zanieczyszczeń",
  " prawdopodobnie pochodzi z tamtych źródeł. Dodatkowo należy podkreślić fakt, że na Polski rynek wpływa coraz więcej samochodów",
  " elektrycznych oraz hybrydowych, które emitują o wiele mniej spalin. Niestety Główny Urząd Statystyczny nie prowadzi",
  " statystyk zarejestrowanych samochodów z podziałem na rodzaj silnika, więc nie jesteśmy w stanie stwierdzić czy brak korelacji",
  " pomiędzy liczbą zarejestrowanych samochodów a jakością powietrza wynika z istotnego wpływu innych czynników a spaliny samochodowe ",
  "nie mają aż tak dużego udziału w tym procesie czy dzięki wprowadzaniu nowej technologii i coraz mniejszej liczbie samochodów z napędem na gaz, benzynę",
  " itp. ta korelacja nie występuje.
  "
  )})
  
  
  
  
  output$pointPlot <- renderPlotly({
    ggplotly(ggplot(samochody_liniowy[(samochody_liniowy$rok >= input$wybierz_rok[1] & samochody_liniowy$rok <= input$wybierz_rok[2]) 
                                      ,], 
                    aes(x = rok, y = suma_w_roku, group = wojewodztwo)) + 
               geom_line(aes(color = wojewodztwo))+
               geom_point(aes(color = wojewodztwo))+
               ggtitle("Liczba samochodów zarejestrowanych w Polsce") +
               xlab("Rok")+
               ylab("Liczba w tys."), tooltip = c("x", "y", "group")
    )
    
    
    
    
  })
  
  
  ##wykres Ani

  output$opiswykres2 <- renderText({paste0("Firmy samochodowe przyczyniają się do globalnego ocieplenia. ",
                              "Aby zmniejszyć emisję dwutlenku węgla, sprzedawane są auta wykorzystujące ",
                             "różne rodzaje paliwa. ",
                             "Poniższy wykres przedstawia zależności pomiędzy modelami",
                         " samochodów zarejestrowanych w Polsce w 2020 roku a średnią",
                         " ilością wyprodukowanych przez te auta spalin obliczoną w teście WLTP.")
  }) 
  
  output$secondSelection <- renderUI({
    selectInput("marka", "Wybierz markę samochodów:",
                choices = 
                  unique(df$Mk[df$Ft == input$paliwo])
                )

  })
  
  
  output$Plot1 <- renderPlotly({
    df %>% 
      filter( Ft %in% input$paliwo &
             Mk %in% input$marka
      ) %>%
      group_by(Cn) %>%
      summarize(srednia = mean(Spaliny)) %>% 
      plot_ly(
        x=~Cn,
        y=~srednia,
        type = "bar",
      hovertemplate = paste('Spaliny: %{y}<br>',
      'Model: %{x}<br><extra></extra>'),
      showlegend = FALSE
      )%>% 
      layout(xaxis = list(
                          title = paste("Modele aut od", ~input$marka)),
             yaxis =list(title = "Średnia ilość spalin w g/km"),
             title = "Średnia ilość spalin produkowana przez różne modele aut")
      
  })
  output$table <- renderTable({
 
    df %>% 
      filter( Ft %in% input$paliwo &
                Mk %in% input$marka
      ) %>%
      group_by(Cn) %>%
      summarize(srednia = mean(Spaliny)) %>% 
      arrange(srednia) %>% 
      rename("Model" = Cn,`Średnie spaliny w g/km` = srednia) %>% 
      head(3)
    
    
  })
  
  output$wniosekwykres2 <- renderText({paste0("Na podstawie wykresu można stwierdzić, że",
           " niewielu klientów firm samochodowych w Polsce interesuje się autami hybrydowymi z przekładnią",
       " beznynowo-elektryczną oraz pojazdami napędzanymi biometanem.",
      " Można również zaobserwować, że wśród samochodów, które",
       " działają za pomocą tego samego rodzaju paliwa, ilości produkowanych spalin są sobie bliske dla różnych modeli.",
      " Porównując marki samochodowe pod względem liczby modeli zasilanych danym rodzajem napędu można wywnioskować, że ",
      " wśród różnych rodzajów paliw modele o najmniejszej ilości spalin należą głównie do firm: Dacia, Mercedes-benz, Renault i Ford, co",
      " może oznaczać, że te firmy starają się tak ulepszać swoje auta, aby bez względu",
      " na rodzaj paliwa ich pojazdy zanieczyszczały środowisko w jak najmniejszym stopniu.","
      \n","\n","Decydując się na zakup samochodu oprócz sprawdzenia ilości produkowanych spalin przez ",
      " wybrany model warto również porównać emisję gazów cieplarnianych przez inne samochody z danej firmy",
      ", aby nie wspierać finansowo marek samochodowych, które nie próbują organiczyć spalin produkowanych przez wszystkie swoje auta.")
    
  })
   
  
  ##wykres Miry
   output$wykres3opis <- renderText({"Wykres znajdujący się poniżej ma na celu ukazanie zależności między modelem i rodzajem napędu samochodów, a ich spalaniem.
  "})
   output$wykres3wnioski<- renderText({paste0("Na wykresie jako pierwsze rzuca się w oczy, że najmniejsze spalanie mają samochody hybrydowe.",
                                              " Tak jasnych zależności nie można wysnuć na temat benzyny, diesla i LPG. Widać również,",
                                              " że duże samochody mają o wiele większe spalanie niż te bardziej kompaktowe. ",
                                              "Wynika to najpewniej z ich większej masy. Przeglądając wykresy można wywnioskować, że",
                                              " do najbardziej ekologicznego poruszania się autem najlepiej wybrać mały samochód hybrydowy (lub elektryczny)."
                                              
                                              
   )})
  
  
  output$Plot <- renderPlot({
    fuels <- c()
    if(input$Petrol) fuels<-append(fuels, 'PETROL')
    if(input$Diesel) fuels<-append(fuels, 'DIESEL')
    if(input$Petrolelectric) fuels<-append(fuels, 'PETROL/ELECTRIC')
    if(input$Dieselelectric) fuels<-append(fuels, 'DIESEL/ELECTRIC')
    if(input$LPG) fuels<-append(fuels, 'LPG')
    if(input$NG_Biometane) fuels<-append(fuels, 'NG-BIOMETHANE')
    if(input$NG) fuels<-append(fuels, 'NG')
    p<-draw_plot(data_grouped, input$Brand, fuels)
    p
    
  }
  , height=1000, width=1000
  )
}

shinyApp(ui , server)
