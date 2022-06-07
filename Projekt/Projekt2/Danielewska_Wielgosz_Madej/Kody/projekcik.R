library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggmap)
library(shiny)
library(readxl)

kraje <- read.csv2("kraje.csv")
kraje <- kraje %>%
  select(polska.nazwa.skrócona, kod.alfa.3)
colnames(kraje)[1] <- "nazwa"

choroba <- read.csv("disease-burden-lead.csv")
colnames(choroba)[4] <- "DALY"
choroba <- choroba %>%
  filter(Entity %in% SmarterPoland::countries$country) %>%
  left_join(kraje, by = c("Code" = "kod.alfa.3"))

smierci <- read.csv("death-rate-lead-exposure.csv")
colnames(smierci)[4] <- "rate"
colnames(smierci)[3] <- "Rok"
smierci <- smierci %>%
  left_join(kraje, by = c("Code" = "kod.alfa.3"))

rozklady <- read.csv("monthly_all.csv")
colnames(rozklady)[2] <- "Rok"
ylim <- max(rozklady$mean)

df <-
  read.csv(
    "https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv"
  )
Dane <- read_excel("Data2.xlsx")
fin <- Dane %>% left_join(df, by = c("Country" = "COUNTRY"))

fin2 <- fin %>%
  filter(Country == 'European Union')

fin <- fin %>%
  filter(
    Country != 'European Union',
    Country != 'Holy See',
    Country != 'Nauru',
    Country != 'State of Palestine'
  )

fin$CODE[12] = "BHM"
fin$CODE[20] = "BOL"
fin$CODE[24] = "BRN"
fin$CODE[38] = "COG"
fin$CODE[41] = "CIV"
fin$CODE[46] = "PRK"
fin$CODE[47] = "COD"
fin$CODE[57] = "SWZ"
fin$CODE[62] = "GMB"
fin$CODE[68] = "GNB"
fin$CODE[76] = "IRN"
fin$CODE[87] = "LAO"
fin$CODE[105] = "FSM"
fin$CODE[111] = "MMR"
fin$CODE[119] = "MKD"
fin$CODE[131] = "KOR"
fin$CODE[132] = "MDA"
fin$CODE[134] = "RUS"
fin$CODE[159] = "SYR"
fin$CODE[173] = "TZA"
fin$CODE[174] = "USA"
fin$CODE[178] = "VEN"
fin$CODE[179] = "VNM"

fin <- fin %>% full_join(fin2)
fin$Country[183] = "United Kingdom"
fin$CODE[183] = "GBR"
fin <- fin %>% full_join(fin2)
fin$Country[184] = "Ireland"
fin$CODE[184] = "IRL"
fin <- fin %>% full_join(fin2)
fin$Country[185] = "Portugal"
fin$CODE[185] = "PRT"
fin <- fin %>% full_join(fin2)
fin$Country[186] = "Spain"
fin$CODE[186] = "ESP"
fin <- fin %>% full_join(fin2)
fin$Country[187] = "France"
fin$CODE[187] = "FRA"
fin <- fin %>% full_join(fin2)
fin$Country[188] = "Italy"
fin$CODE[188] = "ITA"
fin <- fin %>% full_join(fin2)
fin$Country[189] = "Germany"
fin$CODE[189] = "DEU"
fin <- fin %>% full_join(fin2)
fin$Country[190] = "Belgium"
fin$CODE[190] = "BEL"
fin <- fin %>% full_join(fin2)
fin$Country[191] = "Denmark"
fin$CODE[191] = "DNK"
fin <- fin %>% full_join(fin2)
fin$Country[192] = "Netherlands"
fin$CODE[192] = "NLD"
fin <- fin %>% full_join(fin2)
fin$Country[193] = "Luxembourg"
fin$CODE[193] = "LUX"

colnames(fin) <- gsub("19", "R19", colnames(fin))
colnames(fin) <- gsub("20", "R20", colnames(fin))
colnames(fin) <- gsub("R20R19", "R2019", colnames(fin))
colnames(fin) <- gsub("R20R20", "R2020", colnames(fin))

fin[is.na(fin)] <- 0

Angola <- read_excel("Angola.xlsx")
Azerbaijan <- read_excel("Azerbaijan.xlsx")
Afghanistan <- read_excel("Afghanistan.xlsx")
Albania <- read_excel("Albania.xlsx")
Algeria <- read_excel("Algeria.xlsx")
Andorra <- read_excel("Andorra.xlsx")
Antigua_and_Barbuda <- read_excel("Antigua and Barbuda.xlsx")
Argentina <- read_excel("Argentina.xlsx")
Armenia <- read_excel("Armenia.xlsx")
Australia <- read_excel("Australia.xlsx")
Austria <- read_excel("Austria.xlsx")
United_States_of_America <-
  read_excel("United States of America.xlsx")
Brazil <- read_excel("Brazil.xlsx")
Canada <- read_excel("Canada.xlsx")
China <- read_excel("China.xlsx")
Egypt <- read_excel("Egypt.xlsx")
Estonia <- read_excel("Estonia.xlsx")
Ethiopia <- read_excel("Ethiopia.xlsx")
Germany <- read_excel("European Union.xlsx")
Norway <- read_excel("Norway.xlsx")



ui <- dashboardPage(
  dashboardHeader(title = NULL),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Historia Midgley'a",
      tabName = "home",
      icon = icon("th")
    ),
    menuItem("Ołów", tabName = "olow", icon = icon("th")),
    menuItem("Freony", tabName = "freony", icon = icon("th"))
  )),
  dashboardBody(tabItems(
    # First tab content
    tabItem(
      tabName = "home",
      
      box(h2(
        "Naukowiec, którego wynalazki zabiły miliony ludzi i wywołały dwie globalne katastrofy"
      ),htmlOutput("historia")),
     box(h2("Źródła:"),
          htmlOutput("zrodla"))
    ),
    # Second tab content
    tabItem(
      tabName = "olow",
      fluidRow(column(
        12,
        box(plotlyOutput("rozkladPlot")),
        box(h3("Emisja ołowiu na przykładzie USA"),
            textOutput("opisolow"))
      )),
      
      fluidRow(column(
        12,
        box(
          h3("Wpływ nadmiernej emisji na zdrowie człowieka"),
          textOutput("opisolow2")
        ),
        box(
          title = "Wybierz kraje:",
          selectInput(
            "kraj",
            label = NULL,
            choices = unique(choroba$nazwa),
            multiple = TRUE,
            selected = c("Polska", "Chiny", "Mali")
          )
        )
        
        
      )),
      fluidRow(column(
        12,
        box(plotlyOutput("daly")),
        box(plotlyOutput("deathrate"))
      ))
    ),
    tabItem(tabName = "freony",
            fluidRow(column(
              12,
              
              box(selectInput(
                "rok",
                "Wybierz rok:",
                c(
                  "1986" = "R1986",
                  "1989" = "R1989",
                  "1990" = "R1990",
                  "1991" = "R1991",
                  "1992" = "R1992" ,
                  "1993" = "R1993",
                  "1994" = "R1994",
                  "1995" = "R1995",
                  "1996" = "R1996",
                  "1997" = "R1997",
                  "1998" = "R1998",
                  "1999" = "R1999",
                  "2000" = "R2000",
                  "2001" = "R2001",
                  "2002" = "R2002",
                  "2003" = "R2003",
                  "2004" = "R2004",
                  "2005" = "R2005",
                  "2006" = "R2006",
                  "2007" = "R2007",
                  "2008" = "R2008",
                  "2009" = "R2009",
                  "2010" = "R2010",
                  "2011" = "R2011",
                  "2012" = "R2012",
                  "2013" = "R2013",
                  "2014" = "R2014",
                  "2015" = "R2015",
                  "2016" = "R2016",
                  "2017" = "R2017",
                  "2018" = "R2018",
                  "2019" = "R2019",
                  "2020" = "R2020",
                  "2021" = "R2021"
                )
              )),
              box(h3("Emisja freonów na przestrzeni lat"),
                  textOutput("opisfreon"))
            )),
            fluidRow(column(
              12,
              box(plotlyOutput('mapa')),
              box(plotlyOutput('kraje'))
            )))
    
  ))
)

server <- function(input, output)
  ({
    output$historia <-
      renderText({
        paste0(
          "Thomas Midgley Jr. był bardzo uzdolnionym wynalazcą." ,
          " W trakcie jego życia przyznano mu ponad 100 patentów i ",
          "otrzymał wiele prestiżowych odznaczeń. Doprowadził ",
          "również do śmierci milionów ludzi na całym świecie oraz ",
          "dwóch globalnych katastrof, które miały olbrzymi wpływ na całą planetę.<br>",
          "Wynalazł m. in. mieszankę paliwową zawierającą związki ołowiu",
          ", która niwelowała zjawisko stukotania w silnikach samochodowych.",
          "Jego paliwo pod nazwą Ethyl stało się bardzo popularne, co doprowadziło",
          " do ogromnej emisji ołowiu na świecie. Szacuje się, że przez Ethyl i zawarty w nim ",
          "ołów przczynił się do śmierci i chorób milionów ludzi na świecie. ",
          "<br>Kolejnym niszczycielskim dla świata odkryciem Midgley'a było wynalezienie ",
          "Freonu-12, który stał się głównym czynnikiem chłodzącym stosowanym w lodówkach ",
          "i doprowadził m. in. do powstania dziury ozonowej. W naszym projekcie postanowiliśmy ",
          "zbadać jak przez lata wygląda wpływ tych wynalazków na naszą planetę."
        )
      })
    
    output$opisolow <-
      renderText({
        paste0(
          "Postanowiliśmy sprawdzić rozkład emisji ołowiu na poszczególne miesiące w
                                        latach 1990-2018 na przykładzie danych ze Stanów Zjednoczonych.
                                        Dane pochodzą z rządowego programu AQS (Air Quality System). Nie jesteśmy
                                        w stanie zauważyć żadnej zależności między poszczególnymi miesiącami,
                                        a wartością emisji. Dane jednak wyraźnie wskazują na znaczne obniżenie emisji
                                       ołowiu do atmosfery przez ostatnie 30 lat"
        )
      })
    
    output$opisolow2 <-
      renderText({
        paste0(
          "Większa emisja ołowiu spowodowana rozwojem działalności człowieka
                                        ma złe skutki na zdrowie ludzi, które możemy zauważyć na poniższych wykresach.
                                        Wykres liniowy przedstawia jak zmienia się na przestrzeni lat liczba tzw. DALY,
                                        czyli lat, w których człowiek obciążony jest chorobą wywołaną nadmierną emisją ołowiu.
                                        W wielu państwach możemy zauważyć tendecję spadkową, ale np. w Chinach liczba
                                        DALY nie maleje, a nawet przez ostatnie 30 lat wzrosła. Mapa natomiast przedstawia wartość
                                        wskaźnika śmierci spowodowane wpływem ołowiu w przeliczeniu na 100 tys. osób. Możemy
                                        zauważyć także tendencję spadkową. Tendencje spadkowe w tych obszarach są
                                        wynikiem zakazu stosowania ołowiu w poszczególnych państwach i jednocześnie spadkiem jego emisji."
        )
      })
    
    output$opisfreon <- renderText({
      paste0(
        "Niepalność freonów wpłynęła na duże zainteresowanie ich użyciem. 
        Jednak ich negatywny wpływ na środowisko skłonił kraje na całym świecie
        do zaprzestania ich użycia.  Wykres mapa przedstawia sumaryczne użycie freonów na całym 
        świecie na przestrzeni lat. Kliknięcie na wybrany kraj pozwoli nam zaobserwować użycie 
        freonów w wybranym kraju z podziałem na konkretne związki. Na wykresach możemy zaobserwować 
        wyraźny spadek ich wykorzystania, np. w Australii. W ostatnich latach zużycie freonów spadło 
        prawie do zera na całym świecie."
      )
    })
    
    output$zrodla <- renderText({
      paste0(
        "- https://pl.wikipedia.org/wiki/ISO_3166-1 <br>
        - https://pl.wikipedia.org/wiki/Thomas_Midgley <br>
             - https://ourworldindata.org/search?q=lead <br>
             - https://youtu.be/IV3dnLzthDA <br>
             - https://aqs.epa.gov/aqsweb/airdata/download_files.html <br>
             - https://ozone.unep.org/countries/data-table?report_type=0&output_type=
             odp-CO2e-tonnes&party_grouping=total&period_start=1986&
             period_end=2021&group_by=group&op=
             GENERATE+REPORT&form_id=ozone_country_data_form__report_table_form"
      )
    })
    
    output$daly <- renderPlotly({
      choroba %>%
        filter(nazwa %in% input$kraj) %>%
        plot_ly(
          type = "scatter",
          x =  ~ Year,
          y = ~ DALY,
          mode = "lines+markers",
          color = ~ nazwa,
          hovertemplate = "Rok %{x}<br> Lata stracone niepełnosprawnością z powodu wpływu ołowiu:%{y}"
        ) %>%
        layout(
          title = "Obciążenie chorobą spowodowane <br>wpływem ołowiu na człowieka",
          xaxis = list(title = "Rok"),
          yaxis = list(title = "Liczba DALY")
        )
    })
    
    output$deathrate <- renderPlotly({
      m <- list(colorbar = list(title = "Wskaźnik śmierci"))
      smierci <- smierci %>%
        filter(nazwa %in% input$kraj)
      plot_geo(smierci, locationmode = 'country names', frame = ~ Rok) %>%
        add_trace(
          locations = ~ Entity,
          z = ~ rate,
          color = ~ rate,
          colors = c("lightblue", "red"),
          zmin = 0,
          zmax = max(smierci$rate),
          text = ~ paste("Kraj: ", nazwa, "<br> Wskaźnik śmierci:", rate),
          hoverinfo = 'text',
          colorbar = list(title = "Wskaźnik <br>śmierci",
                          thickness = 10)
        ) %>%
        layout(title = "Wskaźnik śmierci spowodowanej wpływem ołowiu <br>na 100 tys. osób")
    })
    
    output$rozkladPlot <- renderPlotly({
      rozklady %>%
        plot_ly(
          y = ~ mean,
          x = ~ MONTH,
          type = "bar",
          frame = ~ Rok,
          hovertemplate = paste0("Wartość: ", round(rozklady$mean, 4), "<extra></extra>")
        ) %>%
        layout(
          yaxis = list(title = "Średnia emisja ołowiu(Ug/m^3)",
                       range = c(0, ylim)),
          xaxis = list(
            title = "Miesiąc",
            ticktext = list(
              "Styczeń",
              "Luty",
              "Marzec",
              "Kwiecień",
              "Maj",
              "Czerwiec",
              "Lipiec",
              "Sierpień",
              "Wrzesień",
              "Październik",
              "Listopad",
              "Grudzień"
            ),
            tickvals = 1:12,
            tickmode = "array"
          ),
          title = paste0("Rozkład emisji ołowiu w USA")
        )
      
    })
    output$kraje <- renderPlotly({
      if (is.null(event_data("plotly_click", source = "mapa")$pointNumber)) {
        df <- reactive({
          x <-
            get(fin$Country[1 == row.names(fin)])
        })
      }
      else{
        df <- reactive({
          x <-
            get(fin$Country[row.names(fin)[event_data("plotly_click", source = "mapa")$pointNumber + 1] == row.names(fin)])
        })
      }
      
      dane <- df()
      names(dane) <- gsub("/", "_", names(dane))
      
      fig <-
        plot_ly(
          dane,
          x = ~ Year,
          y = ~ A_I,
          name = 'Chlorofluoro-<br>carbons (CFCs)',
          type = 'scatter',
          mode = 'lines+markers'
        )
      fig <-
        fig %>% add_trace(y = ~ A_II,
                          name = 'Halons',
                          mode = 'lines+markers')
      fig <-
        fig %>% add_trace(y = ~ B_I,
                          name = 'Other Fully <br>Halogenated CFCs',
                          mode = 'lines+markers')
      fig <-
        fig %>% add_trace(y = ~ B_II,
                          name = 'Carbon Tetra-<br>chloride (CTC)',
                          mode = 'lines+markers')
      fig <-
        fig %>% add_trace(y = ~ B_III,
                          name = 'Methyl Chlo-<br>roform (TCA)',
                          mode = 'lines+markers')
      fig <-
        fig %>% add_trace(y = ~ C_I,
                          name = 'Hydrochloro-<br>fluorocarbons (HCFCs)',
                          mode = 'lines+markers')
      fig <-
        fig %>% add_trace(y = ~ C_II,
                          name = 'Hydrobromo-<br>fluorocarbons (HBFCs)',
                          mode = 'lines+markers')
      fig <-
        fig %>% add_trace(y = ~ C_III,
                          name = 'Bromochloro-<br>methane (BCM)',
                          mode = 'lines+markers')
      fig <-
        fig %>% add_trace(y = ~ E_I,
                          name = 'Methyl Bromide (MB)',
                          mode = 'lines+markers')
      fig <-
        fig %>% add_trace(y = ~ F,
                          name = 'Hydrofluoro-<br>carbons (HFCs)',
                          mode = 'lines+markers')
      fig <-
        fig %>% layout(
          title = fin$Country[row.names(fin)[event_data("plotly_click", source = "mapa")$pointNumber + 1] == row.names(fin)],
          xaxis = list(title = "Rok"),
          yaxis = list (title = "Zużycie substancji kontrolowanych w tonach ODP")
        )
    })
    
    output$mapa <- renderPlotly({
      fin2 <- fin %>%
        select(Country, input$rok , CODE) %>%
        rename(rok = input$rok)
      
      fig <-
        plot_ly(
          fin2,
          type = 'choropleth',
          locations = fin2$CODE,
          z = fin2$rok,
          text = fin2$Country,
          colorscale = "Blues",
          source = "mapa"
        ) %>%
        layout(title = "Zużycie substancji kontrolowanych w tonach ODP")
      
    })
  })

shinyApp(ui, server)
