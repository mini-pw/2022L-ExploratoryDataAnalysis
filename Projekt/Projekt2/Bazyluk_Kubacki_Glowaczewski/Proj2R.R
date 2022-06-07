library(sf)
library(haven)
library(dplyr)
library(data.table)
library(stringr)
library(readxl)
library(shiny)
library(forcats)
library(tidyr)
library(ggplot2)
library(plotly)

# Zaczytanie ramek

gen <- read_xlsx("gene.xlsx")
inv <- read_xlsx("Inv_trends.xlsx")
dane <- read.csv("owid-co2-data.csv")
dane2 <- read.csv("per-capita-energy-use.csv")
df <- read.csv("ramka_do_pracy_projekt2.csv")

#Inwestycje

inv <- inv %>% 
  filter(Category == "by Region")

gen1 <- gen[,1:22] %>% 
  select(-c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012")) %>% 
  filter(`Country/area` %in% c("World","Africa","Middle East","Australia","New Zealand",
                             "Europe","Kazakhstan","Kyrgyzstan","Tajikistan","Turkmenistan",
                             "Uzbekistan","Asia","N America","Chile","Colombia",
                             "Costa Rica","S America","C America + Carib","Oceania")) %>% 
  select(-c("Indicator","Technology"))

rownames(gen1) <- gen1$`Country/area`
gen2 <- as.data.frame(t(gen1))
gen3 <- gen2[-1,]
gen3$Year <- c(2013,2014,2015,2016,2017,2018)

gen4 <- gen3 %>% 
  group_by(Year)
gen4$AfME <- as.numeric(gen4$Africa)+as.numeric(gen4$`Middle East`)
gen4$AuNZ <- as.numeric(gen4$Australia) + as.numeric(gen4$`New Zealand`)
gen4$CA <- as.numeric(gen4$Kazakhstan)+as.numeric(gen4$Kyrgyzstan)+
  as.numeric(gen4$Tajikistan) + as.numeric(gen4$Turkmenistan) + as.numeric(gen4$Uzbekistan)
gen4$Eu <- as.numeric(gen4$Europe)+as.numeric(gen4$CA)
gen4$Asia <- as.numeric(gen4$Asia)-as.numeric(gen4$CA)-as.numeric(gen4$`Middle East`) +
  as.numeric(gen4$Oceania)-as.numeric(gen4$AuNZ)
gen4$OecdSA <- as.numeric(gen4$`Costa Rica`)+as.numeric(gen4$Chile)+as.numeric(gen4$Colombia)
gen4$NaO <- as.numeric(gen4$`N America`)+as.numeric(gen4$OecdSA)
gen4$Sacarib <- as.numeric(gen4$`C America + Carib`)+as.numeric(gen4$`S America`)-as.numeric(gen4$OecdSA)

genfin <- select(gen4, c("Year","AfME","AuNZ","Eu","Asia","NaO","Sacarib"))

Inv2 <- inv %>% group_by(Year) %>% 
  mutate(AfME=sum(`Investment in USD billion`[Product %in% c(
    "Sub-Saharan Africa",
    "Middle East and North Africa")])) %>% 
  mutate(AuNZ=sum(`Investment in USD billion`[Product %in% c(
    "OECD Oceania")])) %>% 
  mutate(Eu=sum(`Investment in USD billion`[Product %in% c(
    "Western Europe",
    "Central Asia and Eastern Europe")])) %>% 
  mutate(As=sum(`Investment in USD billion`[Product %in% c(
    'OECD Asia',
    "South Asia",
    "East Asia and Pacific")])) %>% 
  mutate(NaO=sum(`Investment in USD billion`[Product %in% c(
    'OECD Americas')])) %>% 
  mutate(Sacarib=sum(`Investment in USD billion`[Product %in% c(
    'Latin America and the Caribbean')])) %>% 
  select(-c("Product","Category","Investment in USD billion")) %>% 
  distinct()

final <- as.data.frame(genfin$Year) %>%
  mutate(Year=genfin$Year) %>% 
  select(-c("genfin$Year")) %>% 
  mutate(`Africa + Middle East`=gen4$AfME/Inv2$AfME) %>% 
  mutate(`Australia + New Zealand`=gen4$AuNZ/Inv2$AuNZ) %>% 
  mutate(`Europe + Central Asia`=gen4$Eu/Inv2$Eu) %>% 
  mutate(`Asia and Pacific`=gen4$Asia/Inv2$As) %>% 
  mutate(`North America + OECD South America`=gen4$NaO/Inv2$NaO) %>% 
  mutate(`South America + Carribean`=gen4$OecdSA/Inv2$Sacarib)

final1 <- final %>% 
  pivot_longer(!Year, names_to="Region", values_to ="GWh/mld$")

#Koniec inwestycji

# Mapki

colnames(dane2)
dane %>%
  select(iso_code, country, year, co2, co2_per_capita, coal_co2, coal_co2_per_capita) %>%
  filter(year >= 2006 & year <= 2018) %>%
  filter(iso_code != "") %>%
  group_by(iso_code) %>%
  filter(!is.na(mean(coal_co2, na.rm = FALSE))) %>%
  inner_join(dane2,by =c("year"="Year" ,"country" = "Entity", "iso_code" = "Code")) %>%
  mutate("dirtiness" = 1000000*(co2_per_capita/`Energy per capita (kWh)`))-> dane3

g <- list(
  projection = list(
    type = 'orthographic'
  ),
  showland = TRUE,
  landcolor = toRGB("#e5ecf6")
)

# Koniec mapek

ui <- shinyUI(fluidPage(
  titlePanel("Impact of the renewable energy development and government investments"),
  h3("By Jakub Bazyluk, Jakub Kubacki & Ziemowit Glowaczewski"),
  fluidRow(
    column(6,
           h1("1."),
           h4("The plot represents the correlation between government spending in renewable energy sources,
              and power generation of said powerplants. A good indicator of any efficient spending is energy generated to
              money spent, so we've decided to include this proportion to our report."),
           sliderInput("Year", label = h3("Select year:"),min=min(final1$Year),
                       max=max(final1$Year),value=min(final1$Year)),
           plotlyOutput("pointPlot")
          )
  ),
  titlePanel("\n"),
    mainPanel(
      h1("2."),
      h4("The chart depicts how much money (in millions of dollars)
      is invested in the most active countries in the given year and energy type.
      The user might select their preferred energy type and the year. Interestingly,
      most of the benefactors of these donations are poorer countries,
         which shows great gesture and care."),
      plotlyOutput("Bplot"),
      sliderInput("year", "Select year:",
                  min=min(df$Year), max=max(df$Year), value=min(df$Year), step=1,
                  animate=animationOptions(1000)),
      selectInput("energia", "Energy type", sort(unique(df$Technology))),
      h1("3a."),
      h4("Coal plants are a great threat to the Earth's climate!
Following chart shows the total amount of CO2 each country emitted via coal burning each year,
starting from 2013, ending on 2018.
To keep the comparison fair, all of the figures are measured per capita. Nonetheless, many
countries emit tonnes of CO2, even per capita, some (countries marked orange and red) going
as far as emitting over 5 tonnes of CO2 per capita.
Feel free to rotate the globe to see the area you're interested in. "),
      plotlyOutput("map1"),
      h1("3b."),
      h4("
Following chart shows the total amount of CO2 each country emitted per capita each year,
starting from 2013, ending on 2018.
The purpose of the chart is to compliment the previous chart and allow one to see which
countries are the worst polluters. Note that the color scale is two times wider than
in the previous chart, so if a country has a similar color in both charts, about 50% 
of its CO2 pollution comes from the coal burning!"),
      plotlyOutput("map2"),
      h1("3c."),
      h4("As one can easily see, most of the orange and red on the previous two charts comes from
the developed nations. This is because the amount of CO2 emitted is directly linked to 
the amount of energy produced. If a country consumes a lot of energy, its CO2 emisions
will likely be higher. 
That is why we want to present one more chart to you.
This chart shows how much CO2 (in grams) a country emitted per an unit of energy 
(kilowatt-hour) consumed. This is the most accurate chart in terms of showing the
'greenness' of country's energy infrastructure."),
      plotlyOutput("map3"),
      h5("Plz don't laugh at my Windows not being activated :'("),
      
    )
  )
)
server <- shinyServer(function(input, output) {
  output$pointPlot <- renderPlotly({
    
    pom <- final1 %>% 
      filter(Year %in% input$Year) %>% 
      ggplot(aes(y=Region,x=`GWh/mld$`)) +
      theme_bw() +
      geom_col(fill="dark green") +
      labs(x="GWh/bln$") +
      scale_x_continuous(limits = c(0,30000))
  })
  output$Bplot <- renderPlotly({
    
    df %>% 
      filter(Year == input$year & Technology == input$energia) %>% 
      group_by(`Country/Area`) %>% 
      summarise(n = n()) %>% 
      top_n(8) %>% 
      pull(`Country/Area`) -> top6
    
    df2  <- df %>% 
      filter(`Country/Area` %in% top6) %>% 
      filter(Year == input$year & Technology == input$energia) %>% 
      mutate(Price = as.double(`Amount (2019 USD million)`)) %>% 
      group_by(`Country/Area`, Year) %>% 
      summarise(s = sum(Price, na.rm=T)) %>% 
      mutate(Country = `Country/Area`, Price = s)
    
    p <-plot_ly(data = df2, x = ~Country, y = ~Price, type = "bar")%>%
      layout(title = 'External investments in energy in mln $',
             yaxis=list(title="Funds"))
    p
    
  })
  
  output$map1 <- renderPlotly({
    fig1 <- plot_ly(
      type = 'choropleth',
      locations = dane3$iso_code,
      z = dane3$coal_co2_per_capita,
      zmin = 1,
      zmax = 8,
      hovertemplate = paste(dane3$country,
                            '<br>',
                            '<b>%{z}</b>t<extra></extra>'),
      showlegend = FALSE,
      colors = c("green","red"),
      frame = dane3$year)
    fig1 <- fig1 %>% layout(title = "CO2 emissions from coal energy per capita (in tonnes)",geo = g)
    fig1 <- fig1 %>% animation_opts(2000)
    fig1
  })
  
  output$map2 <- renderPlotly({
    fig2 <- plot_ly(
      type = 'choropleth',
      locations = dane3$iso_code,
      z = dane3$co2_per_capita,
      zmin = 2,
      zmax = 16,
      hovertemplate = paste(dane3$country,
                            '<br>',
                            '<b>%{z}</b>t<extra></extra>'),
      showlegend = FALSE,
      colors = c("green","red"),
      frame = dane3$year)
    fig2 <- fig2 %>% layout(title = "total CO2 emissions per capita (in tonnes)",geo = g)
    fig2 <- fig2 %>% animation_opts(2000)
    fig2
  })
  
  output$map3 <- renderPlotly({
    fig3 <- plot_ly(
      type = 'choropleth',
      locations = dane3$iso_code,
      z = dane3$dirtiness,
      zmin = 100,
      zmax = 400,
      hovertemplate = paste(dane3$country,
                            '<br>',
                            '<b>%{z}</b>g/kWh<extra></extra>'),
      showlegend = FALSE,
      colors = c("green","red"),
      frame = dane3$year)
    fig3 <- fig3 %>% layout(title = "CO2 emitted per kWh of energy used (in g/kWh)",geo = g)
    fig3 <- fig3 %>% animation_opts(2000)
    fig3
  })
  
})

shinyApp(ui, server)
