#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(dplyr)
library(tidyr)
library(thematic)
energy2 <- owid_energy_data
energy2 %>%  select(country, year,solar_energy_per_capita,wind_energy_per_capita,
                    oil_energy_per_capita, nuclear_energy_per_capita,
                    hydro_energy_per_capita,gas_energy_per_capita,
                    fossil_energy_per_capita,coal_cons_per_capita,
                    biofuel_cons_per_capita) %>% 
  filter(year>=1990) %>% 
  filter(!country %in% c('Europe','Africa','European Union (27)','World')) %>%
  na.omit() %>% rename(Biomass = biofuel_cons_per_capita,
                       Coal = coal_cons_per_capita,
                       Gas = gas_energy_per_capita,
                       Water = hydro_energy_per_capita,
                       Oil = oil_energy_per_capita,
                       Atomic = nuclear_energy_per_capita,
                       Solar = solar_energy_per_capita,
                       Wind = wind_energy_per_capita,
                       'Fossil fuels'= fossil_energy_per_capita
  ) -> tabene

my_theme <- bs_theme(bg = "#D3E4F8", fg = "#000000",
                     base_font = font_google("Comfortaa"))
thematic_shiny(font = "auto")
zmienne <- c('Biomass',
             'Wind',
             'Oil',
             'Atomic',
             'Water',
             'Gas',
             'Fossil fuels',
             'Coal'
)

data <- energy2 %>% 
  select("renewables_electricity", "fossil_electricity", "nuclear_electricity","country", "year",  
         "hydro_electricity", "solar_electricity", "wind_electricity",  
         "other_renewable_electricity", "coal_electricity",
         "oil_electricity", "gas_electricity"
  )

tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }"
)

ui <- fluidPage(
    theme = my_theme,
    titlePanel(h1("Sources of energy", align = 'center')),
   
    
    fluidRow(
        column(2,
                              selectInput(
                                inputId = "country",
                                label = "Choose a country:",
                                choices = c("All", unique(as.character(tabene$country))),
                                selected = "All"
                              ),
                              
                              selectInput(
                                inputId = "variable",
                                label = "Source of energy:",
                                choices = zmienne
                              )
                 ),
        column(7,
                 plotlyOutput("plot")),
        column(3,
        p("The following dashboard shows some facts about energy sources and consumption
        around the world. Feel free to choose a country of your interest. The plots illustrate
        energy consumption per capita and energy production with respect to different sources.",
        style = "font-size:19pt;"))
    ),
    fluidRow(
      column(5),
      column(4,
              selectInput("kraj",
                          h2("Choose a country", align = "center"),
                          unique(energy2$country),
                          selected = 'Poland'
                          )
                          ),
      column(3)
    ),
    fluidRow(
      
      column(2,
                 sliderInput("rokwyb",
                             "Choose a range of time:",
                             min = 1990,
                             max = 2020,
                             value = c(2000,2020),
                             sep = "")
                              
               ),
               
               # Show a plot of the generated distribution
      column(5,         
                 plotlyOutput("plot_2000")
               ),
      column(5,
             
               plotlyOutput("plot_2020")
      )
    ),
    fluidRow(
      
      column(6,
          
              plotlyOutput("histPlot")
               ) ,
      column(6,
               
               # Show a plot of the generated distribution
               
                plotlyOutput("stack")
               )
    ),
    fluidRow(
      column(9),
      column(3,
      p("Katarzyna Mamla, Jan Cichowlas, Grzegorz Pióro"))
    )
        )




server <- function(input, output, session) {
    
  filtered_countries <- reactive({
    if(input$country == "All"){
      tabene
    } else {
      tabene %>%
        filter(country == input$country)
    }
    })
    # output$tekstpodtytul <- renderText({
    #   "The following dashboard shows some facts about energy sources and consumption
    #   around the world. Feel free to choose a country of your interest. The plots illustrate
    #   energy consumption per capita and energy production with respect to different sources."
    # })
    output$plot_2000 <- renderPlotly({
      belgia_1990 <-data %>% 
        filter(year == input$rokwyb[1], country == input$kraj) %>% 
        select(-country, - year)
      
      belgia_1990 <- as.numeric(belgia_1990[1, ])
      wykres_1990 <- plot_ly(
        labels = c("renewables", "fossil", "nuclear", "hydro", "solar", "wind", "other",
                   "coal", "oil", "gas"),
        parents = c("", "", "", "renewables", "renewables", "renewables", "renewables",
                    "fossil", "fossil", "fossil"),
        values = belgia_1990, 
        type = "sunburst",
        branchvalues = "total"
      ) %>% 
        layout(title = paste0("Sources of electricity in ", input$kraj,  " in ", input$rokwyb[1]),
              font = list(family = 'Comfortaa'),
              plot_bgcolor = "rgba(0, 0, 0, 0)",
              paper_bgcolor = "rgba(0, 0, 0, 0)",
              fig_bgcolor = "rgba(0, 0, 0, 0)")
    })
    output$plot_2020 <- renderPlotly({
      belgia_1990 <-data %>% 
        filter(year == input$rokwyb[2], country == input$kraj) %>% 
        select(-country, - year)
      
      belgia_1990 <- as.numeric(belgia_1990[1, ])
      wykres_1990 <- plot_ly(
        labels = c("renewables", "fossil", "nuclear", "hydro", "solar", "wind", "other",
                   "coal", "oil", "gas"),
        parents = c("", "", "", "renewables", "renewables", "renewables", "renewables",
                    "fossil", "fossil", "fossil"),
        values = belgia_1990, 
        type = "sunburst",
        branchvalues = "total"
      ) %>% 
        layout(title = paste0("Sources of electricity in ", input$kraj,  " in ", input$rokwyb[2]),
               font = list(family = 'Comfortaa'),
               plot_bgcolor = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)",
               fig_bgcolor = "rgba(0, 0, 0, 0)")
    })
    output$plot <- renderPlotly({
      df <- filtered_countries()
      df$yy <- df[[input$variable]]
      
      plot<-plot_ly(df,
                    x = ~year,
                    y = ~yy,
                    mode = "lines",
                    type = "scatter",
                    color=~country,
                    text = paste0("Kraj: ", df$country),
                    hoverinfo = 'x+y+text') %>% 
        layout( title = paste0("Energy consumption per capita in", ' ',input$country),
                xaxis = list( title='Year'), 
                yaxis = list( title=input$variable),
                font = list(family = 'Comfortaa'),
                plot_bgcolor = "rgba(0, 0, 0, 0)",
                paper_bgcolor = "rgba(0, 0, 0, 0)",
                fig_bgcolor = "rgba(0, 0, 0, 0)"
        )
      plot 
    })
    
    output$histPlot <- renderPlotly({
      l <- energy2 %>% 
        select(country,year ,electricity_demand,electricity_generation) %>% 
        filter(!is.na(electricity_demand) & !is.na(electricity_generation)) %>% 
        filter(country == input$kraj) %>%
        mutate(dif = electricity_generation - electricity_demand) %>% 
        ggplot(aes(x = year, y = dif, label = country)) +
        geom_line(color = 'navyblue') +
        labs(title = paste0("Difference between energy generation and demand over time in ", input$kraj),
             y = 'Difference (in TWh)',
             x = 'Year')+
        theme_bw()
      ggplotly(l, tooltip = c('x','y','label')) %>% 
        layout(font = list(family = 'Comfortaa'),
               plot_bgcolor = "rgba(0, 0, 0, 0)",
               paper_bgcolor = "rgba(0, 0, 0, 0)",
               fig_bgcolor = "rgba(0, 0, 0, 0)")
    })

    output$stack <- renderPlotly({
      p <- energy2 %>% 
        filter(country == input$kraj) %>% 
        rename(Biomass = biofuel_share_elec,
               Coal = coal_share_elec,
               Gas = gas_share_elec,
               Water = hydro_share_elec,
               Oil = oil_share_elec,
               Atomic = nuclear_share_elec,
               Solar = solar_share_elec,
               Wind = wind_share_elec,
               Year = year) %>% 
        filter(Year > 1989) %>% select(country, Year, Coal,Biomass, Gas, 
                                      Water, Oil, Atomic, Solar, Wind) %>% 
        pivot_longer(!c(country,Year),names_to = 'Source', values_to = 'Share') %>% 
        ggplot(aes(x = Year, y = Share, fill = forcats::fct_inorder(Source), label = Source))+
        geom_area()+
        scale_fill_manual(values = c('#fbb4ae',
                                     '#b3cde3',
                                     '#ccebc5',
                                     '#decbe4',
                                     '#fed9a6',
                                     '#ffffcc',
                                     '#e5d8bd',
                                     '#fddaec'),
                          labels = c('Elektrownie weglowe','Biomasa','Elektrownie gazowe',
                                     'Elektrownie na rope naftowa','Elektrownie atomowe', 'Energia sloneczna',
                                     'Elektrownie wiatrowe','Elektrownie wodne'),
                          name = "Sources") +
        scale_x_continuous(expand = c(0,0))+
        scale_y_continuous(expand = c(0,0))+
        labs(title = paste0("Share of produced electricity in ", input$kraj, " from various sources in 1990-2022"),
              y = 'Share [in %]')
      s <- ggplotly(p, tooltip = c("x","y", "label"))
      s
    })
      
}


shinyApp(ui = ui, server = server)