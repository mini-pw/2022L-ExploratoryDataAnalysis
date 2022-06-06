library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringi)
library(tidyr)
library(shiny)
library(forcats)
library(stringr)
library(shinythemes)
# Define UI for application that draws a histogram
ui <- shinyUI(
  navbarPage("Climate change denial",
             theme = shinytheme("sandstone"),
             tabPanel(
               "Global",
               mainPanel(plotlyOutput("mapPlot"),
                         dataTableOutput("CRI")),
                         sidebarPanel(
                           h3("Map of temperatures around the world"),
                         em("This map show us how temperatures have been changing over the years throughout the globe. 
                         The color red means that it got hotter than last year in that particular country, 
                         blue - it is colder than in last year and white means that temperature is stable.
                            Lets focus on the last few years. We can notice that map is dominated by red color which proves that global warming is a real issue."),
                         h3("CRI"),
                         em("The Global Climate Risk Index indicates a level of exposure and vulnerability to extreme weather events,
                            which countries should understand as warnings in order to be prepared for more frequent and/or more severe events in the future." ))
               
             ),
             tabPanel(
               "India",
               mainPanel(plotOutput("india1Plot"),
                         plotOutput("india2Plot")),
               sidebarPanel(
                 h3("Heatwaves in India"),
                 tableOutput("indiaTable"),
                 em("The first barplot show us the number of heatwaves that occured in the last 12 years. At that time the amount of heatwaves has increased 13x in comparison to
                    the turn of the 20th and 21st century. For that reason, we can notice that the number of human deaths is also increasing."),
                 p(),
               img(src="mega_slonisko.png",height = 460, width = 560))
               ),
             
             tabPanel(
               "Japan",
               
             sidebarLayout(
              sidebarPanel(
                tableOutput("japanTable"),
                checkboxInput('deadormissing',
                              'Dead or missing',
                              FALSE),
                h3("Japanes typhoons"),
                em("As we can see, the amount of typhoons during recent years and their respective length hasn't changed much over time. 
                However we can see one positive change, which is progress in typhoon predicting and quick response systems in Japan,
                   that results in a quite noticable decrease in numbers of injuries and deaths caused by typhoons in the 21st century."),
                p(),
                img(src="tsunami.png",height = 500, width = 571)),
              
               mainPanel(
                         plotOutput("japan1Plot"),
                         plotOutput("japan2Plot"),
                         ))
             ),
             tabPanel( 
               "Dominicana",
               mainPanel(plotlyOutput("dominikanaPlot"),
                         h3('Coast of Dominicana now and how it is projected to look in 30 years',align = "center"),
                         img(src="dominicana_kiedys.png", align = "left",height = 480, width = 540),
                         img(src="dominicana_za_30.png", align = "right",height = 480, width = 540)),
               sidebarPanel(
                 h3("Dominican Coastline"),
                 em("Sadly, the situation regarding Dominican Republic's coastline looks pretty dire in recent predictions of sea level rise for XXI century.
                  The rise will have decupled by the end of 90's compared to now."),
                 h3("RCP"),
                 em("A Representative Concentration Pathway (RCP) is a greenhouse gas concentration (not emissions) trajectory adopted by the IPCC (Intergovernmental Panel on Climate Change).")
               )),
             tabPanel(
               "About",
               mainPanel(img(src="JEDREK.png",height = 500, width = 600),
                         img(src="MIECIU.jpg",height = 500, width = 600),
                         img(src="ROTER.png",align = 'center',height = 600, width = 600),
                         img(src="PLANET_B.png",height = 600, width = 600)),
               sidebarPanel(h3("Why this topic matters"),
                            h4("Authors: Jakub Mieczkowski, Roter Rafał, Jędrzej Ruciński"),
                            em("Climate change is a topic which, even though we hear about it a lot, it is often not being treated seriously.
                               In this project we wanted to show, based on raw data gathered from many sources, that the global mean temperature is indeed increasing. 
                               Apart from this, we also present some real life examples of particular countries experiencing various anomalies and tragedies due to global warming."),
                            p(),
                            img(src="logo1.png",height = 100, width = 300))
             )
             
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mapPlot <- renderPlotly({
      Temperature_change<- read_csv("Environment_Temperature_change_E_All_Data_NOFLAG.csv")
      kodp <- read_csv("2014_world_gdp_with_codes.csv")
      
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Bahamas, The", "Bahamas")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Bolivia", "Bolivia (Plurinational State of)")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Congo, Democratic Republic of the", "Democratic Republic of the Congo")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Congo, Republic of the", "Congo")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Cote d'Ivoire", "C???te d'Ivoire")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Czech Republic", "Czechia")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Korea, North", "Democratic People's Republic of Korea")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Gambia, The", "Gambia")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Iran", "Iran (Islamic Republic of)")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Laos", "Lao People's Democratic Republic")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Micronesia, Federated States of", "Micronesia (Federated States of)")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Macedonia", "North Macedonia")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Korea, South", "Republic of Korea")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Moldova", "Republic of Moldova")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Russia", "Russian Federation")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Syria", "Syrian Arab Republic")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Tanzania", "United Republic of Tanzania")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"United States", "United States of America")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Virgin Islands", "United States Virgin Islands")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Venezuela", "Venezuela (Bolivarian Republic of)")
      kodp[ ,1] <- stri_replace_all_regex(kodp$COUNTRY,"Vietnam", "Viet Nam")
      
      
      
      Temp_ch <- Temperature_change %>% 
        filter(Element == "Temperature change", Months == "January")
      Temp_ch <- Temp_ch %>% 
        left_join(kodp,by = c("Area" = "COUNTRY"))
      Temp_ch <- Temp_ch %>% 
        filter(CODE != 'NA')
      
      colnames(Temp_ch)<-c( "Area Code"  ,    "Area"     ,      "Months Code"  ,  "Months"   ,      "Element Code" ,  "Element",       
                     "Unit"       ,   "1961"    ,      "1962"        ,  "1963"    ,      "1964"        ,  "1965",         
                     "1966"      ,    "1967"    ,      "1968"        ,  "1969"    ,      "1970"        ,  "1971",         
                     "1972"      ,    "1973"    ,      "1974"        ,  "1975"    ,      "1976"        ,  "1977",         
                     "1978"      ,    "1979"    ,      "1980"        ,  "1981"    ,      "1982"        ,  "1983",         
                     "1984"      ,    "1985"    ,      "1986"        ,  "1987"    ,      "1988"        ,  "1989",         
                     "1990"      ,    "1991"    ,      "1992"        ,  "1993"    ,      "1994"        ,  "1995",         
                     "1996"      ,    "1997"    ,      "1998"        ,  "1999"    ,      "2000"        ,  "2001",         
                     "2002"      ,    "2003"    ,      "2004"        ,  "2005"    ,      "2006"        ,  "2007",         
                     "2008"      ,    "2009"    ,      "2010"        ,  "2011"    ,      "2012"        ,  "2013",         
                     "2014"      ,    "2015"    ,      "2016"        ,  "2017"    ,      "2018"        ,  "2019",         
                     "GDP (BILLIONS)", "CODE")
      
      Temp_waska <- Temp_ch %>% 
        select(-`Area Code`, -`Months Code`, -Months, -`Element Code`, -Element, -Unit,- `GDP (BILLIONS)`, -CODE) %>% 
        gather(Year, Temperature, -Area) %>% 
        left_join(Temp_ch, by = "Area")
      
      l <- list(color = toRGB("#d1d1d1"), width = 0.5)
      g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'equirectangular'),
        resolution = '100',
        showcountries = TRUE,
        countrycolor = '#d1d1d1',
        showocean = TRUE,
        oceancolor = '#c9d2e0',
        showlakes = TRUE,
        lakecolor = '#99c0db')
      
      p <- plot_geo(Temp_waska) %>%
        add_trace(z = ~Temperature , color = ~Temperature, colors = c('Blue','White','Red'),
                  text = ~Area, frame = ~Year, locations = ~CODE, marker = list(line = l)) %>%
        layout(title = 'Global yearly temperature change', geo = g) %>% 
        colorbar(limits = c(-8,8))
      p
    })
    
    output$CRI <- renderDataTable({
      climate_risk <- read_csv("climate-risk-index-1.csv")
      climate_risk %>% 
        select(rw_country_code,country,cri_rank,cri_score)
    })
    
    output$india1Plot <- renderPlot({nr_heatwaves <- data.frame(
      heat = c(279,	40,	201,	100,	144,	86,	150,	123	,90,	174,	42,	36),
      year = c('2010',	'2011',	'2012',	'2013',	'2014',	'2015',	'2016',	'2017',	'2018',	'2019',	'2020',	'2021'
      )
    ) 
    nr_heatwaves %>% 
      ggplot(aes(x = forcats::fct_inorder(factor(year)), y = heat)) + geom_bar(stat = 'identity', color = 'aquamarine2', fill = 'aquamarine2') + 
      geom_text(aes(label = heat), position = position_dodge(width = 0.9),vjust = -0.35) +
      theme(axis.text.x = element_text(size = 8)) +
      labs(x = 'Year', y = 'Number of heatwaves', title = 'Number of heatwaves over time')
    })
    output$india2Plot <- renderPlot({
      nr_heatwaves <- data.frame(
        heat = c(279,	40,	201,	100,	144,	86,	150,	123	,90,	174,	42,	36),
        year = c('2010',	'2011',	'2012',	'2013',	'2014',	'2015',	'2016',	'2017',	'2018',	'2019',	'2020',	'2021'
        )
      ) 
      nr_deaths <- data.frame(
        year = c('1970-79',	'1980-89',	'1990-99',	'2000-2009'	,'2010-2019'),
        deaths = c(2488,1505,2916,4056,6496)
      )
      nr_deaths %>% 
        ggplot(aes(x = year, y = deaths)) + geom_bar(stat = 'identity', color = '#FF3F64', fill = '#FF3F64') +
        geom_text(aes(label = deaths), position = position_dodge(width = 0.9), vjust = -0.5) +
        labs(x = 'Years', y = 'Number of deaths', title = 'Number of deaths due to heatwaves over time')
    })
    output$indiaTable <- renderTable({
      india_10_years <- data.frame(year = c('1970-79',	'1980-89',	'1990-99',	'2000-09','2010-19'),
                                   "heat waves" = c("100", "93", "90", "94","1387"))
    })
    output$japanTable <- renderTable({
      typhoon_jp_database <- read.csv2("typhoon_jp_database.csv")
      typhoon_dead <- read.csv2("typhoon_dead.csv")
      nr_in_dec <- typhoon_jp_database %>% 
        mutate(dyr = `Birth.YMDHM..UTC.`%/%10 * 10) %>% 
        group_by(dyr) %>% 
        summarise(occurences = n()) %>% 
        slice(-8) %>% 
        mutate(decade = c('1950-59','1960-69','1970-79','1980-89','1990-99','2000-2009','2010-2019')) %>% 
        select(decade, occurences)
    })
    output$japan1Plot <- renderPlot({
      typhoon_jp_database <- read.csv2("typhoon_jp_database.csv")
      typhoon_dead <- read.csv2("typhoon_dead.csv")
      mean_len_dec <- typhoon_jp_database %>% 
      mutate(dyr = `Birth.YMDHM..UTC.`%/%5 * 5) %>% 
      group_by(dyr) %>% 
      summarise(mean_length = round(mean(Length))) %>% 
      slice(-15) %>% 
      mutate(time = c('1950-54','1955-59','1960-64','1965-69','1970-74','1975-79','1980-84',
                      '1985-89','1990-94','1995-1999','2000-2004','2005-2009','2010-2014','2015-2019')) %>% 
      ggplot(aes(x = time, y = mean_length)) + geom_bar(stat = 'identity', fill = 'rosybrown') +
      theme(axis.text.x = element_text(angle = 90,size = 8)) +
      labs(x = 'Time period', y = 'Average length', title = 'Average length of typhoons per 5 years')
    mean_len_dec})
    output$japan2Plot <- renderPlot({
      typhoon_jp_database <- read.csv2("typhoon_jp_database.csv")
      typhoon_dead <- read.csv2("typhoon_dead.csv")
      typhoon_dead <- typhoon_dead %>% 
        filter(`Dead.or.Missing` != "#N/A" & Injured != "--") %>% 
        mutate(yr5 = `Birth.YMDHM..UTC.` %/% 5 * 5)
      typhoon_dead$`Dead.or.Missing` <- as.double(typhoon_dead$`Dead.or.Missing`)
      typhoon_dead$Injured <- as.double(typhoon_dead$Injured)
      x <- typhoon_dead %>% 
        group_by(yr5) %>% 
        summarise(AVG_Injured = round(mean(Injured)), AVG_dead = round(mean(`Dead.or.Missing`))) %>% 
        filter(yr5 != 2020) %>% 
        mutate(time = c('1950-54','1955-59','1960-64','1965-69','1970-74','1975-79','1980-84',
                        '1985-89','1990-94','1995-1999','2000-2004','2005-2009','2010-2014','2015-2019')) %>% 
        ggplot(aes(time)) + 
        geom_bar(aes(y = AVG_Injured, color= "Injured"),stat = 'identity',fill = "aquamarine2", alpha = 0.5)
        if (input$deadormissing == TRUE){
        x <- x +
         geom_point(aes(y = AVG_dead, color = "Dead or missing")) +
         geom_line(aes(y = AVG_dead, color = "Dead or missing" ,group = 1))
        }
        x <- x +
        scale_y_log10() + 
        theme(axis.text.x = element_text(angle = 90, size = 8)) +
        labs(x = 'Time period', y = 'Number of people', title = 'Average typhoon casualties per 5 years',legend)
        if(input$deadormissing == TRUE){
         x <- x + 
        scale_color_manual("",values = c("Injured" ="aquamarine2","Dead or missing" = "#FF3F64"))}else{x <- x +
          scale_color_manual("",values = c("Injured" ="aquamarine2"))}
        x <- x +
          scale_fill_manual(" ", values = "aquamarine2") +
          theme(legend.key=element_blank(),
                legend.title=element_blank(),
                legend.box="horizontal")
      
      
      
      x
    })
    output$dominikanaPlot <- renderPlotly({
      dominikana_wody <- read.csv2("dominikana_wody.csv")
      colnames(dominikana_wody) <- c("Year","RCP_2.6","RCP_2.6_low","RCP_2.6_high")
      dominikana_wody <- dominikana_wody %>% 
        mutate(Year = as.integer(Year))
      
      fig <- plot_ly(dominikana_wody,x = ~Year, y = ~RCP_2.6, name = 'RCP 2.6',type = 'scatter', mode = 'lines', line = list(color = "#FF3F64"))
      fig <- fig %>% 
        add_trace(y = ~RCP_2.6_low, name = 'RCP 2.6 10-90th Percentile Range (low)', mode = 'lines + markers', line = list(color = "aquamarine")) %>% 
        add_trace(y = ~RCP_2.6_high, name = 'RCP 2.6 10-90th Percentile Range (high)', mode = 'lines + markers',line = list(color = "fuchsia")) %>% 
        layout(title = 'Projected sea level rise of coastal Dominican Republic')
      fig
    })
}

shinyApp(ui = ui, server = server)
