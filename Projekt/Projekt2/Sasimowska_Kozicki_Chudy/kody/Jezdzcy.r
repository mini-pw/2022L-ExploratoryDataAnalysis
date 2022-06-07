library(ggstatsplot)
require(maps)
library(ggrepel)
library(patchwork)
library(ggplot2)
library(dplyr)
library(SmarterPoland)
library(lubridate)
library(plotly)
library(thematic)
library(ncdf4)
library(raster)
library(rgdal) 
library(ggplot2)
library(chron)
library(lattice)
library(RColorBrewer)
library(mapdata)
library(rasterVis)
library(sp)
library(maps)
library(maptools)
library(shiny)
library(tidyverse)
library(tidyr)
library(rnaturalearth)

#death cause by natural disasters
death_cause <- read.csv('number-of-deaths-from-natural-disasters.csv')
d_c <- death_cause %>% 
    filter(Entity == 'World') %>%
    filter(Year > 1950) %>% 
    select(Year,Entity, deaths_drought, deaths_temperature, deaths_wildfire) %>% 
    arrange(Year) %>% 
    pivot_longer(
        cols = starts_with("deaths_"),
        names_to = "przyczyna_smierci",
        names_prefix = "deaths_",
        values_to = "lp_zgon",
        values_drop_na = TRUE) %>% 
    pivot_wider(names_from = Entity, values_from = lp_zgon)

#death causes by air pollution
air_pollution <- read.csv('share-deaths-air-pollution.csv')
country <- map_data("world")
air_pollution$Entity[air_pollution$Entity == 'Congo']<- 'Republic of Congo'
air_pollution$Entity[air_pollution$Entity == 'United States']<- 'USA'
air_pollution$Entity[air_pollution$Entity == 'Democratic Republic of Congo']<- 'Democratic Republic of the Congo'
air_pollution$Entity[air_pollution$Entity == 'United States']<- 'USA'
air_pollution$Entity[air_pollution$Entity == 'United Kingdom']<- 'UK'
air_pollution$Entity[air_pollution$Entity == "Cote d'Ivoire"]<- "Ivory Coast"
air_pollution<- air_pollution %>% 
    mutate(Percentage = Deaths...Cause..All.causes...Risk..Air.pollution...Sex..Both...Age..Age.standardized..Percent.) %>% 
    select(Entity, Percentage, Year)
plus<-country %>% 
    left_join(air_pollution, by = c("region" = "Entity"))


#death caused by air pollution (not map)
air <- read.csv('death-rates-from-air-pollution.csv')
air <- air %>%
    filter(Entity == 'World') %>% 
    pivot_longer(
        cols = starts_with("Deaths...Cause..All.causes...Risk.."),
        names_to = "przyczyna_smierci",
        names_prefix = "deaths_",
        values_to = "lp_zgon",
        values_drop_na = TRUE)
air <- air %>% 
    select(Year, lp_zgon, przyczyna_smierci) %>% 
    arrange(Year)
air$przyczyna_smierci[air$przyczyna_smierci == 'Deaths...Cause..All.causes...Risk..Household.air.pollution.from.solid.fuels...Sex..Both...Age..Age.standardized..Rate.'] <- 'Indor air pollution'
air$przyczyna_smierci[air$przyczyna_smierci == 'Deaths...Cause..All.causes...Risk..Ambient.particulate.matter.pollution...Sex..Both...Age..Age.standardized..Rate.'] <- 'Outdoor air pollution'
air$przyczyna_smierci[air$przyczyna_smierci == 'Deaths...Cause..All.causes...Risk..Air.pollution...Sex..Both...Age..Age.standardized..Rate.']<-'Total air pollution'
air$przyczyna_smierci[air$przyczyna_smierci == 'Deaths...Cause..All.causes...Risk..Ambient.ozone.pollution...Sex..Both...Age..Age.standardized..Rate.'] <- 'Outdoor ozone pollution'




mala <- read.csv("malaria-death-rates.csv")
malaria <- mala %>% 
    filter(Entity == 'World') %>% 
    select(Year, deaths = Deaths...Malaria...Sex..Both...Age..Age.standardized..Rate.)


cyklony <- read.csv("cyklony.csv")
huragan <- read.csv("huragan.csv")
wrazliwosc <- read.csv("vulnerability.csv")
swiat <- map_data("world")

huragan$time <- ymd_hms(huragan$time, tz=Sys.timezone())
wrazliwosc <- filter(wrazliwosc, is.na(X2019) == FALSE)



df <- read.csv("food.csv")
dfc <- read.csv("total-agricultural-area-over-the-long-term.csv")
colnames(df)[1] <- "Year"
mozliwe <- c("World", "Canada", "Africa", "Brazil", "India", "China",
             "Rest of Asia (excl. India & China)", "United States")
mozliwe2 <- c("SSP1_RCP60","SSP1_RCP26","SSP2_RCP26","SSP2_RCP60",
              "SSP3_RCP26","SSP3_RCP60")
df4 <- df %>% 
    filter(Variable == "Population at risk of hunger") %>% 
    filter(Factor == "Climate change impact") %>%
    filter(Region..Code. == "WLD") %>% 
    filter(Scenario..Code. == "SSP2_RCP60")  %>% 
    group_by(Year) %>% 
    summarise(srednia = mean(Value)) %>% 
    mutate(indeks = "Climate change SSP2")
df5 <- df %>% 
    filter(Variable == "Population at risk of hunger") %>% 
    filter(Factor == "Climate change impact") %>%
    filter(Region..Code. == "WLD") %>% 
    filter(Scenario..Code. == "SSP2_RCP26") %>% 
    group_by(Year) %>% 
    summarise(srednia = mean(Value)) %>% 
    mutate(indeks = "Mitigation and residual climate change SSP2")
df1 <- df %>% 
    filter(Variable == "Population at risk of hunger") %>% 
    filter(Factor == "Climate change impact") %>%
    filter(Region..Code. == "WLD") %>% 
    filter(Scenario..Code. == "SSP1_RCP60")  %>% 
    group_by(Year) %>% 
    summarise(srednia = mean(Value)) %>% 
    mutate(indeks = "Climate change SSP1")
df2 <- df %>% 
    filter(Variable == "Population at risk of hunger") %>% 
    filter(Factor == "Climate change impact") %>%
    filter(Region..Code. == "WLD") %>% 
    filter(Scenario..Code. == "SSP1_RCP26") %>% 
    group_by(Year) %>% 
    summarise(srednia = mean(Value)) %>% 
    mutate(indeks = "Mitigation and residual climate change SSP1")
df6 <- df %>% 
    filter(Variable == "Population at risk of hunger") %>% 
    filter(Factor == "Climate change impact") %>%
    filter(Region..Code. == "WLD") %>% 
    filter(Scenario..Code. == "SSP3_RCP60")  %>% 
    group_by(Year) %>% 
    summarise(srednia = mean(Value)) %>% 
    mutate(indeks = "Climate change SSP3")
df7 <- df %>% 
    filter(Variable == "Population at risk of hunger") %>% 
    filter(Factor == "Climate change impact") %>%
    filter(Region..Code. == "WLD") %>% 
    filter(Scenario..Code. == "SSP3_RCP26") %>% 
    group_by(Year) %>% 
    summarise(srednia = mean(Value)) %>% 
    mutate(indeks = "Mitigation and residual climate change SSP3")
dfk <- rbind(df1,df2, df4,df5,df6,df7)
dfmodel <- df %>% 
    filter(Variable == "Population at risk of hunger") %>% 
    filter(Region..Code. == "WLD") %>% 
    filter(Scenario..Code. == "SSP2_RCP60") %>% 
    group_by(Year, Model) %>% 
    summarise(result = mean(Value)) %>% 
    mutate(indeks = "No climate change")




library(shiny)

tlo <- bslib::bs_theme(bootswatch = "darkly",
                       base_font = "Arial")

ui <- fluidPage(
    navbarPage("Four Horsemen of the Apocalypse",
               theme = tlo,
               tabPanel("Pestilence",     titlePanel("Pestilence"),
                        textOutput("cytata"),
                        textOutput("pusty1a"),
                        textOutput("pusty2a"),
                        textOutput("pusty3a"),
                        textOutput("pusty4a"),
                        fluidRow(
                            column(12,
                                   textOutput("wstepa"))
                        ),
                        fluidRow(
                            column(4,
                                   textOutput('mapaa')),
                            column(8, 
                                   plotlyOutput("malPlota")
                            )
                        )),
               tabPanel("War",
                        titlePanel("War"),
                        theme = tlo,
                        textOutput("cytat1"),
                        textOutput("pusty1"),
                        textOutput("pusty2"),
                        textOutput("pusty3"),
                        textOutput("pusty4"),
                        
                        fluidRow(
                            column(12,
                                   textOutput("wstep")
                            )
                        ),
                        fluidRow(
                            column(3,
                                   selectInput("hura", "Select a hurricane:", unique(huragan$name)),
                                   uiOutput("czas")
                            ),
                            
                            column(9, 
                                   plotOutput("wykhur")
                            )
                        ),
                        fluidRow(
                            column(12,
                                   textOutput("huragany")
                            )
                        ),
                        fluidRow(
                            column(9, 
                                   plotlyOutput("wykwra")
                            ),
                            column(3, 
                                   textOutput("wrazliwosc")
                            )
                        ),
                        fluidRow(
                            column(12,
                                   textOutput("pusty5")
                            )
                        ),
                        fluidRow(
                            column(3, 
                                   textOutput("moc")
                            ),
                            column(9, 
                                   plotlyOutput("wykmoc")
                            )
                        )
               ),
               tabPanel("Famine",
                        titlePanel("Famine"),
                        
                        textOutput("cytat2"),
                        textOutput("pusty7"),
                        textOutput("pusty8"),
                        textOutput("pusty9"),
                        textOutput("pusty10"),
                        
                        textOutput("wstep2"),
                        textOutput("wstep3"),
                        textOutput("wstep4"),
                        textOutput("wstep5"),
                        
                        fluidRow(
                            column(9,
                                   plotlyOutput("pointPlot")
                            ),
                            column(3,
                                   "A Representative Concentration Pathway (RCP) is
               a greenhouse gas concentration (not emissions) trajectory adopted 
               by the IPCC. The pathways describe different climate futures, all 
               of which are considered possible depending on the volume of greenhouse
               gases emitted in the years to come.
               
               RCP 2.6 (Mitigation and residual climat change)
               is a very stringent pathway. RCP 2.6 requires 
               that carbon dioxide (CO2) emissions start declining by 2020 and go to zero by 2100
               RCP 6 (Climat change), here emissions peak around 2080, then decline.
               According to: Wikipedia
                "
                            )
                        ),
                        
                        fluidRow(
                            column(6,
                                   selectInput("type",
                                               "Choose features of the model",
                                               mozliwe2)
                            ),
                            column(6,
                                   
                            )
                        ),
                        fluidRow(
                            
                            column(3,
                                   "Shared Socioeconomic Pathways (SSPs) are scenarios of projected socioeconomic global
               changes up to 2100. They are used to derive greenhouse gas emissions scenarios with 
               different climate policies.
               SSP1: Sustainability (Taking the Green Road)
               SSP2: Middle of the Road
               SSP3: Regional Rivalry (A Rocky Road)
               According to wikipedia
                "),
                            column(9,
                                   plotlyOutput("linePlot")
                            )
                        ),
                        fluidRow(
                            column(12,
                                   textOutput("pusty6")
                            )
                        ),
                        
                        fluidRow(
                            column(4, 
                                   sliderInput("zakres",
                                               "Choose appropriate range",
                                               value = c(1800, max(df$Year)),
                                               min = 1800,
                                               max = max(dfc$Year),
                                               step = 1),
                                   selectInput("quality",
                                               "Choose the region",
                                               mozliwe)
                            ),
                            column(8,
                                   
                                   plotlyOutput("boxPlot")
                            )
                        )
               ),
               tabPanel("Death",     
                        titlePanel("Death"),
                        textOutput("cytatb"),
                        textOutput("pusty1b"),
                        textOutput("pusty2b"),
                        textOutput("pusty3b"),
                        textOutput("pusty4b"),
                        fluidRow(
                            column(12,
                                   textOutput("wstepb"))
                        ),
                        fluidRow(
                            column(9, 
                                   plotlyOutput("katPlotb")
                            ),
                            column(3,
                                   selectInput("przyczyna_smierci", "Choose the death cause", unique(d_c$przyczyna_smierci))
                            )
                        ),
                        fluidRow(
                            column(4,
                                   textOutput('mapab')),
                            column(2, 
                                   sliderInput("Year",
                                               "Choose the year",
                                               min = 1990,
                                               max = 2019,
                                               value = 2019)
                            ),
                            column(6, 
                                   plotOutput("mapPlotb")
                            )
                        ),
                        fluidRow(
                            column(8, 
                                   plotlyOutput("linePlotb")
                            ),
                            column(4, 
                                   textOutput("mocb")
                            )
                        ))
    )

    
    )
    




server <- function(input, output) {
    output$cytatb <- renderText({
        paste("And I looked, and behold a pale horse:
          and his name that sat on him was Death, and Hell followed with him.
          And power was given unto them over the fourth part of the earth,
          to kill with sword, and with hunger, and with death, and with the beasts of the earth.")})
    
    output$pusty1b <- renderText({
        paste(".        ")
    })
    output$pusty2b <- renderText({
        paste(".        ")
    })
    output$pusty3b <- renderText({
        paste(".        ")
    })
    output$pusty4b <- renderText({
        paste(".        ")
    })
    
    output$wstepb <- renderText({
        paste("Anticipating the impact of climate change on human health,
          the WHO estimated that each year between 2030 and 2050,
          about 250,000 more people will die of it than today. According to the WHO,
          95,000 of these people will die from malnutrition, 60,000 from malaria,
          48,000 from diarrheal diseases, and 38,000 from solar overheating.")
    })
    
    output$mapab <- renderText({
        paste("Air pollution is the fourth-leading risk factor for deaths worldwide.
          Many of the same pollutants responsible for climate change also affect
          human health through air quality impacts that are linked to respiratory
          and cardiac threats, as well as certain cancers.
          Air pollution is already responsible for more than 7 million premature deaths each year;
          1 in 10 deaths is attributable to air pollution exposure.
          While pollution-related deaths mainly strike children and the elderly, 
          pollution also results in lost labor income for working-age men and women.")
    })
    
    output$mocb <- renderText({
        paste("The graph shows how the numer of deaths from air pollution has changes over the years.
          The deaths caused by indoor air pollution decreased around 3 times,
          however nearly the same number of people is dying in consequence of outdoor air pollution")
    })
    
    output$katPlotb <- renderPlotly({
        d_c <- d_c %>%
            filter(przyczyna_smierci == input$przyczyna_smierci) %>% 
            mutate(avarage = (lag(World, n=2) + lag(World)+World+lead(World)+lead(World, n=2))/5)
        
        fig <- plot_ly(d_c, x = ~Year, y = ~avarage, color = 'green',type = 'scatter', mode='lines+markers') %>% 
            layout(
                title = list(text = "Number of deaths from climat-related disasters, World", y = 1),
                xaxis = list(domain = c(0.1, 1), title = "Year"),
                yaxis = list(domain = c(0.1, 1), title = 'Number of deaths', visible = F))
        fig
    })
    
    output$mapPlotb <- renderPlot({
        plus <- plus %>% 
            filter(Year == input$Year)
        plus%>% 
            ggplot(aes(long, lat)) + 
            geom_polygon(aes(group = group, fill = Percentage))+
            coord_map()+
            labs(
                title = "Share of deaths from air pollution")+
            theme(
                text = element_text(color = "#22211d"),
                plot.background = element_rect(fill = "#f5f5f2", color = NA),
                panel.background = element_rect(fill = "#f5f5f2", color = NA),
                legend.background = element_rect(fill = "#f5f5f2", color = NA),
                plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))
        
    })
    
    output$linePlotb <- renderPlotly({
        fig <- plot_ly(air, x = ~Year, y = ~lp_zgon, color = ~przyczyna_smierci, type = 'scatter', mode='lines+markers') %>% 
            layout(
                title = list(text = "Number of deaths from air pollution, World", y = 1),
                xaxis = list(domain = c(0.1, 1), title = "Year"),
                yaxis = list(domain = c(0.1, 1), title = 'Number of deaths', visible = F))
        fig
    })
    
    
    
    
    
    
    output$cytata <- renderText({
        paste("And I saw, and behold a white horse: and he that sat on him had a bow;
          and a crown was given unto him: and he went forth conquering, and to conquer.")
    })
    
    output$pusty1a <- renderText({
        paste(".        ")
    })
    output$pusty2a <- renderText({
        paste(".        ")
    })
    output$pusty3a <- renderText({
        paste(".        ")
    })
    output$pusty4a <- renderText({
        paste(".        ")
    })
    
    output$wstepa <- renderText({
        paste("Climate change influences health through changing the distribution
          and occurrence of vector-borne diseases.
          Warming of 2-3 degrees Celsius is estimated to increase the number of people at 
          risk of malaria by up to 5 percent globally, or more than 150 million people.
          Climate change could increase the burden of diarrhea by up to 10 percent 
          by 2030 in susceptible regions, such as South-East Asia.")
    })
    output$kataa <- renderText({
        paste("Variation in climatic conditions, such as temperature, rainfall patterns, 
          and humidity, has a profound effect on the longevity of the mosquito 
          and on the development of malaria parasites in the mosquito and, subsequently, on malaria transmission.
          It may not be possible to quantify how climate change affects malaria transmission, because it depends on many factors.
          .
          The map shows the prediction of mean temperature according to SPP2-4.5 scenario during certain periods of years.")
    })
    
    output$mapaa <- renderText({
        paste("From 1990 to 2020 the temperature on Earth rised around 0.5 degrees Celsius the numer of deaths caused by malaria decreased,
          so right now we cannot see the connection in temperature rise and malaria cases.")
    })
    
    output$tmpPlota <- renderPlot({
      ncpath <- "/Users/Jacek.LAPTOK/Desktop/"
      ncname <- input$spp  
      ncfname <- paste(ncpath, ncname, ".nc", sep="")
      ncin <- nc_open(ncfname)
      dname <- "climatology-tas-annual-mean"
      lon <- ncvar_get(ncin,"lon")
      lat <- ncvar_get(ncin,"lat")
      time <- ncvar_get(ncin,"time")
      tunits <- ncatt_get(ncin,"time","units")
      tmp_array <- ncvar_get(ncin,dname)
      dlname <- ncatt_get(ncin,dname,"long_name")
      dunits <- ncatt_get(ncin,dname,"units")
      fillvalue <- ncatt_get(ncin,dname,"_FillValue")
      title <- ncatt_get(ncin,0,"title")
      institution <- ncatt_get(ncin,0,"institution")
      datasource <- ncatt_get(ncin,0,"source")
      references <- ncatt_get(ncin,0,"references")
      history <- ncatt_get(ncin,0,"history")
      Conventions <- ncatt_get(ncin,0,"Conventions")
      tustr <- strsplit(tunits$value, " ")
      tdstr <- strsplit(unlist(tustr)[3], "-")
      tmonth <- as.integer(unlist(tdstr)[2])
      tday <- as.integer(unlist(tdstr)[3])
      tyear <- as.integer(unlist(tdstr)[1])
      chron(time,origin=c(tmonth, tday, tyear))
      tmp_array[tmp_array==fillvalue$value] <- NA
      length(na.omit(as.vector(tmp_array[,])))
      tmp_slice <- tmp_array[,]
      
      countries <- map("world", plot=FALSE) 
      countries <- map2SpatialLines(countries, proj4string = CRS("+proj=longlat"))
      
      grid <- expand.grid(lon=lon, lat=lat)
      cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
      levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
                col.regions=(rev(brewer.pal(10,"RdBu"))))+
        latticeExtra::layer(sp.lines(countries))
    })
    
    output$malPlota <- renderPlotly({
        plot_ly(malaria, x = ~Year, y = ~deaths, fill = "hotpink", type = 'scatter', mode='lines+markers') %>% 
            layout(
                title = list(text = "Number of deaths from malaria, World", y = 1),
                xaxis = list(domain = c(0.1, 1), title = "Year"),
                yaxis = list(domain = c(0.1, 1), title = 'Number of deaths', visible = F))
    })
    
    
    
    
    output$cytat1 <- renderText({
        paste("And there went out another horse that was red: and power was 
              given to him that sat thereon to take peace from the earth, and
              that they should kill one another: and there was given unto him a
              great sword.")
    })
    output$cytat2 <- renderText({
        paste("And I heard a voice in the midst of the four beasts say, A 
              measure of wheat for a penny, and three measures of barley for a
              penny; and see thou hurt not the oil and the wine.")
    })
    
    output$pusty1 <- renderText({
        paste(".        ")
    })
    output$pusty2 <- renderText({
        paste(".        ")
    })
    output$pusty3 <- renderText({
        paste(".        ")
    })
    output$pusty4 <- renderText({
        paste(".        ")
    })
    output$pusty5 <- renderText({
        paste(".        ")
    })
    output$pusty6 <- renderText({
        paste(".        ")
    })
    output$pusty7 <- renderText({
        paste(".        ")
    })
    output$pusty8 <- renderText({
        paste(".        ")
    })
    output$pusty9 <- renderText({
        paste(".        ")
    })
    output$pusty10 <- renderText({
        paste(".        ")
    })
    
    
    output$wstep <- renderText({
        paste("As we are all aware the climate change will lead to extreme 
              weather conditions such as cyclons, floods, droughts, wildfires 
              and many other. Many people will find themselves without axes to 
              fresh water or food. And this leads to conflicts and violence. We 
              are heading towards wars over water and other crucial resorces. 
              Many people will flee their homes, looking for a new place to 
              live. It is expected that by 2050 over 200 million people will 
              face such fate. This will also unfortunately lead to ethnic 
              confilcs, as many other times in history. In this page our team 
              mostly focusted on the reasons of climate war rather than 
              outcomes.")
    })
    
    output$huragany <- renderText({
        paste("The map above, shows the path (yellow) of selected cyclone. The 
              slider allows to see where was the storm at given time (red dot)
              .")
    })
    
    output$wrazliwosc <- renderText({
        paste("The map presents countries of the world by their 
                            vulnerability to the changes of the climate. Save
                            countries are marked in blue and endanger in green 
                            or yellow. We can see that richer countries with 
                            more cold climate tent to be less vulnerable than 
                            poorer countries with warmer climate. It seems that 
                            the countries of Sahel (region south of Sahara 
                            Desert) face the biggest problems conected with 
                            climate change. Other countries that will suffer the
                            most include the countries of Central Africa, or 
                            countries at war such as Afganistan or Yemen.")
    })
    
    output$moc <- renderText({
        paste("The graph shows power dissipation index for North Atlantic 
              cyclones. The index value is based of cyclone's strength, 
              frequency, duration ect. It is easy to notice that cyclones have 
              become more powerfull since the '90.")
    })
    
    output$czas <- renderUI({
        rameczka <- huragan %>% 
            filter(name == input$hura)
        sliderInput("czas",
                    "Timescale",
                    min = min(rameczka$time),
                    max = max(rameczka$time),
                    value = min(rameczka$time))
    })
    
    output$wykhur <- renderPlot({
        rameczka <- huragan %>% 
            filter(name == input$hura)
        ramczunia <- rameczka %>% 
            filter(time == input$czas)
        ggplot(NULL, aes(x = long, y = lat))+
            geom_polygon(data = swiat, aes(group = group))+
            geom_point(data = rameczka, col = "yellow")+
            geom_point(data = ramczunia, col = "red", size = 5)
    })
    
    output$wykwra <- renderPlotly({
        plot_ly(wrazliwosc,
                type='choropleth',
                locations=~ISO3,
                z=~X2019,
                text=~Name) %>% 
            colorbar(title = "Vulnerability to the climate change")
    })
    
    output$wykmoc <- renderPlotly({
        plot_ly(
            cyklony,
            x = ~Year,
            y = ~Cyclone.Power.Dissipation.Index..PDI...HUDRAT..NOAA.,
            type = "scatter",
            mode = "lines+markers"
        ) %>% 
            layout(xaxis = list(title = "Year"), yaxis = list(title = "Power dissipation index for North Atlantic cyclones")) 
    })
    
    output$wstep2 <- renderText({
        paste("We should be also aware that climat change may lead to difficulties with famine or malnutrition.
        Droughts cause large-scale crop loss, heat are also dangerous to crops. Heavy rainfalls
reduce crop productivity and may cause significant soil erosion
reducing long-term productivity.  Rising sea level might destroy low-lying coastal
agriculture in major river deltas and small islands.
              ")})
    
    output$wstep3 <- renderText({
        paste(" At the first diagram we could see the absolute difference from a baseline level (which is situation, world with no climat change) 
    in population at risk of hunger (in millions). There are six differents type of situations, each depends on our attitude to climate change.
    According to the results, there is no doubt that reducing the impact of climate change leads
    to decrease in additional number of people at risk of hunger.
    ")})
    
    
    output$wstep4 <- renderText({
        paste("
    The second diagram shows how each type of situations (connected with our attitude to climate change) is predicted by all models.
         ")})
    output$wstep5 <- renderText({
        paste("   The last diagram presents how the farmlands surface is changing.
              ")})
    
    
    output$pointPlot <- renderPlotly({
        
        plot_ly(dfk,
                x = ~Year,
                y = ~srednia,
                color = ~indeks,
                type = "bar")%>% 
            layout(
                title = "Absolute difference from a baseline level 
    in population at risk of hunger (in millions)",
                xaxis = list(title = ""),
                yaxis = list(title = ("Population")),
                legend = list(orientation = 'h')
                
            )
        
    })
    
    output$boxPlot <- renderPlotly({
        
        plot_ly(
            dfc %>% 
                select(-Code) %>% 
                filter(Year >= input$zakres[1],
                       Year <= input$zakres[2]) %>% 
                rename(number = Agricultural.area..crops...grazing...HYDE..2017..) %>% 
                filter(Entity == input$quality), 
            x = ~Year, 
            y = ~number,
            color =~Entity,
            type = "scatter", mode = 'lines') %>% 
            layout(
                title = "Farmlands surface",
                xaxis = list(title = "Year"),
                yaxis = list(title = "Surface"),
                legend = list(orientation = 'h')
            ) 
        
    })
    
    output$linePlot <- renderPlotly({
        
        plot_ly(
            df %>% 
                filter(Variable == "Population at risk of hunger") %>% 
                filter(Factor == "Climate change impact") %>%
                filter(Region..Code. == "WLD") %>% 
                filter(Scenario..Code. == input$type)  %>% 
                group_by(Year, Model) %>% 
                summarise(srednia = mean(Value)),
            x = ~Year,
            y = ~srednia,
            color = ~Model,
            type = "bar"
        )%>% 
            layout(
                title = "Absolute difference from a baseline level in population at risk
                of hunger (in millions) according to different models",
                xaxis = list(title = ""),
                yaxis = list(title = ("Population")),
                legend = list(orientation = 'h'))
        
    })
    
    
    
}


shinyApp(ui = ui, server = server)

