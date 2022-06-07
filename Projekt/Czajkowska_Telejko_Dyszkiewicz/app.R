library(dplyr)
library(plotly)         
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(dashboardthemes)
library(countrycode)

dfwood <- read.csv("Wood_data.csv")

df_brazil <- read.csv('loss_brazil.csv')

df <- read.csv("forest-area-as-share-of-land-area.csv") %>% 
  filter(Year >= 1990) %>% 
  filter(Code != "" & Entity != "World")

World <- df %>% 
  mutate(Continent = countrycode(sourcevar = df$Entity,
                                 origin = "country.name",
                                 destination = "continent"),
         Continent = case_when(Continent == "Americas" ~ countrycode(sourcevar = df$Entity,
                                                                     origin = "country.name",
                                                                     destination = "region"),
                               TRUE ~Continent),
         Continent = case_when(Continent == "Latin America & Caribbean" ~ "south america",
                               TRUE ~ Continent),
         Continent = case_when(Entity == "Timor" ~ "Asia",
                               Entity == "Mexico" ~ "north america",
                               TRUE ~ Continent),
         Continent = tolower(Continent),
         Forest.cover = round(Forest.cover, 1))

df1 <- dfwood %>% 
    filter(Unit == "m3") %>% 
    select("Area", "Item", "Year", "Value")

df2 <- dfwood %>% 
    filter(Unit == "tonnes") %>% 
    select("Area", "Item", "Year", "Value")

customTheme <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "black"
  ,primaryFontColor = "green"
  ,infoFontColor = "green"
  ,successFontColor = "green"
  ,warningFontColor = "green"
  ,dangerFontColor = "green"
  ,bodyBackColor = "white"
  
  ### header
  ,logoBackColor = "#0A0808"
  
  ,headerButtonBackColor = "#0A0808"
  ,headerButtonIconColor = "#FFFFFF"
  ,headerButtonBackColorHover = "#FFFFFF"
  ,headerButtonIconColorHover = "#0A0808"
  
  ,headerBackColor = "#0A0808"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  ,sidebarBackColor = "#B5B3B3"
  ,sidebarPadding = "12"
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = "0"
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarUserTextColor = "#000000"
  
  ,sidebarSearchBackColor = "#FFFAFA"
  ,sidebarSearchIconColor = "#E61C1C"
  ,sidebarSearchBorderColor = "#F57208"
  
  ,sidebarTabTextColor = "#000000"
  ,sidebarTabTextSize = "15"
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = "0"
  
  ,sidebarTabBackColorSelected = "#FFFFFF"
  ,sidebarTabTextColorSelected = "#000000"
  ,sidebarTabRadiusSelected = "0px"
  
  ,sidebarTabBackColorHover = "#FFFFFF"
  ,sidebarTabTextColorHover = "#000000"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "#C8C8C8"
  ,sidebarTabBorderWidthHover = "9"
  ,sidebarTabRadiusHover = "0px"
  
  ### boxes
  ,boxBackColor = "#8CBF89"
  ,boxBorderRadius = "2"
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = "18"
  ,boxDefaultColor = "#E1EFE1"
  ,boxPrimaryColor = "#green"
  ,boxInfoColor = "#B4B4B4"
  ,boxSuccessColor = "#70AD47"
  ,boxWarningColor = "#ED7D31"
  ,boxDangerColor = "#E84C22"
  
  ,tabBoxTabColor = "#F8F8F8"
  ,tabBoxTabTextSize = "14"
  ,tabBoxTabTextColor = "#000000"
  ,tabBoxTabTextColorSelected = "#2D2D2D"
  ,tabBoxBackColor = "#F8F8F8"
  ,tabBoxHighlightColor = "#C8C8C8"
  ,tabBoxBorderRadius = "5"
  
  ### inputs
  ,buttonBackColor = "white"
  ,buttonTextColor = "#2D2D2D"
  ,buttonBorderColor = "#969696"
  ,buttonBorderRadius = "5"
  
  ,buttonBackColorHover = "white"
  ,buttonTextColorHover = "#000000"
  ,buttonBorderColorHover = "#969696"
  
  ,textboxBackColor = "#FFFFFF"
  ,textboxBorderColor = "#767676"
  ,textboxBorderRadius = "5"
  ,textboxBackColorSelect = "#A8A8A8"
  ,textboxBorderColorSelect = "#6C6C6C"
  
  ### tables
  ,tableBackColor = "#F8F8F8"
  ,tableBorderColor = "#EEEEEE"
  ,tableBorderTopSize = "1"
  ,tableBorderRowSize = "1"
)

### tabItems
introduction <- tabItem(
  tabName = "introduction",
  class = "active",
  fluidRow(
    class = "text-center",
    box(h1("Forests", style = "font-size:70px;font-weight:bold;font-family:Times New Roman"),
        width = 12
    )
  ),
  fluidRow(
    class = "text-center",
    box(width = 4,
        height = 230,
        h1("60,082", style = "font-size:70px;font-family:Times New Roman;font-weight:bold"),
        h3("The GlobalTreeSearch database reports the existence of 60,082 tree species.", 
           style = "font-family:Times New Roman")),
  box(width = 4,
      height = 230,
      h1("5,000,000", style = "font-size:70px;font-family:Times New Roman;font-weight:bold"),
      h3("Every year the world loses around 5 million hectares of forest. 95% of
          this occurs in the tropics.", 
         style = "font-family:Times New Roman")),
  box(width = 4,
      height = 230,
      h1("90%", style = "font-size:70px;font-family:Times New Roman;font-weight:bold"),
      h3("Of the people living in extreme poverty, over 90 percent are dependant 
         on forests for at least part of their livelihoods.", 
         style = "font-family:Times New Roman"))
  ),
  fluidRow(
    box(width = 12,
        h3("This dashboard deals with the topic of forests, trying to draw attention to the 
           problem of their reduction. See the distribution of global forests and which countries 
           have the most forest cover, learn about the factors of deforestation, and look at the 
           figures for wood use.",
           style = "font-family:Times New Roman"))
  ),
  fluidRow(
    column(12,
           img(src = "forest.jpg", height = '100%', width = '100%'))
  )
)

worldmap <- tabItem(
  tabName = "worldmap",
  setSliderColor("#326840", c(1, 2)),
  fluidRow(
    box(width = 12,
      h3("This interactive map shows the share of total land area that is
      covered by forest. You can select the continent and the year you want to see.")
      )
  ),
  fluidRow(
    box(width = 8, 
        plotlyOutput("map"),
        height = 825
    ),
    box(width = 4,
           selectInput("region", " ", 
                       c('World','Europe', 'Asia', 'Africa', 'North America', 'South America'), selected = "World"),
           sliderInput("year2", "Year:",
                                 min = 1990, max = 2020, value = 2020),
          h3("We see that, in 2020, South America had the greatest share of global
              forests per country. However, it has suffered with the biggest decrese
              in the share of total land area that is covered by forest. For instance,
              in Paraguay it has dropped from 64.3% to 40.5% in the last 30 years.")))
)

deforastation <- tabItem(
  tabName = "deforastation",
  fluidRow(
    box(width = 12,
      h3("The plot shows the drivers of deforestation. You can select each driver on 
         the legend and see hectares distribution depending on year.")
    )
  ),
  fluidRow(
    box(width = 8,
        height = 825,
        plotlyOutput("chaPlot")    
    ),
    box(width = 4, 
        h3("From this plot, we would conclude that the dominant drivers of deforestation 
           in the Brazilian Amazon were small scale farming and the expansion of pasture 
           for beef production. If we look at forest loss from commercial crops – which 
           is mainly soybeans- we see a significant decline. A large part of forests is 
           cut down also because of tree plantation mainly for palm oil. Also natural 
           disturbances such as flooding or fire are the drivers of forest loss.")))
  
)

woodproduction <- tabItem(
  tabName = "woodproduction",
  chooseSliderSkin(color = "#326840"),
  fluidRow(
    box(h3("The charts below show the production of different categories of wood 
        products on different continents. The first plot shows the data of products, 
        the quantity of which is expressed in cubic meters. The second plot shows 
        data for products expressed in tonnes. You can select the continents you 
        want to see in the chart, the range of years, as well as individual product 
        categories. Each pair of the continent with the category corresponds to 
        one line on the chart, the label of which appears when you hover the mouse 
        over it."),
        width = 12
        )
  ),
  fluidRow(
    box(pickerInput("continent", "Choose continents", unique(df1$Area), unique(df1$Area), multiple = T),
        width = 6,
        height = 120
    ),
    box(sliderInput("year", "Choose range of years", 1961, 2020, 1, value = c(1961, 2020)),
        width = 6,
        height = 120
    )
  ),
  column(9,
    fluidRow(
      box(
        plotlyOutput("plot_m3", height = 475),
        width = 8,
        height = 500,
      ),
      box(
         checkboxGroupInput("items", "Choose product categories", unique(df1$Item), selected = "Roundwood"),
         width =4,
         height = 500
      ),
      
    ),
  fluidRow(
    box(
      plotlyOutput("plot_tonnes", height = 475),
      width = 8,
      height = 500
    ),
    box(
      checkboxGroupInput("items_t", "Choose product categories", unique(df2$Item), 
                         selected = "Wood pellets and other agglomerates"),
      width = 4,
      height = 500
    ))
  ),
  fluidRow(
    column(3,
      box(width = 12,
         h3("In the charts, we can see that the forestry production has generally increased 
       over the years, although in most cases the increase is slight. The production of total fiber 
       furnish, paper and paperboard (especially in Asia), as well as wood pulp and pulp for paper 
       increased significantly. Asia is the leader in world production. Oceania and Africa don't 
       produce that much as other continents, although we can observe rapidly increasing production 
       of wood fuel and roundwood, amounting to 700-800 million in recent years. An interesting 
       observation are the sudden drops in production from time to time. Moreover, a proportional 
       decrease usually occurs on every continent where a given product is produced. In general 
       roundwood, industrial roundwood, woodfuel, paper and paperboard are produced the most.")
      )     
    )
  )
)
  
body <- dashboardBody(
  customTheme,
  tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
  tabItems(
    introduction,
    worldmap,
    deforastation,
    woodproduction
  )
)

ui <- dashboardPage(skin = "green",
      dashboardHeader(title = "Forests"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Introduction", tabName = "introduction"),
          menuItem("Share of global forest area", tabName = "worldmap"),
          menuItem("Drivers of deforestation", tabName = "deforastation"),
          menuItem("Wood Production", tabName = "woodproduction")
        )
      ),
      body
    )
    

server <- function(input, output) {
    
    output$plot_m3 <- renderPlotly({
        df4 <- df1 %>% 
            filter(Year >= input$year[1] & Year <= input$year[2] & Area %in% input$continent & Item %in% input$items) 
            plot_ly(df4,
                    x = ~Year, 
                    y = ~Value, 
                    color = ~Area,
                    colors = "palegreen4",
                    split = ~Item,
                    type = 'scatter', 
                    mode = 'lines',
                    hoverinfo = 'text',
                    text = ~paste('</br> Continent: ', Area,
                                  '</br> Category of products: ', Item)) %>%
        layout(title = list(text = 'Forestry production on different continents in the following years', 
                            font = list(size = 25)),
               plot_bgcolor = "#FBFBFB", 
               xaxis = list(title = 'Year'), 
               yaxis = list(title = 'Quantity in m^3'),
               showlegend = FALSE,
               margin = list(t = 100))
        
    })
    
    output$plot_tonnes <- renderPlotly({
        df3 <- df2 %>% 
            filter(Year >= input$year[1] & Year <= input$year[2] & Area %in% input$continent & Item %in% input$items_t) 
        plot_ly(df3, 
                x = ~Year, 
                y = ~Value, 
                color = ~Area,
                colors = "palegreen4",
                split = ~Item,
                type = 'scatter', 
                mode = 'lines',
                hoverinfo = 'text',
                text = ~paste('</br> Continent: ', Area,
                              '</br> Category of products: ', Item)) %>%
    layout(title = list(text = 'Forestry production on different continents in the following years', 
                        font = list(size = 25)),
           plot_bgcolor = "#FBFBFB", 
           xaxis = list(title = 'Year'), 
           yaxis = list(title = 'Quantity in tonnes'),
           showlegend = FALSE,
           margin = list(t = 100))
    })
    
    output$map <- 
      
      renderPlotly({
        
        data <- if (input$region == "World"){
          World
        } else {filter(World, Continent == tolower(input$region))}
        p <- data %>% 
          filter(Year == input$year2) %>% 
          plot_geo() %>%
          add_trace( z = ~Forest.cover, 
                     locations = ~Code, 
                     text = ~Entity,
                     color = ~Forest.cover,
                     colorscale = "Greens",
                     reversescale = TRUE) %>% 
          colorbar(title = list(text = "Forest cover")) %>%
          layout(geo = list(scope = tolower(input$region)),
                 title= list(text = "Share of global forest area",
                             font = list(size = 30)),
                 showlegend = FALSE,
                 height = 800,
                 margin = list(t = 100)) 
        
        p
      })
    
    output$text <- renderText(({
      
      paste(h3("From this plot, we would conclude that the 
        dominant driver of deforestation in the Brazilian Amazon
        was the expansion of pasture for beef production.
        If we look at forest loss from commercial crops – which
        is mainly soybeans – we see a significant decline. A posture
        is driving most deforestation in the Brazillian Amazon. But,
        this only looks at the direct drivers of deforestation.
              In other words, the cutting down of forest today to make
              space for cropland for soy production."))
      
    }))
    
    output$chaPlot <- renderPlotly({
      fig <- plot_ly(data= df_brazil, x = ~Year, y = ~commercial_crops, name = 'Commercial crops', type = 'scatter', mode = 'none', stackgroup = 'one', fillcolor = '#A020F066')
      fig <- fig %>% add_trace(y = ~flooding_due_to_dams, name = 'Flooding', fillcolor = '#4C78A8')
      
      fig <- fig %>% add_trace(y = ~natural_disturbances, name = 'Natural disturbances', fillcolor = '#9467BD')
      
      fig <- fig %>% add_trace(y = ~pasture, name = 'Pasture (beef)', fillcolor = 'rgb(27,158,119,0.1')
      
      fig <- fig %>% add_trace(y = ~selective_logging, name = 'Selective logging', fillcolor = 'rgb(148,52,110')
      
      fig <- fig %>% add_trace(y = ~fire, name = 'Fire', fillcolor = 'rgb(242,183,1)')
      
      fig <- fig %>% add_trace(y = ~mining, name = 'Mining', fillcolor = '#222A2A')
      
      fig <- fig %>% add_trace(y = ~other_infrastructure, name = 'Other infrastructure', fillcolor = '#990099')
      
      fig <- fig %>% add_trace(y = ~roads, name = 'Roads', fillcolor = '#109618')
      
      fig <- fig %>% add_trace(y = ~tree_plantations_including_palm, name = 'Tree plantation', fillcolor = '#B6E880')
      
      fig <- fig %>% add_trace(y = ~small.scale_clearing, name = 'Small scale farming', fillcolor = 'rgb(229,196,148)')
      fig %>%  add_markers(marker = list(opacity = 0.4))
      fig <- fig %>% layout(title = list(text = 'Drivers of forest loss in the Brizalian Amazon',
                                         font = list(size = 30)),
                            legend = list(font = list(size = 18)),
                            
                            xaxis = list(title = "",
                                         
                                         showgrid = FALSE),
                            
                            yaxis = list(title = " Ha",
                                         
                                         showgrid = TRUE),
                            height = 800,
                            margin = list(t = 100))
    })
}

shinyApp(ui = ui, server = server)
