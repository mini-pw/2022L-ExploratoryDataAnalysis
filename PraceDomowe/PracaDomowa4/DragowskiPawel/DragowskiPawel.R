#Pd4
library(plotly)
library (dplyr)
library(readr)
df<- read_csv("bestsellers_with_categories_2022_03_27.csv")%>%
rename(Rating="User Rating")

wykres<-plot_ly(df,y=~Rating,x=~Price,frame = ~Year,color=~Genre,type="scatter",colors=c("blue","red"),mode = 'markers'
                ,hoverinfo = 'text',
                text = ~paste('</br> Author: ', Author,
                              '</br> Title: ', Name))%>%
  layout(title="Ratings of books depending on price",
         legend = list(x=0.8
                       ,y=0.1,title=list(text='Genre'))
        )%>%
  animation_opts(5000,redraw=TRUE)%>%
  animation_slider(currentvalue = list(prefix = "YEAR ", font = list(color="GREEN")))
wykres

htmlwidgets::saveWidget(as_widget(wykres), "wykres.html")
