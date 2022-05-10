library(dplyr)
library(plotly)


df <- read.csv("bestsellers.csv")


df_1 <- df %>% 
  filter(Genre == "Fiction", Year == 2021)

df_2 <- df %>% 
  filter(Genre == "Non Fiction", Year == 2021)

fig_1 <- plot_ly(
  data = df_1,
  y = ~Reviews,
  type = "box",
  name = "Fiction, Reviews",
  fillcolor = "rgb(168, 138, 219)",
  line = list(color = "rgb(95, 31, 207)"),
  marker = list(color = "rgb(95, 31, 207)" )
) %>% 
  add_trace(
    data = df_2,
    y = ~Reviews,
    type = "box",
    visible = FALSE,
    name = "Non Fiction, Reviews",
    fillcolor = "rgb(138, 242, 234)",
    line = list(color = "rgb(5, 171, 158)"),
    marker = list(color = "rgb(5, 171, 158)" )
    )%>% 
  layout(
    showlegend = FALSE,
    xaxis = list(title = "",
                 showticklabels = FALSE),
    yaxis = list(title = "<b> Reviews <b>",
                 range = c(0, 145000))
  ) 

fig_2 <- plot_ly(
  data = df_1,
  y = ~Price,
  type = "box",
  name = "Fiction, Price",
  fillcolor = "rgb(168, 138, 219)",
  line = list(color = "rgb(95, 31, 207)"),
  marker = list(color = "rgb(95, 31, 207)" )
) %>% add_trace(
  data = df_2,
  y = ~Price,
  type = "box",
  visible = FALSE,
  name = "Non Fiction, Price",
  fillcolor = "rgb(138, 242, 234)",
  line = list(color = "rgb(5, 171, 158)"),
  marker = list(color = "rgb(5, 171, 158)" )
) %>% layout(
  showlegend = FALSE,
  xaxis = list(title = "",
               showticklabels = FALSE),
  yaxis = list(title = "<b> Price <b>",
               range = c(0, 22))
)  
  
  

fig_3 <- plot_ly(
  data = df_1,
  y = ~User.Rating,
  type = "box",
  name = "Fiction, User Rating",
  fillcolor = "rgb(168, 138, 219)",
  line = list(color = "rgb(95, 31, 207)"),
  marker = list(color = "rgb(95, 31, 207)" )
) %>% add_trace(
  data = df_2,
  y = ~User.Rating,
  type = "box",
  visible = FALSE,
  name = "Non Fiction, User Rating",
  fillcolor = "rgb(138, 242, 234)",
  line = list(color = "rgb(5, 171, 158)"),
  marker = list(color = "rgb(5, 171, 158)" )
)%>% layout(
  showlegend = FALSE,
  xaxis = list(title = "",
               showticklabels = FALSE),
  yaxis = list(title = "<b> User Rating <b>",
               range = c(4.25, 5))
)  

fig <- subplot(fig_1, fig_2, fig_3, titleY = TRUE, titleX = TRUE)

fig <- fig %>% 
  layout(
    title = "<b> Distribution of Number of Reviews, Price and User Rating for Fiction <b>",
    updatemenus = list(
      list(
        x = 1.1,
        y = 0.7,
        buttons = list(
          list(method = "update",
               args = list(list(visible = c(TRUE, FALSE)),
                           list(title = "<b> Distribution of Number of Reviews, Price and User Rating for Fiction <b>")),
               label = "Fiction"),
          list(method = "update",
               args = list(list(visible = c(FALSE, TRUE)),
                           list(title = "<b> Distribution of Number of Reviews, Price and User Rating for Non Fiction <b>")),
               label = "Non Fiction")
        ))
    ))

fig <- fig %>% add_annotations(text = "<b> Genre <b>", 
                               x = 1.1, 
                               y = 0.75, 
                               xref = "paper", 
                               yref = "paper", 
                               showarrow = FALSE,
                               font = list(size = 14))
  

fig <- fig %>% 
  layout(margin = list(t = 100))

fig

