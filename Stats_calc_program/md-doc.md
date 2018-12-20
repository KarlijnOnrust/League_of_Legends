R app code
================

Inserting libraries

``` r
library(shiny)
library(rvest)
library(stringr)
library(rpart)
library(ggplot2)
library(plotly)
library(rpivotTable)
library(DT)
library(visNetwork)
```

UI

``` r
ui <- fluidPage(
  includeCSS("styles.css"),
  
  navbarPage(
    "Champion stat calculator",
    tabPanel("Decision tree",
             tabsetPanel(
               tabPanel("Anova", br(),
                        tabsetPanel(
                          tabPanel("LVL one + ratios", br(),
                                   visNetworkOutput("fitLVLoneratios")),
                          tabPanel("LVL one", br(),
                                   visNetworkOutput("fitLVLone")),
                          tabPanel("LVL eighteen", br(),
                                   visNetworkOutput("fitLVLeighteen"))
                        )),
               tabPanel("Class", br(),
                        tabsetPanel(
                          tabPanel("LVL one + ratios", br(),
                                   visNetworkOutput("fitLVLoneratiosc")),
                          tabPanel("LVL one", br(),
                                   visNetworkOutput("fitLVLonec")),
                          tabPanel("LVL eighteen", br(),
                                   visNetworkOutput("fitLVLeighteenc"))
                        ))
             )),
    tabPanel("Visuals",
             tabsetPanel(
               tabPanel("With ratio's", br(),
                        sidebarLayout(
                          sidebarPanel(class = "nav",
                                       tags$table(tags$tr(
                                         tags$th(class = "marg",
                                                 selectInput(
                                                   "x_asr",
                                                   "X_AS",
                                                   c(
                                                     "HP",
                                                     "HP per lvl",
                                                     "HP5",
                                                     "HP5 per lvl",
                                                     "MP",
                                                     "MP per lvl",
                                                     "MP5",
                                                     "MP5 per lvl",
                                                     "AD",
                                                     "AD per lvl",
                                                     "AS",
                                                     "AS per lvl",
                                                     "AR",
                                                     "AR per lvl",
                                                     "MR",
                                                     "MR per lvl",
                                                     "MS",
                                                     "Range"
                                                   ),
                                                   width = 100
                                                 )),
                                         tags$th(class = "marg",
                                                 selectInput(
                                                   "y_asr",
                                                   "Y_AS",
                                                   c(
                                                     "HP5",
                                                     "HP",
                                                     "HP per lvl",
                                                     "HP5 per lvl",
                                                     "MP",
                                                     "MP per lvl",
                                                     "MP5",
                                                     "MP5 per lvl",
                                                     "AD",
                                                     "AD per lvl",
                                                     "AS",
                                                     "AS per lvl",
                                                     "AR",
                                                     "AR per lvl",
                                                     "MR",
                                                     "MR per lvl",
                                                     "MS",
                                                     "Range"
                                                   ),
                                                   width = 100
                                                 ))
                                       ))),
                          mainPanel(plotlyOutput("ratiovisual"))
                        )),
               tabPanel("No ratio's", br(),
                        sidebarLayout(
                          sidebarPanel(class = "nav",
                                       tags$table(tags$tr(
                                         tags$th(class = "marg",
                                                 selectInput(
                                                   "x_as",
                                                   "X_AS",
                                                   c("HP", "HP5", "MP", "MP5", "AD", "AS", "AR", "MR", "MS", "Range"),
                                                   width = 100
                                                 )),
                                         tags$th(class = "marg",
                                                 selectInput(
                                                   "y_as",
                                                   "Y_AS",
                                                   c("MP", "HP", "HP5", "MP5", "AD", "AS", "AR", "MR", "MS", "Range"),
                                                   width = 100
                                                 ))
                                       ))),
                          mainPanel(tabsetPanel(
                            tabPanel("LVL one", br(),
                                     plotlyOutput("lvlonevisual")),
                            tabPanel("LVL eighteen", br(),
                                     plotlyOutput("lvleighteenvisual"))
                          ))
                        ))
             )),
    tabPanel("Tables",
             tabsetPanel(
               tabPanel("LVL one", br(),
                        tabsetPanel(
                          tabPanel("Table", br(),
                                   dataTableOutput("lvlonedt")),
                          tabPanel("Pivot", br(),
                                   rpivotTableOutput("lvlonepivot"))
                        )),
               tabPanel("LVL eighteen", br(),
                        tabsetPanel(
                          tabPanel("Table", br(),
                                   dataTableOutput("lvleighteendt")),
                          tabPanel("Pivot", br(),
                                   rpivotTableOutput("lvleighteenpivot"))
                        ))
             )),
    tabPanel(
      "Usage",
      p("Made by Karlijn Onrust"),
      p(
        "Data from ", tags$a(href="http://leagueoflegends.wikia.com/wiki/List_of_champions/Base_statistics", "http://leagueoflegends.wikia.com/wiki/List_of_champions/Base_statistics")
      ),
      br(),
      p(
        "You can find general rules for balancing in the clasificationtrees. Use this for making a decision on which stats accompany each other"
      ),
      p(
        "In the fisuals, you can see the ratio's of certain stats. The decisiontrees can help by making a decision on which stats to compare"
      ),
      p(
        "I used the datatable to see the five highest and lowest numbers of one stat and how this is balanced out in the other stats"
      ),
      p("Use the pivottable as you like")
    )
  )
)
```

Server

``` r
server <- function(input, output) {
  #inport data
  r <-
    read_html("http://leagueoflegends.wikia.com/wiki/List_of_champions/Base_statistics")
  Data <-
    r %>% html_node("table.wikitable") %>% html_table(fill = TRUE)
  #clean data
  c <- data.frame(Data$Champions)
  c <- data.frame(word(c$Data.Champions, 1, sep = " "))
  Data$`AS+` = substr(Data$`AS+`, 2, nchar(Data$`AS+`) - 1)
  Data$`AS+` = as.numeric(Data$`AS+`) / 100
  Data <- data.frame(data.matrix(Data))
  Data$Champions <- c$word.c.Data.Champions..1..sep.......
  
  baselvl1 <-
    data.frame(
      Data$Champions,
      Data$HP,
      Data$HP.,
      Data$HP5,
      Data$HP5.,
      Data$MP,
      Data$MP.,
      Data$MP5,
      Data$MP5.,
      Data$AD,
      Data$AD.,
      Data$AS,
      Data$AS.,
      Data$AR,
      Data$AR.,
      Data$MR,
      Data$MR.,
      Data$MS,
      Data$Range
    )
  colnames(baselvl1) <-
    c(
      "Champions",
      "HP",
      "HP per lvl",
      "HP5",
      "HP5 per lvl",
      "MP",
      "MP per lvl",
      "MP5",
      "MP5 per lvl",
      "AD",
      "AD per lvl",
      "AS",
      "AS per lvl",
      "AR",
      "AR per lvl",
      "MR",
      "MR per lvl",
      "MS",
      "Range"
    )
  
  baselvl18 <-
    data.frame(
      Data$Champions,
      Data$HP + Data$HP. * 18,
      Data$HP5 + Data$HP5. * 18,
      Data$MP + Data$MP. * 18,
      Data$MP5 + Data$MP5. * 18,
      Data$AD + Data$AD. * 18,
      Data$AS * (+Data$AS. * 18),
      Data$AR + Data$AR. * 18,
      Data$MR + Data$MR. * 18,
      Data$MS,
      Data$Range
    )
  colnames(baselvl18) <-
    c("Champions",
      "HP",
      "HP5",
      "MP",
      "MP5",
      "AD",
      "AS",
      "AR",
      "MR",
      "MS",
      "Range")
  
  fitLVLoneratios <-
    rpart(
      Champions ~ HP + `HP per lvl` + HP5 + `HP5 per lvl` + MP + `MP per lvl` + MP5 + `MP5 per lvl` + AD + `AD per lvl` + AS + `AS per lvl` + AR + `AR per lvl` + MR + `MR per lvl` + MS + Range,
      data = baselvl1,
      method = "anova",
      control = rpart.control(
        minsplit = 1,
        minbucket = 1,
        cp = 0
      )
    )
  
  output$fitLVLoneratios <- renderVisNetwork({
    visTree(fitLVLoneratios)
  })
  
  fitLVLoneratiosc <-
    rpart(
      Champions ~ HP + `HP per lvl` + HP5 + `HP5 per lvl` + MP + `MP per lvl` + MP5 + `MP5 per lvl` + AD + `AD per lvl` + AS + `AS per lvl` + AR + `AR per lvl` + MR + `MR per lvl` + MS + Range,
      data = baselvl1,
      method = "class",
      control = rpart.control(
        minsplit = 1,
        minbucket = 1,
        cp = 0
      )
    )
  
  output$fitLVLoneratiosc <- renderVisNetwork({
    visTree(fitLVLoneratiosc)
  })
  
  fitLVLone <-
    rpart(
      Champions ~ HP + HP5 + MP + MP5 + AD + AS + AR + MR + MS + Range,
      data = baselvl1,
      method = "anova",
      control = rpart.control(
        minsplit = 1,
        minbucket = 1,
        cp = 0
      )
    )
  
  output$fitLVLone <- renderVisNetwork({
    visTree(fitLVLone)
  })
  
  fitLVLonec <-
    rpart(
      Champions ~ HP + HP5 + MP + MP5 + AD + AS + AR + MR + MS + Range,
      data = baselvl1,
      method = "class",
      control = rpart.control(
        minsplit = 1,
        minbucket = 1,
        cp = 0
      )
    )
  
  output$fitLVLonec <- renderVisNetwork({
    visTree(fitLVLonec)
  })
  
  fitLVLeighteen <-
    rpart(
      Champions ~ HP + HP5 + MP + MP5 + AD + AS + AR + MR + MS + Range,
      data = baselvl18,
      method = "anova",
      control = rpart.control(
        minsplit = 1,
        minbucket = 1,
        cp = 0
      )
    )
  
  output$fitLVLeighteen <- renderVisNetwork({
    visTree(fitLVLeighteen)
  })
  
  fitLVLeighteenc <-
    rpart(
      Champions ~ HP + HP5 + MP + MP5 + AD + AS + AR + MR + MS + Range,
      data = baselvl18,
      method = "class",
      control = rpart.control(
        minsplit = 1,
        minbucket = 1,
        cp = 0
      )
    )
  
  output$fitLVLeighteenc <- renderVisNetwork({
    visTree(fitLVLeighteenc)
  })
  
  
  output$lvlonevisual <- renderPlotly({
    q <-
      ggplot(baselvl1, aes(baselvl1[[input$x_as]], baselvl1[[input$y_as]])) + geom_point(size =
                                                                                           1,
                                                                                         col = "darkgreen",
                                                                                         aes(text = baselvl1$Champions)) + geom_smooth(method = "glm") + labs(x =
                                                                                                                                                                input$x_as, y = input$y_as)
    ggplotly(q)
  })
  
  output$lvleighteenvisual <- renderPlotly({
    p <-
      ggplot(baselvl18, aes(baselvl18[[input$x_as]], baselvl18[[input$y_as]])) + geom_point(size =
                                                                                              1,
                                                                                            col = "darkgreen",
                                                                                            aes(text = baselvl18$Champions)) + geom_smooth(method = "glm") + labs(x =
                                                                                                                                                                    input$x_as, y = input$y_as)
    ggplotly(p)
  })
  
  output$ratiovisual <- renderPlotly({
    q <-
      ggplot(baselvl1, aes(baselvl1[[input$x_asr]], baselvl1[[input$y_asr]])) + geom_point(size =
                                                                                             1,
                                                                                           col = "darkgreen",
                                                                                           aes(text = baselvl1$Champions)) + geom_smooth(method = "glm") + labs(x =
                                                                                                                                                                  input$x_asr, y = input$y_asr)
    ggplotly(q)
  })
  
  
  output$lvlonedt <- renderDataTable({
    datatable(baselvl1)
  })
  
  output$lvleighteendt <- renderDataTable({
    datatable(baselvl18)
  })
  
  output$lvlonepivot <- renderRpivotTable({
    rpivotTable(baselvl1)
  })
  
  output$lvleighteenpivot <- renderRpivotTable({
    rpivotTable(baselvl18)
  })
}
```

Run app

``` r
#shinyApp(ui = ui, server = server)
```
