###Compare stats
##Visual
#TODO: search on champs
#TODO: drilling when filtering
#TODO: Navbar css

##Table
#TODO: Table on filter from visual


###Champs stats
#choose champ
#choose level
#titel = selected champ
#autoselected champ = AAA
#star with stats

###about this
#text



library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

Champs <- read_excel("All Champions Stats.xlsx", sheet = "NAME")
Types <- read_excel("All Champions Stats.xlsx", sheet = "TYPES")
Primary <- read_excel("All Champions Stats.xlsx", sheet = "PRIMARY")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  includeCSS("styles.css"),
  
  navbarPage("League of Legends, Raw stats",
    tabPanel("Compare stats",
      sidebarLayout(
        sidebarPanel(class = "nav",
          sliderInput("level", "Level", 1, 18, 1),
          tags$table(
            tags$tr(
              tags$th(class = "marg",
                selectInput("x_as", "X_AS", c("HP", "AR", "AD", "AS", "MR", "HP5"), width = 100)
              ),
              tags$th(class = "marg",
                selectInput("y_as", "Y_AS", c("AR", "HP", "AD", "AS", "MR", "HP5"), width = 100)
              )
            )
          ),
          tags$table(
            tags$tr(
              tags$th(
                tags$div( class = "champscroll", checkboxGroupInput("Champs", "Champion", Champs$NAME))
              ),
              tags$th(
                checkboxGroupInput("Primary", "Primary", Primary$PRIMARY)
              ),
              tags$th(
                checkboxGroupInput("Type", "Type", Types$TYPE)
                )
            )
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Visual", br(),
              plotOutput("stats_v", height = 700)
            ),
            tabPanel("Table", br(),
              tableOutput("stats_t")
            )
          )
        )
      )
    )
  )
)
  

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  Base_stats <- read_excel("All Champions Stats.xlsx", sheet = "Base Stats")
 
  lv <- reactive({
    input$level
  })
  
  plusx <- reactive({
    paste(input$x_as, "PLUS", sep="")
  }) 
  
  plusy <- reactive({
    paste(input$y_as, "PLUS", sep="")
  }) 
  
  filtered <- NULL
  
  as.data.frame(filtered)
  
  output$stats_v <- renderPlot({
    
    if(is.null(input$Type)){
      if(is.null(input$Primary)){
        if(is.null(input$Champ)){
          filtered <- Base_stats
        }
      }
    }
    
    if(!is.null(input$Type)){
      i = as.list(input$Type) 
      filtered = NULL
      as.data.frame(filtered)
      while(length(i) > 0){
        filtered <- rbind(filtered, filter(Base_stats, Base_stats$TYPE == i[1]))
        i[1] = NULL
      }
    }
    
    if(!is.null(input$Primary)){
      i = as.list(input$Primary)
      filtered = NULL
      as.data.frame(filtered)
      while(length(i) > 0){
        filtered <- rbind(filtered, filter(Base_stats, Base_stats$PRIMARY == i[1]))
        i[1] = NULL
      }
    }
    
    if(!is.null(input$Champs)){
      i = as.list(input$Champs)
      filtered = NULL
      as.data.frame(filtered)
      while(length(i) > 0){
        filtered <- rbind(filtered, filter(Base_stats, Base_stats$NAME == i[1]))
        i[1] = NULL
      }
    }
    
    #type and primary
    
    
    #type and champ
    
    
    #primary and Champ
    
    
    #primary, champ and type
    
    

    ggplot(filtered, aes(filtered[[input$x_as]] + filtered[[plusx()]] * (lv() - 1), filtered[[input$y_as]] + filtered[[plusy()]] * (lv() - 1), colour=NAME, label = NAME)) + geom_point(size=2) + geom_text(hjust = -0.1) + theme(legend.position="none") + labs(y = input$y_as, x = input$x_as)   
    
    
  })
  
  
  output$stats_t <- renderTable({
    
    if(is.null(input$Type)){
      if(is.null(input$Primary)){
        if(is.null(input$Champ)){
          filtered <- Base_stats
        }
      }
    }
    
    if(!is.null(input$Type)){
      i = as.list(input$Type) 
      filtered = NULL
      as.data.frame(filtered)
      while(length(i) > 0){
        filtered <- rbind(filtered, filter(Base_stats, Base_stats$TYPE == i[1]))
        i[1] = NULL
      }
    }
    
    if(!is.null(input$Primary)){
      i = as.list(input$Primary)
      filtered = NULL
      as.data.frame(filtered)
      while(length(i) > 0){
        filtered <- rbind(filtered, filter(Base_stats, Base_stats$PRIMARY == i[1]))
        i[1] = NULL
      }
    }
    
    if(!is.null(input$Champs)){
      i = as.list(input$Champs)
      filtered = NULL
      as.data.frame(filtered)
      while(length(i) > 0){
        filtered <- rbind(filtered, filter(Base_stats, Base_stats$NAME == i[1]))
        i[1] = NULL
      }
    }
    
    #type and primary
    
    
    #type and champ
    
    
    #primary and Champ
    
    
    #primary, champ and type
    
    
    
    filtered
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

