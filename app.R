source("helpers.R")


ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "sandstone",
  ),
  
  titlePanel("Quel nom de famille dans quelle commune en Suisse"),
  
  fluidRow(
    
    column(1:2,
           textInput(
             inputId = 'name',
             label = 'Saisir un nom')),
    column(2:9,
           plotOutput('graph_1'),
    column(2:9,
           plotOutput('graph_2'))
    )
  )
)




server <- function(input, output) {
  output$graph_1 <- renderPlot({
    
    map_name(data = data, 
                   name = input$'name')
  }
  )
  output$graph_2 <- renderPlot({
    
    top_10(data = data,
           name = input$'name')
  }
  )
}

shinyApp(ui = ui, server = server)





