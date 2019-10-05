library(shiny)

ui <- fluidPage(
  mainPanel( 
    leafletOutput(outputId = "mymap"), 
    absolutePanel(top = 60, left = 20, 
                  checkboxInput("markers", "Depth", FALSE),
                  checkboxInput("heat", "Heatmap", FALSE)
    )
  ))