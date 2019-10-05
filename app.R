#load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

#import data
data <- read.csv("earthquake_map.csv")

#categorize earthquake depth
data$depth_type <- ifelse(data$depth <= 70, "shallow", ifelse(data$depth <= 300 | data$depth >70, "intermediate", ifelse(data$depth > 300, "deep", "other")))

#ui function
ui <- fluidPage(
  mainPanel( 
    leafletOutput(outputId = "mymap"), 
    absolutePanel(top = 60, left = 20, 
                  checkboxInput("markers", "Depth", FALSE),
                  checkboxInput("heat", "Heatmap", FALSE)
    )
  ))

#server function
server <- function(input, output, session) {
  pal <- colorNumeric(
    palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
    domain = data$mag)
  
  pal2 <- colorFactor(
    palette = c('blue', 'yellow', 'red'),
    domain = data$depth_type
  )
  
  output$mymap <- renderLeaflet({
    leaflet(data) %>% 
      setView(lng = 134, lat = -6, zoom = 4)  %>% 
      addTiles() %>% 
      addCircles(data = data, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~sqrt(mag)*25000, popup = ~as.character(mag), label = ~as.character(paste0("Magnitude: ", sep = " ", mag)), color = ~pal(mag), fillOpacity = 0.5)
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = data)
    proxy %>% clearMarkers()
    if (input$markers) {
      proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(depth_type), fillOpacity = 0.2,  label = ~as.character(paste0("Magnitude: ", sep = " ", mag))) %>%
        addLegend("bottomright", pal = pal2, values = data$depth_type,
                  title = "Depth Type",
                  opacity = 1)}
    else {
      proxy %>% clearMarkers() %>% clearControls()
    }
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = data)
    proxy %>% clearMarkers()
    if (input$heat) {
      proxy %>%  addHeatmap(lng=~longitude, lat=~latitude, intensity = ~mag, blur =  10, max = 0.05, radius = 15) 
    }
    else{
      proxy %>% clearHeatmap()
    }
  })
}

shinyApp(ui, server)
