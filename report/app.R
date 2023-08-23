library(shiny)
library(rgdal)
library(DT)
library(dygraphs)
library(xts)
library(leaflet)
library(sp)

# ui object

shinyApp(
ui <- fluidPage(
  titlePanel(p("Spatial app", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variableselected",
        label = "Select variable",
        choices = c("VegePercent", "BicPercent","SubDensity","TrafDensity","MedianIncome","Popdensity","WalkSubDist","NeighPercent")
      ),
      p("Made with", a("Shiny",
                       href = "http://shiny.rstudio.com"
      ), ".")
    ),
    mainPanel(
      leafletOutput(outputId = "map")
    )
  )
),

# server()
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    
    # Add data to map
    # Create variableplot
    map$variableplot <- as.numeric(
      map@data[, input$variableselected])
    
    # Create leaflet
    # CHANGE map$cases by map$variableplot
    pal <- colorBin("YlOrRd", domain = map$variableplot, bins = 7)
    
    # CHANGE map$cases by map$variableplot
    labels <- sprintf("%s: %g", map$UHFCODE, map$variableplot) %>%
      lapply(htmltools::HTML)
    
    # CHANGE choices by variableplot
    l <- leaflet(map) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(variableplot),
        color = "white",
        dashArray = "3",
        weight = 1,
        fillOpacity = 0.7,
        label = labels
      ) %>%
      # CHANGE cases by variableplot
      leaflet::addLegend(
        pal = pal, values = ~variableplot,
        opacity = 0.7, title = NULL
      )
  })
}
)
shinyApp(ui = ui, server = server)







