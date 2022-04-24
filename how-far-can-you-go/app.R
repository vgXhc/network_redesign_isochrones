#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(sf)
library(tmap)
bike_isochrones <- readRDS("data/bike_isochrones.RDS")
tmap_mode("view")
tmap_options(check.and.fix = TRUE)

start_points <- bike_isochrones
bike_isochrones <- bike_isochrones %>% 
  mutate(time_formatted = as.factor(time/60)) %>% 
  st_set_geometry(., "iso") %>% 
  st_make_valid()

tmapIcon <- tmap_icons("https://raw.githubusercontent.com/Rush/Font-Awesome-SVG-PNG/master/black/png/48/map-marker.png")




start_locations <- bike_isochrones %>%
  st_drop_geometry() %>% 
  distinct(name) %>% 
  arrange(name) %>% 
  pull()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("How far can you go?"),

    # Sidebar with input for start location mode 
    sidebarLayout(
        sidebarPanel(
            radioButtons("mode_input",
                         "Bike or e-bike?",
                         c("Bike" = "bike", 
                           "E-bike" = "e-bike")),
            selectInput(
              "location_input",
              "Select start location",
              start_locations,
              multiple = F,
              selected = "Allied at Lovell"
            ),
            p(
              "Metro is ", a("redesigning their bus network.", href = "https://www.cityofmadison.com/metro/routes-schedules/transit-network-redesign"), "The goal is to create a more efficient system, with more frequent service and a less complicated network. However, since the budget is fixed, more frequent service means that some areas currently served by transit will have less or no service."
            ),
            p(
              "To show the impact of the proposed network, Metro published maps that show how far you would be be able to get by bus within 45 minutes (including the walk to the nearest stop and waiting time) for a number of locations in the city. The maps show access both before and after the redesign"
            ),
            p(
              "Trips on a bike or e-bike may be an option for some pepole on some trips. How far can you get from those same locations by bike or e-bike, in 15, 30, or 45 minutes? This app will tell you."
            ),
            p(
              "Bus maps: City of Madison/Jarrett Walker Associates", 
              br(), 
              "Bike maps:", a("OpenRouteService", href="https://openrouteservice.org/"), 
              br(), 
              "App:", a("Harald Kliems", href="https://haraldkliems.netlify.app")
        )
        ), 

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
            column(6,
          h1("How far can you get by bus:"),
          imageOutput("busMap",
                      #width = "100%",
                      inline = F)
            ),
          column(6,
          h1(textOutput("mode")),
          tmapOutput("bikeMap",
                     width = "100%")
        ))
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$busMap <- renderImage({
    #generate filename
    outfile <- paste0("img/", input$location_input, ".png")
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = "40%",
         alt = "A map showing how far you can get in 45 minutes by bus")
    }, deleteFile = F)
  
  output$bikeMap <- renderTmap({
    bike_isochrones %>% 
      filter(bike_type == input$mode_input & name == input$location_input) %>% 
      tm_shape(unit = "imperial") +
      tm_polygons(col = "time_formatted",
                  title = "Area reachable within time (minutes)",
                  alpha = .2) +
    tm_basemap(leaflet::providers$Stamen.TonerLite) +
      tm_scale_bar() +
      tm_view(set.view = 12) +
      tm_shape(start_points %>% filter(bike_type == input$mode_input & name == input$location_input)) +
      tm_symbols(shape = tmapIcon)
  }
  )
  output$location <- renderText({
    input$location_input
    })
  
  output$mode <- renderText({
    paste0("How far you can get by ", input$mode_input, ":")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
