#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
install.packages("dplyr")
library(shiny)
library(leaflet)
library(dplyr)
library(rgdal)
setwd('C:/Users/gmzollicoffer/Documents/R/rShinyProject')
world_spdf <- readOGR(dsn= "C:/Users/gmzollicoffer/Documents/R/rShinyProject/custom", 
                      layer="yShape",
                      verbose=FALSE
)

# TESTING PURPOSES ONLY#################
mybins <- c(0,200,400,1000,5000,20000,50000,100000,200000,400000,Inf)
mypalette <- colorBin(palette="YlOrBr", domain=world_spdf@data$nw_cssQ1_x, na.color="transparent",bin=mybins)

# Prepare the text for tooltips:
mytext <- paste(
  "Country: ", world_spdf@data$NAME,"<br/>", 
  "iso_code: ", world_spdf@data$ISO3, "<br/>", 
  "New Cases for Quarter: ",world_spdf@data$nw_cssQ1_x,"<br/>", 
  "Hospital beds per thousand: ", world_spdf@data$hspt___,"<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

# Final Map
leaflet(world_spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = ~mypalette(world_spdf@data$nw_cssQ1_x), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~world_spdf@data$nw_cssQ1_x, opacity=0.9, title = "Population (M)", position = "bottomleft" )

#MAIN APPLICATION########
###UI####################
ui <- fluidPage(
  titlePanel("Comparison of Quarterly COVID Statistics"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("select", "Select", choices = c("Cases", "Deaths"),selected = "Cases"),
      sliderInput("quarterSlide", "Quarter:",
                  min = 1, max = 4,
                  value = 1)
    ),
    mainPanel(leafletOutput("map"))
  )
)

###############MAIN LEAFLET###############
# Define server logic ----
server <- function(input, output) {
  infectPercentage<- reactive({
    if(input$quarterSlide == 1){
      world_spdf$nw_csQ1/(world_spdf$nw_csQ2+world_spdf$nw_csQ3+world_spdf$nw_csQ4+world_spdf$nw_csQ1)
    }
    else if(input$quarterSlide == 2){
      world_spdf$nw_csQ2/(world_spdf$nw_csQ2+world_spdf$nw_csQ3+world_spdf$nw_csQ4+world_spdf$nw_csQ1)
    }
    else if(input$quarterSlide == 3){
      world_spdf$nw_csQ3/(world_spdf$nw_csQ2+world_spdf$nw_csQ3+world_spdf$nw_csQ4+world_spdf$nw_csQ1)
    }
    else{
      world_spdf$nw_csQ4/(world_spdf$nw_csQ2+world_spdf$nw_csQ3+world_spdf$nw_csQ4+world_spdf$nw_csQ1)
    }
  })
  
  infectQuarter<- reactive({
    if(input$quarterSlide == 1){
      world_spdf$nw_csQ1
    }
    else if(input$quarterSlide == 2){
      world_spdf$nw_csQ2
    }
    else if(input$quarterSlide == 3){
      world_spdf$nw_csQ3
    }
    else{
      world_spdf$nw_csQ4
    }
  })
  stringeRate <- reactive({
    if(input$quarterSlide == 1){
      world_spdf$str_nQ1
    }
    else if(input$quarterSlide == 2){
      world_spdf$str_nQ2
    }
    else if(input$quarterSlide == 3){
      world_spdf$str_nQ3
    }
    else{
      world_spdf$str_nQ4
    }
  })
  dataMap <- reactive({
    if(input$select == "Cases" ){
      if(input$quarterSlide == 1){
        world_spdf$nw_csQ1
      }
      else if(input$quarterSlide == 2){
        world_spdf$nw_csQ2
      }
      else if(input$quarterSlide == 3){
        world_spdf$nw_csQ3
      }
      else{
        world_spdf$nw_csQ4
      }
    }
    else{
      if(input$quarterSlide == 1){
        world_spdf$nw_dtQ1
      }
      else if(input$quarterSlide == 2){
        world_spdf$nw_dtQ2
      }
      else if(input$quarterSlide == 3){
        world_spdf$nw_dtQ3
      }
      else if(input$quarterSlide == 4){
        world_spdf$nw_dtQ4
      }
    }
  })
  output$map <- renderLeaflet({
    mytext <- paste(
      "Country: ", world_spdf@data$NAME,"<br/>", 
      "Quarter Stringency Index: ",round(stringeRate(),2), "<br/>", 
      "New Cases for Quarter: ",infectQuarter(),"<br/>", 
      "Hospital beds per thousand: ", world_spdf$hspt___,"<br/>",
      "Human Development Index: ", world_spdf$hmn_dv_,"<br/>",
      "Percentage out of total year cases:  %",round(100*infectPercentage(),4),
      sep="") %>%
      lapply(htmltools::HTML)
    
    
    mypalette <- colorBin(palette="YlOrBr", domain=dataMap(), na.color="transparent",bin=mybins)
    
    leaflet(world_spdf) %>% 
      addTiles()  %>% 
      setView( lat=10, lng=0 , zoom=1) %>%
      addPolygons( 
        fillColor = ~mypalette(dataMap()), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="white", 
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      )%>%
      addLegend( pal=mypalette, values=~dataMap(), opacity=0.7, title = "Population (Cases/Deaths)", position = "bottomleft" ) 
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)


