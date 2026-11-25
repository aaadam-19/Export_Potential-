library(tidyverse)
library(readxl)
library(afcharts)
library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)


Exportp_2024 <- read_excel("UK Export potential - Fall 2024 (003).xlsx")%>%
  mutate(Year = 2024)

Exportp_2025 <- read_excel("UK Export potential - Fall 2025.xlsx")%>%
  mutate(Year = 2025)

Combined_Exports <- rbind(Exportp_2024, Exportp_2025)

USD_2024 <- (1.2783)
USD_2025 <- (1.3185)

############ Map Data  ##############

Map_data <- Combined_Exports %>%
  group_by(Year, `Sub-region`) %>%
  summarise(
    export_potential = sum(`Export potential ($ thousand)`, na.rm = TRUE),
    .groups = "drop"
  )

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

world <- world %>%
  mutate(
    DBT_Region = case_when(
      
      subregion == "Caribbean" ~ "Caribbean",
      
      subregion == "Northern America" ~ "North America",
      
      subregion %in% c(
        "Central America",
        "South America"
      ) ~ "South & Central America",
      
      subregion %in% c(
        "Western Europe",
        "Northern Europe",
        "Southern Europe"
      ) ~ "EU & West Europe",
      
      subregion %in% c(
        "Eastern Europe",
        "Central Asia"
      ) ~ "East Europe & Central Asia",
      
      subregion == "Eastern Asia" ~ "East Asia",
      
      subregion == "South-Eastern Asia" ~ "Southeast Asia",
      
      subregion == "Southern Asia" ~ "South Asia",
      
      subregion == "Western Asia" ~ "Middle East",
      
      subregion == "Eastern Africa" ~ "Eastern Africa",
      
      subregion == "Middle Africa" ~ "Central Africa",
      
      subregion == "Western Africa" ~ "Western Africa",
      
      subregion == "Northern Africa" ~ "Northern Africa",
      
      subregion == "Southern Africa" ~ "Southern Africa",
      
      subregion %in% c(
        "Australia and New Zealand",
        "Melanesia",
        "Micronesia",
        "Polynesia"
      ) ~ "Pacific",
      
      TRUE ~ NA_character_
    )
  )

dbt_regions_sf <- world %>%
  filter(!is.na(DBT_Region)) %>%
  group_by(DBT_Region) %>%
  summarise() %>%
  st_make_valid()


###################  APP ######################
custom_value_box <- function(title, value) {
  div(class = "value-box",
      div(class = "value-box-title", title),
      div(class = "value-box-value", value)
  )
}

ui <- fluidPage(
  
  # Custom CSS for styling
  tags$head(
    tags$style(HTML("

      body {
        background: #f8f9fb;
      }
      
      /* Hero section */
      
      .hero {
      text-align: center;
      padding: 60px 40px;
      margin: 30px auto 40px auto;
      background: white;
      border: 3px solid #600475;
      border-radius: 24px;
      max-width: 1200px;
      box-shadow: 0 2px 12px rgba(0,0,0,0.06);
      }
      
      .hero-title {
      font-size: 3rem;
      font-weight: 700;
      color: #1f2937;
      margin-bottom: 15px;
      }
      
      .hero-subtitle {
      font-size: 1.5rem;
      line-height: 1.7;
      color: #6b7280;
      max-width: 900px;
      margin: 0 auto;
      }
      
      
      /* Analysis containers */
      
      .analysis-card {
      background: white;
      border: 3px solid #600475;
      border-radius: 24px;
      padding: 30px;
      margin: 0 auto 30px auto;
      max-width: 1200px;
      box-shadow: 0 2px 12px rgba(0,0,0,0.06);
      }
      
      .section-title {
      font-size: 2.5rem;
      font-weight: 600;
      color: #1f2937;
      margin-bottom: 10px;
      padding-left: 15px;
      border-left: 5px solid #600475;
      }
      
      .section-description {
      color: #6b7280;
      font-size: 1.5rem;
      line-height: 1.6;
      margin-bottom: 25px;
      }
      
      .subsection-title {
      font-size: 1.8rem;
      font-weight: 600;
      color: #1f2937;
      margin: 25px 0 15px 0;
      padding-left: 12px;
      border-left: 4px solid #600475;
      }
      
      /* Inputs */
      
      .control-section {
      margin-bottom: 25px;
      }
      
      .value-box {
      background: white;
      border: 3px solid #600475;
      border-radius: 16px;
      padding: 20px 25px;
      text-align: center;
      box-shadow: 0 2px 12px rgba(0,0,0,0.06);
      }

      .value-box-title {
      font-size: 1.5rem;
      color: #6b7280;
      margin-bottom: 8px;
      }

      .value-box-value {
      font-size: 2.2rem;
      font-weight: 700;
      color: #012169;
      } 

      /* Footer section styles */
      
      .footer-section {
        margin-top: 50px;
        padding-top: 30px;
        border-top: 3px solid #012169;
      }
      
      .footer-content {
        color: #666;
        font-size: 0.9em;
        line-height: 1.6;
      }
      
      .footer-heading {
        color: #012169;
        font-weight: 600;
        margin-bottom: 15px;
      }
      "))
  ),
  
  div(
    class = "hero",
    h1(class = "hero-title",
       "UK Export Potential"
    ),
    
    p(
      class = "hero-subtitle",
      "The aim of this dashboard is to monitor the impact of increasing imports from Australia and New Zealand on UK beef production and prices. "
    )
  ),
  
  div( ### Title and description ####
       class = "analysis-card",
       h2(class = "section-title",
          "Map of Export potential"),
       p(class = "section-description",
         "Insert descriptive text"),
       
       ui <- fluidPage(
         
         sidebarLayout(
           sidebarPanel(
             sliderInput(
               "year",
               "Year",
               min = 2024,
               max = 2025,
               value = 2024,
               step = 1,
               sep = ""
             )
             
           ),
           
           mainPanel(
             
             leafletOutput("export_map", height = 700)
             
           )
           
         )
         
       )

       )
  
) #close UI

server <- function(input, output, session) {
  
  map_filtered <- reactive({
    Map_data %>%
      filter(Year == input$year)
  })
  
  map_data <- reactive({
    
    dbt_regions_sf %>%
      left_join(
        map_filtered(),
        by = c("DBT_Region" = "Sub-region")
      )
  })
  
  output$export_map <- renderLeaflet({
    
    leaflet(dbt_regions_sf) %>%
      addProviderTiles("CartoDB.Positron")
    
  })
  
  observe({
    
    df <- map_data()
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = df$export_potential,
      na.color = "#F0F0F0"
    )
    
    leafletProxy("export_map", data = df) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(export_potential),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        popup = ~paste0(
          "<b>", DBT_Region, "</b><br>",
          "Export Potential: $",
          scales::comma(round(export_potential)),
          " thousand"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~export_potential,
        title = "Export Potential ($000)"
      )
    
  })
  
}

shinyApp(ui = ui, server = server)