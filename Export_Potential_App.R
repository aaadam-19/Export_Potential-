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
library(stringr)


Exportp_2024 <- read_excel("UK Export potential - Fall 2024 (003).xlsx")%>%
  mutate(Year = 2024)

Exportp_2025 <- read_excel("UK Export potential - Fall 2025.xlsx")%>%
  mutate(Year = 2025)

Combined_Exports <- rbind(Exportp_2024, Exportp_2025)

Combined_Exports <- Combined_Exports %>%
  mutate(HS2 = substr(`Product group code`, 1, 2),
         HS4 = if_else(!str_detect(substr(`Product group code`, 1, 4), "X"),
                       substr(`Product group code`, 1, 4),
                       NA_character_),
         HS6 = if_else(
           !str_detect(substr(`Product group code`, 1, 6), "X"),
           substr(`Product group code`, 1, 6),
           NA_character_)
         )%>%
  rename(Export_potential = `Export potential ($ thousand)`,
         Static_export_potential = `Static export potential ($ thousand)`,
         Dynamic_export_potential = `Dynamic export potential ($ thousand)`,
         Untapped_potential = `Untapped potential ($ thousand)`,
         Static_untapped_potential = `Static untapped potential ($ thousand)`,
         Dynamic_untapped_potential = `Dynamic untapped potential ($ thousand)`,
         Actual_exports = `Actual exports ($ thousand)`)%>%
  mutate(across(7:13, ~ . / 1000))%>%
  mutate(Ratio = Untapped_potential/Export_potential)

######### Exchange Rates and Trade Blocs #############

USD_2024 <- (1.2783)
USD_2025 <- (1.3185)

eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus",
  "Czech Republic", "Denmark", "Estonia", "Finland", "France",
  "Germany", "Greece", "Hungary", "Ireland", "Italy",
  "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
  "Spain", "Sweden"
)

CPTPP_countries <- c(
  "Australia", "Brunei", "Canada", "Chile", "Japan",
  "Malaysia", "Mexico", "New Zealand", "Peru",
  "Singapore", "Vietnam"
)

GCC_countries <- c(
  "Bahrain", "Kuwait", "Oman",
  "Qatar", "Saudi Arabia", "United Arab Emirates"
)

MERCOSUR_countries <- c(
  "Argentina", "Bolivia", "Brazil", "Paraguay", "Uruguay"
)

ASEAN_countries <- c(
  "Brunei", "Cambodia", "Indonesia", "Laos",
  "Malaysia", "Myanmar", "Philippines",
  "Singapore", "Thailand", "Vietnam"
)

EFTA_countries <- c(
  "Iceland", "Liechtenstein",
  "Norway", "Switzerland"
)

USMCA_countries <- c(
  "Canada", "Mexico", "United States"
)

RCEP_countries <- c(
  "Australia", "Brunei", "Cambodia", "China",
  "Indonesia", "Japan", "Laos", "Malaysia",
  "Myanmar", "New Zealand", "Philippines",
  "Singapore", "South Korea", "Thailand",
  "Vietnam"
)

African_Continental_Free_Trade_Area <- c(
  "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso",
  "Burundi", "Cameroon", "Cape Verde", "Central African Republic",
  "Chad", "Comoros", "Congo", "Democratic Republic of the Congo",
  "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea",
  "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana",
  "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya",
  "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi",
  "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique",
  "Namibia", "Niger", "Nigeria", "Rwanda",
  "Sao Tome and Principe", "Senegal", "Seychelles",
  "Sierra Leone", "Somalia", "South Africa", "South Sudan",
  "Sudan", "Tanzania", "Togo", "Tunisia",
  "Uganda", "Zambia", "Zimbabwe"
)

############ Map Data  ##############

Map_data <- Combined_Exports %>%
  group_by(Year, `Sub-region`) %>%
  summarise(
    export_potential = sum(Export_potential, na.rm = TRUE),
    .groups = "drop")

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

dbt_regions_sf <- dbt_regions_sf %>%
  st_make_valid() %>%
  st_wrap_dateline(options = c("WRAPDATELINE=YES"))


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
         
       div(
         style = "width: 300px; margin-bottom: 15px;",
         
         sliderInput(
           "year",
           "Year",
           min = 2024,
           max = 2025,
           value = 2025,
           step = 1,
           sep = ""
         )
       ),
           
       leafletOutput("export_map" ,width = "100%" ,height = "700px"),
       
       br(),
       
       tabItem(
         tabName = "trading_blocs",
         
         fluidRow(
           box(
             width = 3,
             
             sliderInput(
               "bloc_year",
               "Year",
               min = min(Combined_Exports$Year),
               max = max(Combined_Exports$Year),
               value = max(Combined_Exports$Year),
               step = 1,
               sep = ""
             )
           )
         ),
         
         fluidRow(
           box(
             width = 12,
             plotOutput("bloc_chart", height = "600px")
           )
         )
       )
       ) #close div
  
) #close UI

server <- function(input, output, session) {
  
  ############ MAP TAP ####################
  
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
      addTiles()%>%
      setView(
        lng = 10,
        lat = 40,
        zoom = 2.2
      )
    
  })
  
  observe({
    
    df <- map_data()
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = log10(df$export_potential + 1),
      na.color = "#F0F0F0"
    )
    
    
    leafletProxy("export_map", data = df) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(log10(export_potential + 1)),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        popup = ~paste0(
          "<b>", DBT_Region, "</b><br>",
          "Export Potential: $",
          scales::comma(round(export_potential)),
          " million"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~export_potential,
        title = "Export Potential ($m)"
      )
    
  })
  
  #############  BLOC ####################
  
  bloc_data <- reactive({
    
    Combined_Exports %>%
      mutate(
        Trading_Bloc = case_when(
          Market %in% eu_countries ~ "EU",
          Market %in% CPTPP_countries ~ "CPTPP",
          Market %in% GCC_countries ~ "GCC",
          Market %in% MERCOSUR_countries ~ "MERCOSUR",
          Market %in% ASEAN_countries ~ "ASEAN",
          Market %in% EFTA_countries ~ "EFTA",
          Market %in% USMCA_countries ~ "USMCA",
          Market %in% RCEP_countries ~ "RCEP",
          Market %in% African_Continental_Free_Trade_Area ~ "AfCFTA",
          TRUE ~ "Other"
        )
      ) %>%
      filter(Year == input$bloc_year) %>%
      group_by(Trading_Bloc) %>%
      summarise(
        Value = sum(Export_potential, na.rm = TRUE),
        .groups = "drop"
      )%>%
      arrange(desc(Value))
    
  })
  
  group_colors <- c(
    "EU" = "#12436D",
    "GCC" = "#28A197",
    "MERCOSUR" = "#801650",
    "CPTPP" = "#F46A25",
    "ASEAN" = "#3D3D3D",
    "EFTA" = "#A285D1",
    "USMCA" = "#2B8CC4",
    "RCEP" = "#76B7B2",
    "AfCFTA" = "#49B090",
    "Other" = "#B1B4B6"
  )
  
  
  output$bloc_chart <- renderPlot({
    
    ggplot(bloc_data(),
           aes(x = reorder(Trading_Bloc, -Value),
               y = Value,
               fill = Trading_Bloc))+
      geom_col(width = 0.8) +
      scale_fill_manual(values = group_colors)+
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = NULL,
        y = "Export Potential ($m)",
        fill = "Trading Bloc"
      ) +
      theme_minimal()+
        theme(
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    
  })
  
  
}

shinyApp(ui = ui, server = server)