library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(leaflet)
library(leaflet.esri)
library(bslib)
library(dplyr)
library(leaflet.extras)
library(DT)
library(plotly)
library(sf)

# TODO Value boxes


rainfall_data <- readRDS("data/cleaned_rain_data.rds")
columbia_county_boundary <- readRDS("data/columbia_county_boundary.rds")
rainfall_data_monthly_means <- readRDS("data/monthly_means.rds")
annual_rain_raster <- readRDS("data/annual_rain_raster.rds")

rainfall_data_spatial <- rainfall_data_monthly_means %>% 
filter(!is.na(LAT)) 

# raster palette
# pal <- colorNumeric(
#   palette = "Blues",  
#   domain = terra::values(annual_rain_raster),  
#   na.color = "transparent" 
# )
# Columbia County boundaries
columbia_bounds <- list(
  min_lat = 46.2,
  max_lat = 46.5,
  min_lon = -118.1,
  max_lon = -117.6
)

ui <-page_sidebar(
  tags$style(HTML("
  .bslib-nav { 
    margin-top: 10px; /* Add spacing above nav panel */
  }
  
  .value-box { 
    padding-top: 10px; /* Add padding to value boxes */
  }
")),
  shinyjs::useShinyjs(),
 title = "Columbia County Rainfall",
  theme = bs_theme(bootswatch = "minty"),
      sidebar = sidebar(title = "Marked Location:",
                        #p("Local Rain Gauge"),
                        #textOutput("local_name"),
                        #p("Map Click"),
                        textOutput("coords"),
              shinyjs::hidden(
              shinyWidgets::actionBttn(color = "warning",size = "xs",style = "pill",
                                         "clear_marker","Clear Marker & Data")),
                        
                        width = '230px',

          shinyjs::hidden(bslib::accordion(id="data_filters",
                                           open = FALSE,
        accordion_panel(
        title = "Data Filters",
        shinyWidgets::pickerInput("variables_query", "Extra Variable(s):", 
        choices = list("Precip. Prob. Max"="precipitation_probability_max",
                       "Min Temp"="temperature_2m_min",
                       "Max Wind Speed"="wind_speed_10m_max",
                       "Precip Hours"="precipitation_hours",
                       "Radiation Sum"="shortwave_radiation_sum"),
        label= "Parameters",
        selected = NULL, multiple = TRUE),
        p("Date Range:"),
        dateInput("start_date", "Start Date:", value = "2020-01-01", format = "yyyy-mm-dd"),
        dateInput("end_date", "End Date:", value = "2024-12-31", format = "yyyy-mm-dd"),
        shinyWidgets::prettyCheckboxGroup("rain_units",label = "Rain Units",
                                          selected = "inch",choices = c("mm","inch"))
        ))),
        shinyjs::hidden(
          shinyWidgets::actionBttn(color = "success","fetch_api","Get Data")
        ),
        shinyWidgets::actionBttn(size = "xs",style = "pill","guidance","help"),
        ), navset_tab(id = "main_tabs",
        nav_panel(
          title = "Map",
        page_fillable(
      # Hide until API data are loaded
          shinyjs::hidden(
            div(
              id = "value_boxes_api",
              layout_columns(
                value_box(
                  max_height = "110px",
                  title = "Rain",
                  value = div(style = "font-size: 16px; font-weight: bold;", textOutput("rain_value")), # Larger text size
                  theme = NULL,
                  showcase = bsicons::bs_icon("cloud-rain-fill", style = "font-size: 14px; color: black;"), # Smaller black icon
                  showcase_layout = "left center",
                  full_screen = FALSE,
                  fill = FALSE,
                  height = 100
                ) %>% withSpinner(),
                
                value_box(
                  max_height = "110px",
                  title = "Snow",
                  value = div(style = "font-size: 16px; font-weight: bold;", textOutput("snow_value")), # Larger text size
                  theme = NULL,
                  showcase = bsicons::bs_icon("cloud-snow-fill", style = "font-size: 14px; color: black;"), # Smaller black icon
                  showcase_layout = "left center",
                  full_screen = FALSE,
                  fill = FALSE,
                  height = 100
                ),
                
                value_box(
                  max_height = "110px",
                  title = "Total Precipitation",
                  value = div(style = "font-size: 16px; font-weight: bold;", textOutput("total_precip_value")), # Larger text size
                  theme = NULL,
                  showcase = bsicons::bs_icon("moisture", style = "font-size: 14px; color: black;"), # Smaller black icon
                  showcase_layout = "left center",
                  full_screen = FALSE,
                  fill = FALSE,
                  height = 100
                ),
                pickerInput(
                  inputId = "month_picker",
                  label = "Select Months:",
                  choices = month.abb, 
                  selected = month.abb,
                  multiple = TRUE,    
                  options = list(
                    container = "body",
                    `actions-box` = TRUE,   
                    `live-search` = TRUE,
                    `selected-text-format`= "count"
                  )
                ),
              ))
          ),
            
          card(
            card_header(textOutput("dates")),
            leafletOutput("map"),min_height = 500)
        
        
    )
  ),
  nav_panel(
    title = "Local Station Data",
card(
shiny::fixedRow(
  pickerInput(
  inputId = "local_year_picker",
  label = "Years",
  choices = unique(rainfall_data$YEARS),
  selected = unique(rainfall_data$YEARS),
  multiple = TRUE,    
  options = list(
    `actions-box` = TRUE,   
    `live-search` = TRUE ,
    `selected-text-format`= "count"
  )
),
  pickerInput(
    inputId = "local_station_picker",
    label = "Station",
    choices = unique(rainfall_data$STATION),
    selected = unique(rainfall_data$STATION),
    multiple = TRUE,    
    options = list(
      `actions-box` = TRUE,   
      `live-search` = TRUE ,
      `selected-text-format`= "count"
    )
  )
),
fill = TRUE,min_height = 800, 
        DTOutput("points_table")
        )

     # )
   # )
  ),
  nav_panel(
    value="map_click_data_tab",
    title = "Map Click Data",
    card(fill = TRUE,
    card_header(textOutput("coords2")),
    card_body(min_height = 800,
    DTOutput("apifetch")
     
  ))),
  nav_panel(
    value="map_click_summary_tab",
    title = "Map Click Summary",
    layout_columns(
      card(
        card_header("Monthly Distribution"),
        DTOutput(height=500,"monthly_precip_table")
        ),
      card(
        card_header("Yearly Distribution"),
        DTOutput("yearly_precip_table")
        )
      ),
    card(full_screen = TRUE,
      card_header("Daily"),
      plotlyOutput("daily_map_click_data")
      )
    )
  )
)

server <- function(input, output, session) {
  
  
  # Create reactive values
  search_message <- reactiveVal("")
  selected_station <- reactiveVal(NULL)
  station_info <- reactiveVal(NULL)
  vals <- reactiveValues(lat = NULL, lon = NULL, data = NULL)
  local <- reactiveValues(name = NULL)
  
  # Capture map click coordinates
  observeEvent(input$map_click, {
  
  req(!"stations" %in% leaflet_selected$groups)  
  if (is.null(vals$lat) && is.null(vals$lon)) {
    click <- input$map_click
    vals$lat <- click$lat
    vals$lon <- click$lng
    
    req(input$map_click)
    print(vals$lat)
    print(vals$lon)
  }
    })
  
# Reactive value to track whether a station is hovered over
# prevent adding points when trying to click local markers
  
is_hovering_station <- reactiveVal(FALSE)


  observe({
    req(!is.null(vals$data))
    shinyjs::show("value_boxes_api")
  })
  
  observe({
    if (is.null(vals$data)) {
      # Hide tabs if data is NULL
      nav_hide("main_tabs", target = "map_click_data_tab")
      nav_hide("main_tabs", target = "map_click_summary_tab")
    } else {
      # Show tabs if data is available
      nav_show("main_tabs", target = "map_click_data_tab")
      nav_show("main_tabs", target = "map_click_summary_tab")
    }
  })
  
    
   

  
  # Capture map click coordinates
  observeEvent(input$clear_marker, {
    vals$lat <- NULL
    vals$lon <- NULL
    vals$data <- NULL
    
    leafletProxy("map") %>%
    clearGroup("vals") 
    
    shinyjs::hide("value_boxes_api")  
    shinyjs::hide("clear_marker") 
    shinyjs::hide("fetch_api")
    shinyjs::hide("data_filters")
    #shinyjs::hide("map_click_data_tab")
    #shinyjs::hide("map_click_summary_tab")
    })
  
  observe({
    
    req(!is.null(vals$lat))
    shinyjs::show("clear_marker") 
    shinyjs::show("data_filters")
    
    req(is.null(vals$data))
    shinyjs::show("fetch_api")
    
  
    })
  
  output$dates <- renderText({
    
    req(!is.null(input$start_date) && !is.null(input$end_date))
    req(!is.null(vals$data))
    paste("Start:",input$start_date,"End:",input$end_date)
    
  })
  
  # Display captured coordinates
  output$coords <- renderText({
    if (is.null(vals$lat) || is.null(vals$lon)) {
      # Display a default message when vals$lat or vals$lon is NULL
      return("Click a spot on the map to set the location of interest.")
    }
    
    # Otherwise, display the coordinates
    paste("Latitude:", round(vals$lat, 4), "Longitude:", round(vals$lon, 4))
  
    })
  
  output$coords2 <- renderText({
   
 req(length(input$month_picker)<12)

   paste(
   c("Filtered to: ",input$month_picker),
   collapse = ",")
    
  })
  
  # Fetch data from Daymet API on button click
  observeEvent(input$fetch_api, {
    
    vals$data <- NULL
    
    req(vals$lat, vals$lon)  # Ensure lat/lon are available
    
    required <- c("snowfall_sum","rain_sum","showers_sum","temperature_2m_max")
    
    variables_query <- paste(
     c(required,input$variables_query),
     collapse = ",")

    # Build API URL Day Met
    # api_url <- paste0(
    #     "https://daymet.ornl.gov/single-pixel/api/data?",
    #     "lat=", vals$lat,
    #     "&lon=", vals$lon,
    #     "&vars=", tolower(variables_query),
    #     "&start=", input$start_date,
    #     "&end=", input$end_date
    #   )
    
    # API open-meteo
    api_url <- paste0("https://archive-api.open-meteo.com/v1/archive?",
                      "latitude=", vals$lat,
                      "&longitude=", vals$lon,
                      "&start_date=",input$start_date,
                      "&end_date=", input$end_date,
                      "&daily=", variables_query,
                      "&precipitation_unit=",input$rain_units
    )
    
    
    
    print(api_url)
    
    # Fetch the data
    response <- httr::GET(api_url)
    if (response$status_code == 200) {
      # Parse the CSV response
      #content <- httr::content(response, as = "raw")
      content <-  jsonlite::fromJSON(httr::content(response, as = "text"))
      
      vals$data <- as.data.frame(content$daily) %>%
              mutate(date = lubridate::date(time),
               month=lubridate::month(date,label = TRUE),
               year=lubridate::year(date)) %>%
        group_by(date) %>%
        mutate(
        snowfall_sum=round(snowfall_sum/2.54,3), 
        precipitation_sum=round(sum(`snowfall_sum`,`rain_sum`,na.rm = TRUE),3)) %>%
        ungroup() %>%
        select(-time)
        
       print(head(vals$data))
      # vals$data <- readr::read_csv(content,skip = 6) %>%
      #   mutate(date = lubridate::date(lubridate::ymd(paste0(year, "-01-01")) + lubridate::days(yday - 1)))%>%
      #   tibble::tibble() %>%
      #   mutate(month=lubridate::month(date,label = TRUE))
    } else {
      showNotification("Failed to fetch data. Check API or coordinates.", type = "error")
      vals$data <- NULL
    }
  })
  

  
  # Display fetched data
  output$apifetch <- renderDT({
    req(vals$data)
     datatable(vals$data %>%
     filter(month %in% input$month_picker),
     height = "100%",
      extensions = 'Buttons', 
      options = list(
        dom = 'frltipB', 
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 25 
      )
      )
  })
  # Function to find nearest station
  # find_nearest_station <- function(click_lat, click_lng) {
  #   distances <- sqrt((station_data$LAT - click_lat)^2 + 
  #                       (station_data$LONG - click_lng)^2)
  #   nearest_idx <- which.min(distances)
  #   station_data[nearest_idx, ]
  # }
  
  output$search_message <- renderText({
    search_message()
  })
  
  
  # Initialize the map
  output$map <- renderLeaflet({
    leaflet(rainfall_data_spatial) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(group="stations",
        ~LONG, ~LAT,
        layerId = ~STATION,
        label =  ~STATION,
        radius = 8,
        color = "blue",
        fillOpacity = 0.7,
        #popup = ~paste(STATION, "<br>", ADDRESS, "<br>Avg Rainfall:", round(MEAN_VALUE, 1), "mm")
      ) %>%
      leaflet.esri::addEsriDynamicMapLayer(
        url = "https://webgis.dor.wa.gov/arcgis/rest/services/Base/WADOR_Base_PLSS/MapServer",
        group = "SectTownRange",
        options = tileOptions(opacity = 0.8) # Adjust opacity if needed
      ) %>%
      addPolygons(data=columbia_county_boundary,fill = FALSE,color = "black",stroke = "black")%>%
      addSearchOSM() %>%
      setView(lng = -117.9074, lat = 46.29717, zoom = 9) %>%
      addWMSTiles(
        baseUrl = "https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WMSServer?",
        layers = "0",
        options = leaflet::WMSTileOptions(
          format = "image/png32",
          version = "1.3.0",
          minZoom = 3,
          maxZoom = 16,
          transparent = TRUE
        ),group = "Waterways"
      )%>%
      addWMSTiles(group = "fire.MODIS",
        baseUrl = "https://firms.modaps.eosdis.nasa.gov/mapserver/wms/fires/88be7e789472786a1a8032a9330a65fc/",
        layers = "fires_modis_24", 
        options = WMSTileOptions(format = "image/png", transparent = TRUE), # Tile format and transparency
        attribution = "NASA FIRMS MODIS" # Attribution for the data source
      ) %>%
      addProviderTiles("Esri.WorldGrayCanvas", group="Gray") %>%
      addProviderTiles("Esri.WorldImagery", group="Imagery") %>%
      addProviderTiles("CartoDB.DarkMatter", group="Dark") %>%
      addProviderTiles("Esri.NatGeoWorldMap", group="Topo") %>%
      addProviderTiles("OpenStreetMap", group="Street")%>%
      leaflet.extras::addFullscreenControl() %>%
      addLayersControl(
        overlayGroups = c("Waterways","stations","selected","SectTownRange","fire.MODIS"),
        baseGroups = c("Topo","Imagery", "Dark", "Street","Gray")
      ) %>%
      hideGroup("Annual") %>%
      hideGroup("SectTownRange") %>%
      hideGroup("fire.MODIS") %>%
      hideGroup("stations") %>%
      addControl(
        html = tags$style(
          HTML("
            .leaflet-container {
              cursor: crosshair; /* Change cursor to crosshairs */
            }
          ")
        ),
        position = "topright",
        className = "hidden-control"
      ) 
  })
  
  leaflet_selected <- reactiveValues(groups=NULL)
  
  # Display selected leaflet groups in the UI
  observe({
    print(input$map_groups)
    leaflet_selected$groups <- input$map_groups   
  })
  
  # Create the data table
  output$points_table <- renderDT({
    
  req(nrow(rainfall_data)>=1)
    
    DT::datatable(data = 
      rainfall_data %>% 
      dplyr::select(YEARS,STATION,ADDRESS,SEPT:AUG,TOTAL) %>%
      filter(STATION %in% input$local_station_picker) %>%
      filter(YEARS %in% input$local_year_picker),
      height = "100%",
      extensions = 'Buttons', # Enable Buttons extension
      options = list(
        dom = 'frltipB', # Layout: Buttons, Filtering, Table, and Pagination
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), # Add buttons
        pageLength = 25 # Show 10 rows per page
      )
    )
      
  })
  
  # Update map markers when a station is selected
  observe({
    selected <- selected_station()
    if (!is.null(selected)) {
      leafletProxy("map") %>%
        clearGroup("selected") %>%
        addCircleMarkers(
          data = rainfall_data_spatial[rainfall_data_spatial$STATION == selected, ],
          lng = ~LONG, 
          lat = ~LAT,
          radius = 8,
          color = "red",
          fillOpacity = 0.7,
          group = "selected"
        )
    }
  })
  
  

  
  observe({
 
    input$clear_markers
    
    if (!is.null(vals$lat)) {
      
      leafletProxy("map") %>%
        clearGroup("vals") %>%
        addMarkers(
          lng = vals$lon,
          lat = vals$lat,
          group = "vals"
        )
    }
  })
  
  # Handle map clicks
  # observeEvent(input$map_click, {
  #   click <- input$map_click
  #   nearest <- find_nearest_station(click$lat, click$lng)
  #   station_info(nearest)
  #   selected_station(nearest$STATION)
  # })
  
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    clicked_station <- rainfall_data_spatial[rainfall_data_spatial$STATION %in% click$id, ]
    station_info(clicked_station)
    selected_station(click$id)
    local$name <- clicked_station$STATION %>%
    unique()
    
    
    output$local_name <- renderText({
      req(nrow(station_info())>1)
      local$name
    })
    
    output$station_info <- renderTable({
      req(nrow(station_info())>1)
      station <- station_info()
      station %>%
        dplyr::select(MONTH,MEAN_VALUE)
    })
    
    # output$total <- renderText({
    #   req(nrow(station_info())>1)
    #   station <- station_info()
    #    station %>%
    #     dplyr::select(MONTH,MEAN_VALUE) %>%
    #     mutate(Overall=mean(MEAN_VALUE))
    # })
    
    output$station_title <- renderTable({
      req(nrow(station_info())>1)
      station <- station_info()
      station %>%
        mutate(AVG=mean(MEAN_VALUE))   %>%
        dplyr::select(STATION,ADDRESS,AVG) %>%
        distinct() 
    })
    
    output$rainfall_boxplot <- renderPlotly({
      station <- station_info()
      if (is.null(station)) {
        return(NULL)
      }
      
      # Reshape monthly data for boxplot
      plot_ly(rainfall_data_monthly_means, x = ~MONTH, y = ~MEAN_VALUE, type = "box") %>%
        layout(
          title = paste("Monthly Rainfall Distribution for", rainfall_data_monthly_means$STATION),
          xaxis = list(title = "Month"),
          yaxis = list(title = "Rainfall (in)")
        )
    })
    
    # Rainfall trend plot
    output$rainfall_trend <- renderPlotly({
      station <- station_info()
      if (is.null(station)) {
        return(NULL)
      }
      
      yearly_data <- rainfall_data %>%
        filter(STATION %in% station$STATION)
      
      plot_ly(yearly_data, x = ~YEARS, y = ~TOTAL, type = 'scatter', mode = 'lines+markers') %>%
        layout(
          title = paste("Yearly Rainfall Trend for", station$STATION),
          xaxis = list(title = "Year"),
          yaxis = list(title = "Rainfall (in)")
        )
    })
    
    showModal(
      modalDialog(size = "xl",
        title = "Precipitation",
        tableOutput("station_title"),
        bslib::layout_columns(
        card(width=8,plotlyOutput(session$ns("rainfall_trend")),
        card(plotlyOutput(session$ns("rainfall_boxplot")))
        )),    
        tableOutput("station_info"),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })

  observeEvent(input$guidance,{
    
    showModal(
      modalDialog(size = "xl",
                  title = "guidance",
                  bslib::accordion(open = TRUE,
                                   accordion_panel(title = "Guidance",
                                                   value_box(
                                                     title = "For new locations:", value = "Add Map Points",
                                                     shiny::markdown("Click any point on the map within Columbia County. Choose parameters of interest and date range https://open-meteo.com/"),
                                                     theme = value_box_theme(bg = "#FFFFFF", fg = "#000000"),
                                                     showcase = fontawesome::fa_i("map-location-dot"), showcase_layout = "left center",
                                                     full_screen = FALSE, fill = TRUE, height = NULL
                                                   ),
                                                   value_box(
                                                     title = "For Existing Rain Stations:", value = "Select Points",
                                                     shiny::markdown("Click on \"Stations\" layer on the map. Click on any station to view data"),
                                                     theme = value_box_theme(bg = "#FFFFFF", fg = "#000000"),
                                                     showcase = fontawesome::fa_i("droplet"), showcase_layout = "left center",
                                                     full_screen = FALSE, fill = TRUE, height = NULL
                                                   ))),    
                  tableOutput("station_info"),
                  easyClose = TRUE,
                  footer = modalButton("Close")
      )
    )
    
  })

  summary_map_click <- reactiveValues(monthly_rain=NULL,yearly_rain=NULL,
                                      mean_precipitation=NULL,
                                      mean_snow=NULL,
                                      mean_rain=NULL)

  observe({
    vals$data
    req(nrow(vals$data)>=1)
    
    # Fixed to be sum
      summary_map_click$monthly_rain <- vals$data %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(mean_precipitation=round(sum(`precipitation_sum`,na.rm=TRUE),3),
                       mean_snow=round(sum(`snowfall_sum`,na.rm=TRUE),3),
                       mean_rain=round(sum(`rain_sum`,na.rm=TRUE),3)
                       ,.groups="drop")
    
      
      summary_map_click$yearly_rain <- vals$data %>%
        group_by(year,month) %>%
        summarise(monthly_sum=sum(precipitation_sum))%>%
        group_by(year) %>%
        summarise(total_precip=sum(monthly_sum),.groups="drop") 
      
      # TODO fixing 
      req("precipitation_sum" %in% names(vals$data))
      summary_map_click$mean_precipitation <- 
      vals$data %>%
      group_by(year,month) %>%
      summarise(monthly_sum=sum(precipitation_sum))%>%
      filter(month %in% input$month_picker) %>%
      ungroup() %>%
      dplyr::summarise(Mean=round(mean(monthly_sum,.na.rm=TRUE),3),.groups="drop")
      
      print(summary_map_click$mean_precipitation)
      
      # TODO fixing
      req("rain_sum" %in% names(vals$data))
      summary_map_click$mean_rain <- 
        vals$data %>%
        group_by(year,month) %>%
        summarise(monthly_sum=sum(rain_sum))%>%
        filter(month %in% input$month_picker) %>%
        ungroup() %>%
        dplyr::summarise(Mean=round(mean(monthly_sum,.na.rm=TRUE),3),.groups="drop")
      
      # TODO fixing
       req("snowfall_sum" %in% names(vals$data))
       summary_map_click$mean_snow <- 
      vals$data %>%
        group_by(year,month) %>%
        summarise(monthly_sum=sum(snowfall_sum))%>%
        filter(month %in% input$month_picker) %>%
        ungroup() %>%
        dplyr::summarise(Mean=round(mean(monthly_sum,.na.rm=TRUE),3),.groups="drop")
      
      
      })
  
  # Render values for the boxes
  output$snow_value <- renderText({
    req(summary_map_click$mean_snow) 
    paste(summary_map_click$mean_snow, "(in)")
  })
  
  output$rain_value <- renderText({
    req(summary_map_click$mean_rain) 
    paste(summary_map_click$mean_rain, "(in)")
  })

  output$total_precip_value <- renderText({
    req(summary_map_click$mean_precipitation) 
    paste(summary_map_click$mean_precipitation, "(in)")
  })
  
  output$monthly_precip_table <- renderDT({
    req(nrow(summary_map_click$monthly_rain)>=1)
    req(!is.null(vals$lat))
   
    datatable(summary_map_click$monthly_rain %>%
                mutate(lat=vals$lat,
                       lon=vals$lon)%>%
                select(month,mean_precipitation,lat,lon),
              height = "100%",
              extensions = 'Buttons',
              options = list(
                dom = 'frltipB', 
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), 
                pageLength = 25
              )) 

  })
  
  output$yearly_precip_table <- renderDT({
    req(nrow(summary_map_click$yearly_rain)>=1)
    
    datatable(summary_map_click$yearly_rain,
              height = "100%",
              extensions = 'Buttons',
              options = list(
                dom = 'frltipB', 
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), 
                pageLength = 100
              ))
    
  })

  
output$daily_map_click_data <- renderPlotly({
  
  req(nrow(vals$data)>=1)
  
  rainfall_data <- vals$data
  
  plot_ly(rainfall_data, x = ~date) %>%
    # Add precipitation as bars
    add_bars(
      y = ~precipitation_sum,
      name = "Total Precipitation (in)",
      marker = list(color = 'rgba(58, 71, 80, 0.6)'),
      text = ~paste("Date:", date, "<br>Precipitation:", precipitation_sum, "in"),
      hoverinfo = "text"
    ) %>%
    # Add snowfall as a line
    add_lines(
      y = ~snowfall_sum,
      name = "Snowfall (in)",
      line = list(color = 'rgba(44, 160, 44, 0.8)', width = 2),
      text = ~paste("Snowfall:", snowfall_sum, "in"),
      hoverinfo = "text"
    ) %>%
    # Add rain as a line
    add_lines(
      y = ~rain_sum,
      name = "Rain (in)",
      line = list(color = 'rgba(31, 119, 180, 0.8)', width = 2, dash = 'dash'),
      text = ~paste("Rain:", rain_sum, "in"),
      hoverinfo = "text"
    ) %>%
    # Add temperature as a separate y-axis
    add_lines(
      y = ~temperature_2m_max,
      name = "Max Temperature (°C)",
      yaxis = "y2",
      line = list(color = 'rgba(214, 39, 40, 0.8)', width = 2),
      text = ~paste("Max Temperature:", temperature_2m_max, "°C"),
      hoverinfo = "text"
    ) %>%
    # Layout adjustments
    layout(
      title = list(text = "Daily Rainfall, Snowfall, and Temperature in Columbia County", font = list(size = 20)),
      xaxis = list(title = "Date"),
      yaxis = list(title = "Precipitation (in)", side = "left"),
      yaxis2 = list(
        title = "Temperature (°C)",
        overlaying = "y",
        side = "right",
        showgrid = FALSE
      ),
      legend = list(orientation = "h", x = 0.1, y = -0.2),
      hovermode = "x unified"
    )
  
  
})
  
  
  
  
}

shinyApp(ui, server)
