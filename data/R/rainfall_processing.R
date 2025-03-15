
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(FedData)
library(geojsonsf)
library(terra)
library(raster)

columbia_county_boundary_url <- "https://services.arcgis.com/XG15cJAlne2vxtgt/ArcGIS/rest/services/WA_Columbia_Web_LiDAR/FeatureServer/1/query?where=1%3D1&objectIds=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&relationParam=&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=false&returnEnvelope=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&collation=&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnTrueCurves=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=geojson"
columbia_county_boundary <- suppressWarnings(geojson_sf(geojson = columbia_county_boundary_url))

# Get the DAYMET (North America only)
# Returns a raster
DAYMET <- get_daymet(template = columbia_county_boundary,
                     label = "Columbia",
                     elements = c("prcp"),
                     years = 2023,
                     tempo="ann")

demo_raster <- DAYMET$prcp

# demo_raster2 <- projectRaster(
#   demo_raster, 
#   crs = "+proj=longlat +datum=WGS84"
# )

pal <- colorNumeric(
  palette = "Blues",  # Choose a color palette
  domain = values(demo_raster),  # Use raster values as the domain
  na.color = "transparent"  # Make NA values transparent
)



# raW
raw_rain_data_path <- "../../../Downloads/Historical Rain Data Starting 00-01.xlsx"
coords_path <- "../../../Downloads/COOPERATORS.xlsx"

coords <- read_excel(coords_path, sheet = 1)%>%
  mutate(NAME=toupper(NAME), STATION=paste0(STATION,"-",CITY))

# Get all sheet names, dump SEPT-DEC|SEPT-APRIL
sheet_names <- excel_sheets(raw_rain_data_path) %>%
  str_subset(pattern = "SEPT-DEC|SEPT-APRIL",negate = TRUE)


# Read and combine all sheets
combined_data <- sheet_names %>%
  purrr::map_dfr(~read_excel(raw_rain_data_path, sheet = .x) %>%
                   mutate(YEARS=.x)%>%
                   mutate(across(everything(),
                 ~if (cur_column() %in% c("NAME","YEARS")) as.character(.) else as.numeric(.))))%>%
  mutate(NAME=toupper(NAME)) %>%
  select(NAME:YEARS) %>%
  left_join(coords %>% 
              select(STATION,NAME,ADDRESS,LAT,LONG),by="NAME") %>%
  mutate(STATION=if_else(is.na(STATION),"UNKNOWN",STATION)) %>%
  mutate(across(SEPT:TOTAL, ~na_if(., 0))) %>%
  filter(!is.na(TOTAL)) %>%
  group_by(STATION) %>%
  mutate(AVERAGE=mean(TOTAL,na.rm = TRUE))%>%
  select(-NAME)%>%
  ungroup()
  

monthly_means <- combined_data %>%
  pivot_longer(cols = SEPT:TOTAL,  # Columns to pivot (months and TOTAL)
               names_to = "MONTH",  # New column for month names
               values_to = "VALUE") %>%  # New column for month values
  filter(MONTH != "TOTAL") %>%  # Exclude the TOTAL column
  group_by(STATION, MONTH) %>%  # Group by station and month
  summarize(MEAN_VALUE = mean(VALUE, na.rm = TRUE), .groups = "drop") %>%
  left_join(coords %>% 
              select(STATION,ADDRESS,LAT,LONG),by="STATION") 


# Save processed data
saveRDS(combined_data, "data/cleaned_rain_data.rds")
saveRDS(monthly_means, "data/monthly_means.rds")
saveRDS(columbia_county_boundary, "data/columbia_county_boundary.rds")
saveRDS(demo_raster, "data/annual_rain_raster.rds")
