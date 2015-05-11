#'@title create a leaflet map for soilmoisture
#'@param depth in inches for sensors
#'@param out_file html output file name for leaflet map
#'@importFrom htmlwidgets saveWidget
#'@importFrom dplyr select filter %>% left_join
#'@import leaflet
#'@import XML
#'@examples
#'\dontrun{
#'moisture_map(-2,  out_file= 'soilmoisture_usgs.html')
#'}
#'@export
moisture_map <- function(depth, out_file){
  sites = scan_sites_by_element_code('SMS')
  
  ca_sites <- sites#[is_ca]
  #Just use the default start/end date
  data = scan_data(ca_sites, depth = depth, start = '2014-06-01', end = '2014-06-02')
  soil_moisture <- select(data, station, value) %>% 
    filter(!duplicated(station))
  names(soil_moisture)[1] <- 'stationTriplet'
  soil_moisture$stationTriplet <- as.character(soil_moisture$stationTriplet)
  ca_metadata = scan_sites_metadata(soil_moisture$stationTriplet)
  
  
  
  
  station_loc <- select(ca_metadata, name, latitude, longitude, stationTriplet)
  station_loc <- left_join(station_loc, soil_moisture, by = 'stationTriplet')
  
  names(station_loc)[2] <- 'lat'
  names(station_loc)[3] <- 'lng'
  col_types <- c( "red","orange", "yellow", "grey80","dodgerblue","darkblue")
  leg_vals <- pretty(station_loc$value)
  cols <- colorNumeric(col_types, domain = leg_vals)
  pops <- paste(station_loc$name, sprintf('<br/>(%1.1f)',station_loc$value), paste0('<br/>',station_loc$stationTriplet))
  
  
  
  m = leaflet(station_loc) %>% 
    addTiles(options = tileOptions(zIndex = 0, maxZoom = 8, minZoom = 4)) %>% 
    addCircleMarkers(~lng, ~lat, popup = pops,
                     fillColor = ~cols(value), color = "grey60", 
                     fillOpacity = 0.8, radius = 6, opacity = 0.8, 
                     options = markerOptions(zIndexOffset = 9)) %>%
    setView(-109.36, 36.67, zoom = 4)
  
  saveWidget(m, out_file, selfcontained = FALSE, libdir = NULL)
  doc = htmlParse(out_file,isURL=FALSE, useInternalNodes = TRUE)
  top = xmlRoot(doc)
  div_node <- getNodeSet(top, "//div[contains(@id,'htmlwidget_container')]/div[1]")
  div_node[[1]] <- add_svg_legend(div_node[[1]],
                                  cols = cols(leg_vals), names = leg_vals, title = 'Soil moisture [-]')
  saveXML(doc, file = out_file)
  return(out_file)
}