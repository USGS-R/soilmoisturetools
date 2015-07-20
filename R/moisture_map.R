#'@title create a leaflet map for soilmoisture
#'@param data data.frame from data service function (e.g., \link{tx_data})
#'@param metadata data.frame from data service metadata function (e.g., \link{ok_sites_metadata})
#'@param out_file html output file name for leaflet map
#'@importFrom htmlwidgets saveWidget
#'@importFrom dplyr select filter %>% left_join
#'@import leaflet
#'@import XML
#'@examples
#'\dontrun{
#'moisture_map(to_hist_percentile(ok_data()), ok_sites_metadata(ok_data()$station), out_file= 'soilmoisture_usgs.html')
#'}
#'@export
moisture_map <- function(data, metadata, out_file){
  
	
  soil_moisture <- select(data, station, value) %>% 
    filter(!duplicated(station))
  
  soil_moisture$station <- as.character(soil_moisture$station)
  
  
  station_loc <- select(metadata, name, latitude, longitude, station)
  station_loc <- left_join(station_loc, soil_moisture, by = 'station')
  
  names(station_loc)[2] <- 'lat'
  names(station_loc)[3] <- 'lng'
  col_types <- c( "red","orange", "yellow", "grey80","dodgerblue","darkblue")
  leg_vals <- seq(0,100, 10)
  cols <- colorNumeric(col_types, domain = leg_vals)
  pops <- paste(station_loc$name, sprintf('<br/>(%1.1f)',station_loc$value), paste0('<br/>',station_loc$station))
  
  
  m = leaflet(station_loc) %>% 
    addTiles(options = tileOptions(zIndex = 0, maxZoom = 8, minZoom = 4)) %>% 
    addCircleMarkers(~lng, ~lat, popup = pops,
                     fillColor = ~cols(value), color = "grey60", 
                     fillOpacity = 0.8, radius = 6, opacity = 0.8, 
                     options = markerOptions(zIndexOffset = 9)) %>%
    addLegend(
      position = 'bottomright',
      colors = cols(leg_vals),
      labels = paste(leg_vals,'%', sep = ''), opacity = 0.8,
      title = paste0('Soil moisture<br/>',Sys.Date())) %>%
    setView(-109.36, 36.67, zoom = 4)

  saveWidget(m, out_file, selfcontained = FALSE, libdir = NULL)
  return(out_file)
}