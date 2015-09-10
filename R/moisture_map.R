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
#'ok = to_hist_percentile(ok_data())
#'ok_meta = ok_sites_metadata(ok$station)
#'moisture_map(ok, ok_meta, out_file= '~/soilmoisture_usgs.html')
#'}
#'@export
moisture_map <- function(data, metadata, out_file=NULL){
  
	
  soil_moisture <- select(data, station, value, datetime, depth_cm) %>% 
    filter(!duplicated(station))
  
  soil_moisture$station <- as.character(soil_moisture$station)
  
  
  station_loc <- select(metadata, station, latitude, longitude, station, name)
  station_loc <- left_join(station_loc, soil_moisture, by = 'station')
  
  names(station_loc)[2] <- 'lat'
  names(station_loc)[3] <- 'lng'
  
  #this should have complete data, omit data missing lat/lon/station or value
  station_loc = na.omit(station_loc)
  
  col_types <- c( "#600000", "#E00000", "#E69800","#FFFF00","#FFFFFF","#D9F0A3","#ADDD8E","#78C679","#31A354","#006837")
  leg_vals <- seq(0,100, 10)
  cols <- colorNumeric(col_types, domain = leg_vals)
  pops <- build_popups(station_loc)
  
  
  m = leaflet(station_loc) %>% 
    addTiles(options = tileOptions(zIndex = 0, maxZoom = 8, minZoom = 4)) %>% 
    addCircleMarkers(~lng, ~lat, popup = pops,
                     fillColor = ~cols(value), color = "grey60", 
                     fillOpacity = 0.8, radius = 6, opacity = 0.8, stroke=FALSE,
                     options = markerOptions(zIndexOffset = 9)) %>%
    addLegend(
      position = 'bottomright',
      colors = cols(leg_vals),
      labels = paste(leg_vals,'%', sep = ''), opacity = 0.8,
      title = paste0('Soil moisture<br/>',Sys.Date())) %>%
    setView(-95.36, 36.67, zoom = 4)

  if(is.null(out_file)){
  	return(m)
  }else{
  	saveWidget(m, out_file, selfcontained = FALSE, libdir = NULL)
  	return(out_file)
  }
}


build_popups = function(data){
	
	#this is just a hack for now. Need to persist or store this data in a different way
	network = sapply(data$station, function(x){strsplit(x, ':')[[1]][3]})
	network[network == 'TX_MESO'] = "Texas Mesonet"
	network[network == 'OK_MESO'] = "Oklahoma Mesonet"
	
	
	data$name[is.na(data$name)] = "unknown"
	
	sprintf("Station Name: %s<br/>Network Name: %s<br/>Measurement depth: %i cm<br/><br/>Soil moisture percentile: %1.1f%%<br/>Last measurement: %s", 
					data$name, network, data$depth_cm, data$value, format(data$datetime, '%m/%d/%Y %I:%M %p'))
	
}