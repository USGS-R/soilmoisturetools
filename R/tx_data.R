#'
#'@title Get Texas soil moisture data
#'
#'@description Texas data supplied by Texas A&M partner service
#'
#'@import httr
#'@importFrom reshape2 melt
#'@importFrom stringr str_extract_all
#'
#'@examples
#'
#'data = tx_data()
#'
#'@import httr
#'@import jsonlite
#'@importFrom stringr str_extract_all
#'
#'@export
tx_data = function(){
	
	state = 'tx'
	
	url = paste0('http://128.194.107.250/SoilAPI/api/soil', state)
	soil_data = content(GET(url))
	
	soil_df = do.call(rbind, lapply(soil_data, as.data.frame, stringsAsFactors=FALSE))
	
	#parse date/time
	soil_df$datetime = as.POSIXct(strptime(soil_df$Time, "%FT%T", tz="UTC"))
	
	#concat station name and number together, add OK for Oklahoma
	soil_df$station = paste(soil_df$StationName, toupper(state), sep=':')
	
	soil_df = soil_df[,c('station','datetime','SoilM5', 'SoilM60', 'SoilM75')]
	
	soil_long = melt(soil_df, id.vars = c('station','datetime'))
	soil_long$variable = as.character(soil_long$variable)
	
	#pull out the numeric part, that's the depth
	depths = str_extract_all(soil_long$variable, pattern = '[0-9]+', simplify = TRUE)
	
	#extract depth from variable name and convert to inches 
	soil_long$depth_in = as.numeric(depths)/2.54
	
	soil_long$variable = NULL
	#convert to percent
	soil_long$value = soil_long$value*100
	
	return(soil_long)
}