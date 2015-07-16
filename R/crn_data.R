#'
#'
#'@title Get CRN soil moisture data
#'
#'@description CRN data supplied by Texas A&M partner service.
#'
#'
#'
#'@import httr
#'@importFrom reshape2 melt
#'
#'@export
crn_data = function(){
	
	state = 'crn'
	
	url = paste0('http://128.194.107.250/SoilAPI/api/soil', state)
	soil_data = content(GET(url))
	
	soil_df = do.call(rbind, lapply(soil_data, as.data.frame, stringsAsFactors=FALSE))
	
	#parse date/time
	soil_df$datetime = as.POSIXct(strptime(soil_df$Time, "%FT%T", tz="UTC"))
	soil_df$Time = NULL
	
	#concat station name and number together, add OK for Oklahoma
	soil_df$station = paste(soil_df$StationNum, toupper(state), soil_df$StationName, sep=':')
	
	soil_df$StationNum = NULL
	soil_df$StationName = NULL
	
	soil_df = soil_df[,c('station','datetime', 'SoilM5', 'SoilM10', 'SoilM20', 'SoilM50', 'SoilM100')]
	
	soil_long = melt(soil_df, id.vars = c('station','datetime'))
	soil_long$variable = as.character(soil_long$variable)
	
	#extract depth from variable name and convert to inches from cm 
	soil_long$depth_in = as.numeric(substr(soil_long$variable, 6, 8))/2.54
	
	soil_long$variable = NULL
	
	#convert to percent
	soil_long$value = soil_long$value*100
	
	return(soil_long)
}

