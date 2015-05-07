#'
#'
#'@title Get Oklahoma soil moisture data
#'
#'@description Data supplied by Texas A&M partner service
#'
#'
#'
#'@importFrom httr GET
#'@importFrom reshape2 melt
#'
#'@export
ok_data = function(){

	state = 'ok'
	
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
	
	soil_df = soil_df[,c('station','datetime','TR05', 'TR25', 'TR60')]
	
	soil_long = melt(soil_df, id.vars = c('station','datetime'))
	soil_long$variable = as.character(soil_long$variable)
	
	#extract depth from variable name and convert to inches 
	soil_long$depth_in = as.numeric(substr(soil_long$variable, 3,4))/2.54
	
	soil_long$variable = NULL
	return(soil_long)
}
