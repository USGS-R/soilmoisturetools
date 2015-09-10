#'
#'
#'@title Get Oklahoma soil moisture data
#'
#'@description OK data supplied by Texas A&M partner service.
#'
#'
#'
#'@import httr
#'@importFrom reshape2 melt
#'
#'@export
ok_data = function(){

	state = 'ok'
	
	url = paste0('http://128.194.107.250/SoilAPI/api/soil', state)
	soil_data = content(GET(url))
	
	conv_path     = system.file('extdata/ok_mesonet_soil_coefficients.csv', package=packageName())
	ok_conversion = read.csv(conv_path, header=TRUE)
	
	soil_df = do.call(rbind, lapply(soil_data, as.data.frame, stringsAsFactors=FALSE))
	
	#parse date/time
	soil_df$datetime = as.POSIXct(strptime(soil_df$Time, "%FT%T", tz="UTC"))
	soil_df$Time = NULL
	
	#merge the conversion data in 
	soil_df = merge(soil_df, ok_conversion, by.x='StationName', by.y="stid")
	
	soil_df$VWC05 = tr_to_vwc(soil_df$TR05, soil_df$WCR05, soil_df$WCS05, soil_df$A05, soil_df$N05)
	soil_df$VWC25 = tr_to_vwc(soil_df$TR25, soil_df$WCR25, soil_df$WCS25, soil_df$A25, soil_df$N25)
	soil_df$VWC60 = tr_to_vwc(soil_df$TR60, soil_df$WCR60, soil_df$WCS60, soil_df$A60, soil_df$N60)
	
	#concat station name and number together, add OK for Oklahoma
	soil_df$station = paste(soil_df$StationNum, toupper(state), "OK_MESO", sep=':')
	
	soil_df$StationNum = NULL
	soil_df$StationName = NULL
	
	soil_df = soil_df[,c('station','datetime','VWC05', 'VWC25', 'VWC60')]
	
	soil_long = melt(soil_df, id.vars = c('station','datetime'))
	soil_long$variable = as.character(soil_long$variable)
	
	#extract depth from variable name
	soil_long$depth_cm = as.numeric(substr(soil_long$variable, 4,5))
	
	soil_long$variable = NULL
	
	#convert to percent
	soil_long$value = soil_long$value*100
	
	return(soil_long)
}


tr_to_vwc = function(tr, wcr, wcs, a, n){
	
	MP = -0.717*exp(1.788*tr)
	vwc = wcr + (wcs - wcr)/((1+(a*(-1*MP)/100))^(1-(1/n)))
	
	return(vwc)
}

