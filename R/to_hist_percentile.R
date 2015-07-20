#'@title Convert values to historical percentile
#'
#'@param data A data frame from one of the service functions (e.g., \link{ok_data})
#'
#'
#'
#'
#'
#'
#'@export
to_hist_percentile = function(data){
	
	service = strsplit(data$station[1], ':')[[1]][2]
	
	fpath = switch(toupper(service), 
				 		TX="WestTexas_Coefficients.csv", 
				 		OK="Oklahoma_mesonet.csv",
				 		'error'
				 )
	#make sure we got a match
	if(fpath == 'error'){
		stop('unrecognized service in station ids')
	}
	
	coeff = load_coefficients(fpath)
	
	data$month = as.POSIXlt(data$datetime)$mon+1
	data$depth_cm = floor(data$depth_in*2.54) #convert to CM for better matching
	data$station_id = sapply(strsplit(data$station, ':'), function(x){x[1]})
	
	
	data = merge(data, coeff, by=c('station_id', 'month', 'depth_cm'))
	
	values = data$value/100
	data$value = data$c4*values^4 + data$c3*values^3 + data$c2*values^2 + data$c1*values + data$c0
	
	data$value = data$value * 100
	
	return(data[, c('station', 'datetime', 'value', 'depth_in')])
	
}


load_coefficients = function(fpath){
	
	pkg_path = system.file(paste0('extdata/historical_prct_coeff/', fpath), package=packageName())
	
	coeff = read.csv(pkg_path, header=FALSE)
	names(coeff) = c('station_id', 'depth_cm', 'month', 'c4', 'c3', 'c2', 'c1', 'c0')
	return(coeff)
}

