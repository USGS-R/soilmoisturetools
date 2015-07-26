#'@title Output ncdf file for web service
#'
#'
#'@param outpath Path to store generated output
#'
#'@importFrom netcdf.dsg write_timeseries_dsg
#'@importFrom reshape2 dcast
#'
#'@export
automated_ncdf_update = function(output='.'){
	
	ok     = to_hist_percentile(ok_data())
	tx     = to_hist_percentile(tx_data())
	
	data = rbind(tx, ok)
	
	fname = paste0(format(Sys.time(), '%Y%m%d_%H%M'), '.nc')
	fpath = file.path(output, fname)

	
	wide_sites = dcast(data, datetime~station, mean)
	times = wide_sites$datetime
	wide_sites = wide_sites[,-1]
	station_names = names(wide_sites)
	names(wide_sites) = rep('moisture_percentile', ncol(wide_sites))
	
	ok_met = ok_sites_metadata(station_names[grepl(':OK:', station_names)])
	tx_met = tx_sites_metadata(station_names[grepl(':TX', station_names)])
	
	ok_met = ok_met[, intersect(names(ok_met), names(tx_met))]
	tx_met = tx_met[, intersect(names(ok_met), names(tx_met))]
	
	#preserve the station order
	metadata = merge(data.frame(station=station_names), rbind(ok_met, tx_met), all.x=TRUE)
	
	write_timeseries_dsg(fpath, station_names, metadata$latitude, metadata$longitude,
											 times, wide_sites, data_unit='percent', data_prec ='double', 
											 attributes=list(
											 	title='National Soil Moisture Network SOS', 
											 	abstract="This service provides soil moisture data from the U.S. National Soil Moisture Network Pilot and serves data from SCAN, CRN, West Texas and Oklahoma Mesonets. This SOS web service delivers the data using OGC's GML.", 
											 	'provider name'='U.S. Geological Survey, Office of Water Information, Center for Integrated Data Analytics, United States Government', 
											 	'provider site'='http://cida.usgs.gov', 
											 	description='Percentile of Volumetric Soil Moisture as compared to the historical distribution. Percentiles are calculated using cumulative distribution functions and range from 0-100.'))
	
	
	
}