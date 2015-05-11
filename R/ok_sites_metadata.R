#'@title OK Mesonet site metadata
#'
#'@param station_ids Character vector of station ids. 
#'
#'@description
#'OK site metadata for OK mesonet sites. Data pulled from here
#'\url{http://www.mesonet.org/index.php/site/sites/station_names_map}
#'
#'@examples
#'
#'ok = ok_data()
#'meta = ok_sites_metadata(ok$station)
#'
#'@export
ok_sites_metadata = function(station_ids){
	
	#File from here: http://www.mesonet.org/index.php/site/sites/station_names_map
	meta_path     = system.file('extdata/ok_mesonet_site_info.csv', package=packageName())
	ok_meta = read.csv(meta_path, header=TRUE)
	
	ok_meta$station = paste(ok_meta$stnm, "OK", ok_meta$stid, sep=':')
	
	ok_meta = merge(data.frame(station=station_ids), ok_meta, all.x=TRUE)
	
	return(ok_meta)
}
