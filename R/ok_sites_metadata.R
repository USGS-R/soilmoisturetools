#'@title OK Mesonet site metadata
#'
#'@param station_ids Character vector of station ids. 
#'
#'@description
#'OK site metadata for OK mesonet sites. Data pulled from here
#'\url{http://www.mesonet.org/index.php/site/sites/station_names_map}
#'
#'@return A data.frame of metadata. All metadata methods return a data.frame with 
#'\code{station}, \code{latitude}, \code{longitude}
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
	
  ok_meta$latitude = ok_meta$nlat
  ok_meta$longitude = ok_meta$elon
  ok_meta$elevation = ok_meta$elev
  
	ok_meta$nlat = NULL
	ok_meta$elon = NULL
	ok_meta$elev = NULL
  
	ok_meta = merge(data.frame(station=station_ids), ok_meta, all.x=TRUE)
	
	return(ok_meta)
}
