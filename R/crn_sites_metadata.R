#'@title CRN site metadata
#'
#'@param station_ids Character vector of station ids. 
#'
#'@description
#'CRN site metadata for CRN sites. 
#'
#'@return A data.frame of metadata. All metadata methods return a data.frame with 
#'\code{station}, \code{latitude}, \code{longitude}, and \code{name}
#'
#'@examples
#'
#'crn = crn_data()
#'meta = crn_sites_metadata(crn$station)
#'
#'@export
crn_sites_metadata = function(station_ids){
	
	meta_path     = system.file('extdata/crn_site_info.csv', package=packageName())
	meta = read.csv(meta_path, header=TRUE, as.is=TRUE)
	
	station = sapply(strsplit(station_ids, ':'), function(x){x[1]})
	
	meta = merge(data.frame(station=station, station_ids=station_ids), meta, all.x=TRUE)
	#drop the station number, replace with full station tuple
	meta$station = meta$station_ids
	meta$station_ids = NULL
	
	return(meta)
}
