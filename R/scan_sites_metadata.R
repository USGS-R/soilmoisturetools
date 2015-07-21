
#'@title Get SCAN station metadata
#'
#'@inheritParams scan_data
#'
#'@note This is somewhat slow. Queries over 1000 sites are split into multiple requests. 
#'I recommend querying only the specific sites you need.
#'
#'@return data frame with a row for each station. Order returned same as requested station order is not guaranteed.
#'
#'@author Luke Winslow
#'
#'@examples 
#'\dontrun{
#'scan_sites_metadata("9897:NM:UCOOP")
#'}
#'@export
scan_sites_metadata = function(stationTriplets){
	
	df_out = data.frame()
	stationChunks = split(stationTriplets, ceiling(seq_along(stationTriplets)/1000))
	
	for(i in 1:length(stationChunks)){
		
		cmd = xmlNode("tns:getStationMetadataMultiple", namespaceDefinitions=c("tns"="http://www.wcc.nrcs.usda.gov/ns/awdbWebService"))
		
		stationNodes = lapply(stationChunks[[i]], xmlNode, name="stationTriplets")
		cmd = addChildren(cmd, kids=stationNodes)
			
		body = xmlNode("soap:Body", cmd)
		
		root = xmlNode("soap:Envelope", namespaceDefinitions=c("soap"="http://schemas.xmlsoap.org/soap/envelope/", "xsd"="http://www.w3.org/2001/XMLSchema"), body)
		
		out = POST(service_url, content_type("text/soap_xml; charset-utf-8"), body=toString.XMLNode(root))
		#values = xpathSApply(content(out), '//values', xmlValue)
		
	
		df_out = rbind(df_out, xmlToDataFrame(nodes=xpathApply(content(out), '/*/*/*/return')))
	}
	
	df_out$elevation = as.numeric(df_out$elevation)
	df_out$latitude  = as.numeric(df_out$latitude)
	df_out$longitude  = as.numeric(df_out$longitude)
	
	return(df_out)
}
