#'
#'@title Get Data for a SCAN site
#'
#'@param stationTriplets 
#'A character vector of the station ID triplets (e.g., c("9897:NM:UCOOP", "9652:AZ:UCOOP")). 
#'Stations can be queried with \code{\link{scan_sites_by_element_code}}.
#'
#'@param depth Individual numeric value for soil moisture sensor depth. In units inches. Common depths include \code{-2, -4, -8, -20, and -40}
#'@param start Value with class \code{\link{Date}} for the beginning date for the data query
#'@param end Value with class \code{\link{Date}} for the latest date for the data query
#'
#'
#'@import httr
#'@import XML
#'
#'@examples
#'\dontrun{
#'#To get pretty much all the data, just do this
#'stations = scan_sites_by_element_code('SMS')
#'
#'#most stations seem to have 5 sensors
#'#2inch sensors
#'data_2 = scan_data(stations, -2)
#'
#'#4inch sensors
#'data_4 = scan_data(stations, -4)
#'
#'#8 inch sensors
#'data_8 = scan_data(stations, -8)
#'
#'#20 inch
#'data_20 = scan_data(stations, -20)
#'
#'#40 inch
#'data_20 = scan_data(stations, -40)
#'}
#'@export
scan_data = function(stationTriplets, depth, start=Sys.Date()-as.difftime(1, units='days'), end=Sys.Date()){
	
	
	cmd = xmlNode("tns:getHourlyData", namespaceDefinitions=c("tns"="http://www.wcc.nrcs.usda.gov/ns/awdbWebService"))
	
	stationNodes = lapply(stationTriplets, xmlNode, name="stationTriplets")
  cmd = addChildren(cmd, kids=stationNodes)
	
	cmd = addChildren(cmd, kids=list(
		xmlNode("ordinal", 1),
		xmlNode("beginDate", as.character(start)),
		xmlNode("endDate", as.character(end))))
	
	#newXMLNode("getFlags", "true", parent=cmd)
	#newXMLNode("alwaysReturnDailyFeb29", "true", parent=cmd)
	#newXMLNode("duration", "DAILY", parent=cmd)
	hd = xmlNode("heightDepth", 
		xmlNode("unitCd",'in'),
		xmlNode("value", depth))
	
	cmd = addChildren(cmd, kids=list(
		hd, 
		xmlNode("elementCd", "SMS")
		))
	
	body = xmlNode("soap:Body", cmd)
	
	root = xmlNode("soap:Envelope", namespaceDefinitions=c("soap"="http://schemas.xmlsoap.org/soap/envelope/", "xsd"="http://www.w3.org/2001/XMLSchema"), body)
	
	out = POST(service_url, content_type("text/soap_xml; charset-utf-8"), body=toString.XMLNode(root))
	#values = xpathSApply(content(out), '//values', xmlValue)
	
	parse_vals = function(x){
		station = xpathSApply(x, 'stationTriplet', xmlValue)
		datetime = xpathSApply(x, 'values/dateTime', xmlValue)
		flag = xpathSApply(x, 'values/flag', xmlValue)
		value = xpathSApply(x, 'values/value', xmlValue, trim=TRUE)
		
		return(data.frame(station, datetime, flag, value, stringsAsFactors = FALSE))
	}
	
	returns = xpathApply(content(out), '/*/*/*/return[values]', fun=parse_vals)
	
	all_out = do.call(rbind, returns)
	all_out$depth_cm = abs(round(depth*2.54)) #convert to cm and round this one because that's what TX group is doing
	all_out$value = as.numeric(all_out$value)
	all_out$flag = NULL #drop flag for now, not using it, no other service has them
	
	return(all_out)
}
