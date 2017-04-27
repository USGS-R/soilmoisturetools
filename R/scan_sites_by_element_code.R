

#'@title Grab all scan sites with certain parameter
#'
#'@param elementCd SCAN element code (see \code{\link{scan_elements}} for full list of codes)
#'
#'@return A list of site triplets as character strings
#'
#'@examples
#'\dontrun{
#'sites_vec = scan_sites_by_element_code('SMS')
#'}
#'@export
scan_sites_by_element_code = function(elementCd){
	
	root = newXMLNode("SOAP-ENV:Envelope", namespaceDefinitions=c("SOAP-ENV"="http://schemas.xmlsoap.org/soap/envelope/", "q0"="http://www.wcc.nrcs.usda.gov/ns/awdbWebService", "xsd"="http://www.w3.org/2001/XMLSchema", "xsi"="http://www.w3.org/2001/XMLSchema-instance"))
	
	body = newXMLNode("SOAP-ENV:Body", parent=root)
	
	cmd = newXMLNode("q0:getStations", parent=body)
	
	newXMLNode("elementCds", elementCd, parent=cmd)
	newXMLNode("logicalAnd", "false", parent=cmd)
	
	out = POST(service_url, content_type("text/soap_xml; charset-utf-8"), body=toString.XMLNode(root))
	
	sites = xpathSApply(content(out), '//return', xmlValue)
	
	#Currently filter and just return SCAN sites, this may change in the future
	id_strings = sapply(sites, function(x){strsplit(x, ":")[[1]][3]})
	sites = sites[id_strings == "SCAN"]
	
	return(sites)
}

