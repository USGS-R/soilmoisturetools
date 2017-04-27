

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
	
	root = newXMLNode("soap:Envelope", namespaceDefinitions=c("soap"="http://schemas.xmlsoap.org/soap/envelope/", "xsd"="http://www.w3.org/2001/XMLSchema"))
	
	body = newXMLNode("soap:Body", parent=root)
	
	cmd = newXMLNode("tns:getStations", namespaceDefinitions=c("tns"="http://wcc.sc.egov.usda.gov/awdbWebService"), parent=body)
	
	newXMLNode("elementCds", elementCd, parent=cmd)
	newXMLNode("ordinals", 1, parent=cmd)
	newXMLNode("logicalAnd", "false", parent=cmd)
	
	out = POST(service_url, content_type("text/soap_xml; charset-utf-8"), body=toString.XMLNode(root))
	sites = xpathSApply(content(out), '//return', xmlValue)
	
	
	#Currently filter and just return SCAN sites, this may change in the future
	id_strings = sapply(sites, function(x){strsplit(x, ":")[[1]][3]})
	sites = sites[id_strings == "SCAN"]
	
	return(sites)
}

