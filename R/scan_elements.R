
#'@title Get all scan database element codes
#'
#'
#'@return a \code{data.frame} with columns elementCd, name, and storedUnitCd
#'
#'@examples
#'\dontrun{
#'all_elements = scan_elements()
#'print(all_elements$elementCds)
#'}
#'
#'@import XML
#'@import httr
#'
#'@export
scan_elements = function(){
	
	root = newXMLNode("soap:Envelope", namespaceDefinitions=c("soap"="http://schemas.xmlsoap.org/soap/envelope/", "xsd"="http://www.w3.org/2001/XMLSchema"))
	
	body = newXMLNode("soap:Body", parent=root)
	
	cmd = newXMLNode("tns:getElements", namespaceDefinitions=c("tns"="http://www.wcc.nrcs.usda.gov/ns/awdbWebService"), parent=body)
	
	#newXMLNode("elementCds", element_code, parent=cmd)
	#newXMLNode("ordinals", 1, parent=cmd)
	#newXMLNode("logicalAnd", "false", parent=cmd)
	
	out = POST(service_url, content_type("text/soap_xml; charset-utf-8"), body=toString.XMLNode(root))
	elementCd = xpathSApply(content(out), '//return//elementCd', xmlValue)
	name = xpathSApply(content(out), '//return//name', xmlValue)
	storedUnitCd = xpathSApply(content(out), '//return//storedUnitCd', xmlValue)
	
	return(data.frame(elementCd, name, storedUnitCd))
}
