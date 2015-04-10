
##Demo getting all data for all sites with soil moisture
library(soilmoisturetools)

sites = scan_sites_by_element_code('SMS')

#Just use the default start/end date
data = scan_data(sites, depth = -2)

metadata = scan_sites_metadata(sites)


