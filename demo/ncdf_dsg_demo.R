## Get data, save in NCDF DSG file.
library(soilmoisturetools)
library(dplyr)
library(netcdf.dsg)

ok = ok_data() %>% filter(depth_in < 2)

ok_meta = ok_sites_metadata(ok$station)

#tx = ok_data() %>% filter(depth_in < 2)

#tx_meta = ok_sites_metadata(ok$station)

#scan = scan_data(scan_sites_by_element_code('SMS'), depth = -2, start=Sys.Date()-1, end=Sys.Date())

write_point_dsf('test.nc', ok$datetime, ok_meta$nlat, ok_meta$elon, ok_meta$elev,	
								ok[, 'value', drop=FALSE], data_units='%', data_prec ='double')


