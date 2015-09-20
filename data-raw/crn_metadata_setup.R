

#'\code{station}, \code{latitude}, \code{longitude}, and \code{name}

meta    = read.table('data-raw/metadata_files/crn_site_info.csv', sep=',', header=TRUE)
id_conv = read.table('data-raw/id_conv_files/crn reference.csv', sep=',', header=TRUE)

meta = merge(meta, id_conv[, c('WBANNO', 'StationID')], by='StationID')

meta = meta[, c('WBANNO', 'Latitude', 'Longitude', 'StationName')]
names(meta) = c('station', 'latitude', 'longitude', 'name')

write.table(meta, 'inst/extdata/crn_site_info.csv', sep=',', row.names=FALSE)
