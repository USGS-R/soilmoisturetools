
files = Sys.glob('data-raw/coefficient_files/crn/*.csv')

all_out = data.frame()

for(i in 1:length(files)){
	d = read.csv(files[i], header=FALSE)
	
	d$depth = as.numeric(gsub("[^0-9]", "", basename(files[i])))
	
	all_out = rbind(all_out, d)
	
}

id_conv = read.table('data-raw/id_conv_files/crn reference.csv', sep=',', header=TRUE, comment.char="")

all_out = merge(all_out, id_conv, by.x='V1', by.y='StationID')


write.table(all_out[, c('WBANNO', 'depth', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7')], 
						'inst/extdata/historical_prct_coeff/CRN_Coefficients.csv', sep=',', row.names=FALSE, col.names=FALSE)

