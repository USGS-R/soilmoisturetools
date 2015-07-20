library(stringr)

files = Sys.glob('inst//extdata//historical_prct_coeff/Oklahoma*')

out = data.frame()

for(i in 1:length(files)){
	 depth = as.numeric(str_extract(files[i], '([0-9]+)'))
	 
	 tmp = read.csv(files[i], header=FALSE)
	 tmp$V8 = depth
	 tmp = tmp[, c('V1', 'V8', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7')]
	 
	 out = rbind(out, tmp)
}


id_conv = read.csv('inst/extdata/id_conv/Oklahoma Mesonet Station Reference.csv')

out = merge(id_conv, out, by.x='StationID', by.y='V1')
out$StationID = NULL #drop the old texas station ID

out = out[order(out$STNUM, out$V8, out$V2), ]

write.table(out, 'inst/extdata/historical_prct_coeff/Oklahoma_mesonet.csv', row.names=FALSE, col.names=FALSE, sep=',')
