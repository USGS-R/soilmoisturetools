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


