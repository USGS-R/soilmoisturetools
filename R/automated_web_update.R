#'@title Wrapper function for automated runs
#'
#'
#'@param outpath Path to store generated output
#'
#'
#'
#'@export
automated_web_update = function(outpath='.'){
	
	#as per the suggestion here, don't try to direct knitr output
	# https://github.com/yihui/knitr/issues/913
	#origin = getwd()
	#setwd(outpath)
	
	kfile = system.file('rmd_web/index.Rmd', package=packageName())
	
	rmarkdown::render(input = kfile, output_dir=outpath)
	
	#Now move the header and footer over
	hdir = system.file('rmd_web/header', package=packageName())
	fdir = system.file('rmd_web/footer', package=packageName())
	file.copy(hdir, outpath, recursive = TRUE)
	file.copy(fdir, outpath, recursive = TRUE)
	
	#setwd(origin)
}