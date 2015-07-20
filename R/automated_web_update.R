#'@title Wrapper function for automated runs
#'
#'
#'@param outpath Path to store generated output
#'
#'
#'
#'@export
automated_web_update = function(outpath='.'){
	
	moisture_map(to_hist_percentile(ok_data()), 
							 ok_sites_metadata(ok_data()$station), 
							 out_file= file.path(outpath, 'index.html'))
	
	
}