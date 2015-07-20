#'@title Wrapper function for automated runs
#'
#'
#'@param outpath Path to store generated output
#'
#'
#'
#'@export
automated_web_update = function(outpath='.'){
	
	ok     = to_hist_percentile(ok_data())
	ok_met = ok_sites_metadata(ok$station)
	
	tx     = to_hist_percentile(tx_data())
	tx_met = tx_sites_metadata(tx$station)
	
	ok_met = ok_met[, intersect(names(ok_met), names(tx_met))]
	tx_met = tx_met[, intersect(names(ok_met), names(tx_met))]
	
	moisture_map(rbind(tx, ok), 
							 rbind(tx_met, ok_met),
							 out_file= file.path(outpath, 'index.html'))
	
	
}