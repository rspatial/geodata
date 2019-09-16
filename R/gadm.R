
gadm <- function(country, level, path, version=3.6) {

	stopifnot(file.exists(path))
	country <- .getCountryISO(country)

	if (missing(level)) {
		stop('provide a "level=" argument; levels can be 0, 1, or 2 for most countries, and higher for some')
	}
	stopifnot(version %in% 3.6)

	if (version > 3) {
		filename <- file.path(path, paste0('gadm36_', country, '_', level, "_pk.rds"))
	}
	
	if (!file.exists(filename)) {
		baseurl <- paste0("https://biogeo.ucdavis.edu/data/gadm", version)
		theurl <- paste(baseurl, '/pck/gadm36_', country, '_', level, "_pk.rds", sep="")			
		.download(theurl, filename)
		if (!file.exists(filename))	{ 
			message("\nCould not download file -- perhaps it does not exist") 
		}
	}	
	if (file.exists(filename)) {
		return( vect(readRDS(filename)) ) 
	} else {
		return(NULL)
	}
}


