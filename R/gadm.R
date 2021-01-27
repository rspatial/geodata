
.gadm_download <- function(filename, gversion, upath="pck") {
	if (!dir.exists(dirname(filename))) {
		stop("path does not exist")
	}
	if (!file.exists(filename)) {
		baseurl <- paste0("https://biogeo.ucdavis.edu/data/gadm", gversion)
		if (upath=="") {
			theurl <- file.path(baseurl, basename(filename))		
		} else {
			theurl <- file.path(baseurl, upath, basename(filename))
		}
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


world <- function(resolution=5, level=0, path, version=3.6, ...) {
	stopifnot(level[1] == 0)
	resolution = round(resolution[1])
	stopifnot(resolution %in% 1:5)
	stopifnot(version[1] == 3.6)
	filename <- file.path(path, paste0("gadm36_adm", level, "_r", resolution, "_pk.rds"))
	.gadm_download(filename, version[1], "")
}


gadm <- function(country, level=1, path, version=3.6, ...) {

	stopifnot(file.exists(path))
	country <- .getCountryISO(country)

	if (missing(level)) {
		stop('provide a "level=" argument; levels can be 0, 1, or 2 for most countries, and higher for some')
	}
	stopifnot(version == 3.6)
	filename <- file.path(path, paste0('gadm36_', country, '_', level, "_pk.rds"))
	.gadm_download(filename, version)
}


