

osm <- function(country, var, path, proxy=FALSE, ...) {
	stopifnot(var %in% c("places", "highways", "railway"))
	iso <- .getCountryISO(country)
	path <- .get_path(path)

	filename <- paste0(iso, "_", var, ".gpkg")
	filepath <- file.path(path, filename)

	if (!(file.exists(filepath))) {
		url <- paste0(.data_url(), "osm/", var, "/", filename)
		if (!.downloadDirect(url, filepath, ...)) return(NULL)
	} 
	vect(filepath, proxy=proxy)
}

	

