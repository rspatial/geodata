

osm <- function(country, var, path, proxy=FALSE, ...) {
	stopifnot(var %in% c("places", "highways", "railway"))
	iso <- .getCountryISO(country)
	path <- .get_path(path, "osm")

	filename <- paste0(iso, "_", var, ".gpkg")
	filepath <- file.path(path, filename)

	if (!(file.exists(filepath))) {
		url <- .data_url(paste0("osm/", var, "/", filename))
		if (is.null(url)) return(NULL)
		if (!.downloadDirect(url, filepath, ...)) return(NULL)
	} 
	vect(filepath, proxy=proxy)
}

	

