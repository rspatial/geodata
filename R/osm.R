

osm <- function(country, var, path, proxy=FALSE, ...) {
	stopifnot(var %in% c("places", "highways", "railway"))
	iso <- .getCountryISO(country)
	.check_path(path)

	filename <- paste0(iso, "_", var, ".gpkg")
	filepath <- file.path(path, filename)

	if (!(file.exists(filepath))) {
		url <- paste0(.data_url(), "osm/", var, "/", filename)
		.downloadDirect(url, filepath, ...)
	} 
	vect(filepath, proxy=proxy)
}

	

