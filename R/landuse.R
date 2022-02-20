

cropland_africa <- function(path, ...) {

	.check_path(path)
	filename <- paste0("geosurvey_cropland.tif")
	filepath <- file.path(path, filename)

	if (!(file.exists(filepath))) {
		url <- paste0(.data_url(), "landuse/", filename)
		.downloadDirect(url, filepath, ...)
		
		r <- try(rast(filepath))
		if (class(r) == "try-error") {
			try(file.remove(filepath), silent=TRUE)
			stop("download failed")
		}
	} else {
		r <- rast(filepath)
	}
	r	
}



cropland_world <- function(path, ...) {
	.check_path(path)
	filename <- paste0("WorldCover_cropland_30s.tif")
	filepath <- file.path(path, filename)

	if (!(file.exists(filepath))) {
		url <- paste0(.data_url(), "landuse/", filename)
		.downloadDirect(url, filepath, ...)
		
		r <- try(rast(filepath))
		if (class(r) == "try-error") {
			try(file.remove(filepath), silent=TRUE)
			stop("download failed")
		}
	} else {
		r <- rast(filepath)
	}
	r	
}
