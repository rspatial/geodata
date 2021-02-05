

cropland_africa <- function(path) {

	stopifnot(dir.exists(path))
	filename <- paste0("geosurvey_cropland.tif")
	filepath <- file.path(path, filename)

	if (!(file.exists(filepath))) {
		url <- paste0("https://biogeo.ucdavis.edu/data/geodata/landuse/", filename)
		utils::download.file(url, filepath, mode="wb")
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


