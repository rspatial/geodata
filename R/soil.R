

soil_africa <- function(var, depth=20, version=0.13, path) {

	stopifnot(depth %in% c(20,50))
	
	stopifnot(var %in% c("ph_h2o", "log.ecec.f", "log.oc", "log.ca_mehlich3", "log.k_mehlich3", "log.p_mehlich3"))
	stopifnot(dir.exists(path))
	
	
	stopifnot(version == 0.13)
	
	depth <- ifelse(depth == 20, "0-20cm", "20-50cm")

	filename <- paste0("isda", "_", var, "_", depth, "_v", version,  ".tif")
	filepath <- file.path(path, filename)

	if (!(file.exists(filepath))) {
		url <- paste0("https://biogeo.ucdavis.edu/data/geodata/soil/", filename)
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


