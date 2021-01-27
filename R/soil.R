

soil_africa <- function(var, depth=20, version=13, path, ...) {

	stopifnot(depth == 20)
#	stopifnot(depth %in% c(20,50))
	
	stopifnot(var %in% c("ph", "cec", "orgC"))
	stopifnot(dir.exists(path))
	stopifnot(version == 13)
	
	
	if (depth == 20) {
		depth <- "0-20cm"
	} else {
		depth <- "20-50cm"	
	}
	filename <- paste0("isda", version, "_", var, "_", depth, ".tif")
	filepath <- file.path(path, filename)

	if (!(file.exists(filepath))) {
		url <- paste0("https://biogeo.ucdavis.edu/data/geodata/soil/", filename)
		utils::download.file(url, filepath, mode="wb")
		r <- try(rast(filepath))
		if (class(r) == "try-error") {
			try(file.remove(filepath), silent=TRUE)
			stop("download failed")
		}
		return(r)
	}
	rast(ff)
}


