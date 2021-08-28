

soil_af_isda <- function(var, depth=20, error=FALSE, path) {

	stopifnot(as.numeric(depth) %in% c(20, 50))
	depth <- ifelse(depth == 20, "0-20cm", "20-50cm")
	
	stopifnot(var %in% c("al", "bdr", "clay", "c.tot", "ca", "db.od", "ecec.f", "fe", "k", "mg", "n.tot", "oc", "p", "ph.h2o", "sand", "silt", "s", "texture", "wpg2", "zn"))
	stopifnot(dir.exists(path))
	
	if (error) var <- paste0(var, "-error")
	
	version <- 0.13
	#stopifnot(version == 0.13)
	
	filename <- paste0("isda_", var, "_", depth, "_v", version,  "_30s.tif")
	filepath <- file.path(path, filename)

	if (!(file.exists(filepath))) {
		url <- paste0("https://biogeo.ucdavis.edu/data/geodata/soil/isda/", filename)
		utils::download.file(url, filepath, mode="wb")
		if (file.exists(filepath) && grepl("texture", filename)) {
			url <- paste0("https://biogeo.ucdavis.edu/data/geodata/soil/isda/", paste0(filename, ".aux.xml"))
			try(utils::download.file(url, paste0(filepath, ".aux.xml"), mode="wb"), silent=TRUE)
		}
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


