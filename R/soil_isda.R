

soil_af_isda <- function(var, depth=20, error=FALSE, path, ...) {
	stopifnot(dir.exists(path))


	var <- tolower(var)
	vars <- c("al", "bdr", "clay", "c.tot", "ca", "db.od", "ecec.f", "fe", "k", "mg", "n.tot", "oc", "p", "ph.h2o", "sand", "silt", "s", "texture", "wpg2", "zn")
	if (!(var %in% vars)) {
		stop(paste("unknown variable. Use one of:\n", vars))
	}
	
	if (var %in% c("clay", "silt", "sand")) {
		var <- paste0(var, ".tot.psa")
	} else if (var == "n.tot") {
		var <- "n.tot.ncs"
	} else if (var == "texture") {
		var <- "texture.class"	
	}

	if (var == "bdr") {
		depth = "0-200cm"
	} else {
		stopifnot(as.numeric(depth) %in% c(20, 50))
		depth <- ifelse(depth == 20, "0-20cm", "20-50cm")
	}
	
	if (error) var <- paste0(var, "-error")
	
	version <- 0.13
	#stopifnot(version == 0.13)
	
	filename <- paste0("isda_", var, "_", depth, "_v", version,  "_30s.tif")
	filepath <- file.path(path, filename)


	if (!(file.exists(filepath))) {
		burl <- "https://geodata.ucdavis.edu/geodata/soil/isda/"
		url <- file.path(burl, filename)
		.downloadDirect(url, filepath, ...)
		if (file.exists(filepath) && grepl("texture", filename)) {
			url <- file.path(burl, paste0(filename, ".aux.xml"))
			.downloadDirect(url, paste0(filepath, ".aux.xml"), ...)
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


