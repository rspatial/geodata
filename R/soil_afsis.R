
.donwload_url <- function(url, filepath) {
	if (!(file.exists(filepath))) {
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



soil_af <- function(var, depth=20, path) {

	stopifnot(dir.exists(path))
	
	var <- tolower(var)

	stopifnot(var %in% tolower(c("clay", "sand", "silt", "coarse", "SOC", "BLKD", "poros", "AWpF2.0", "AWpF2.3", "AWpF2.5", "AWpF4.2", "BDR", "pH", "ECN", "acid-exch", "bases-exch", "CEC", "Al-extr", "Al-exch", "Ca-exch", "K-exch", "Mg-exch", "Na-exch", "Ntot")))

	dpts <- c("5", "15", "30", "60", "100", "200")
	if (!(depth %in% dpts)) {
		stop(paste("depth must be one of:", paste(dpts, collapse=", ")))
	}
	depth <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")[depth == dpts]
	
	filename <- paste0("af_", var, "_", depth, "cm_30s.tif")
	filepath <- file.path(path, filename)
	url <- paste0("https://biogeo.ucdavis.edu/data/geodata/soil/afsis/", filename)

	.donwload_url(url, filepath)
}



soil_af_elements <- function(var, path) {

	stopifnot(dir.exists(path))
	
	var <- tolower(var)
	stopifnot(var %in% c("al", "b", "ca", "cu", "fe", "k", "mg", "mn", "n", "na", "p", "ptot", "zn"))

	filename <- paste0("af_", var, "_0-30cm_30s.tif")
	filepath <- file.path(path, filename)

	url <- paste0("https://biogeo.ucdavis.edu/data/geodata/soil/afsis/", filename)
	.donwload_url(url, filepath)
}


