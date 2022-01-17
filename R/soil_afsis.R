

soil_af <- function(var, depth=20, path, ...) {

	.check_path(path)
	
	var <- tolower(var)
	knownvars <- c("clay", "sand", "silt", "coarse", "SOC", "BLKD", "poros", "AWpF2.0", "AWpF2.3", "AWpF2.5", "AWpF4.2", "BDR", "pH", "ECN", "acid-exch", "bases-exch", "CEC", "Al-extr", "Al-exch", "Ca-exch", "K-exch", "Mg-exch", "Na-exch", "Ntot")
	if (!(var %in% tolower(knownvars))) {
		stop(paste("var should be one of:", knownvars))
	}

	dpts <- c("5", "15", "20", "30", "50", "60", "100", "200")
	if (!(depth %in% dpts)) {
		stop(paste("depth must be one of:", paste(dpts, collapse=", ")))
	}
	depth <- c("0-5", "5-15", "0-20", "15-30", "20-50", "30-60", "60-100", "100-200")[depth == dpts]
	
	filename <- paste0("af_", var, "_", depth, "cm_30s.tif")
	filepath <- file.path(path, filename)
	url <- paste0("https://biogeo.ucdavis.edu/data/geodata/soil/afsis/", filename)

	.donwload_url(url, filepath, ...)
}



soil_af_elements <- function(var, path, ...) {

	.check_path(path)
	
	var <- tolower(var)
	stopifnot(var %in% c("al", "b", "ca", "cu", "fe", "k", "mg", "mn", "n", "na", "p", "ptot", "zn"))

	filename <- paste0("af_", var, "_0-30cm_30s.tif")
	filepath <- file.path(path, filename)

	url <- paste0("https://biogeo.ucdavis.edu/data/geodata/soil/afsis_nuts/", filename)
	.donwload_url(url, filepath, ...)
}


