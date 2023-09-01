

soil_af <- function(var, depth=20, path, ...) {

	if (length(var) > 1) {
		r <- lapply(var, function(v) soil_af(v, depth, path, ...))
		return(rast(r))
	}

	path <- .get_path(path, add="soil_af")

	depth <- depth[1]
	var <- tolower(var)
	
	knownvars <- c("clay", "sand", "silt", "coarse", "SOC", "BLKD", "poros", "AWpF2.0", "AWpF2.3", "AWpF2.5", "AWpF4.2", "BDR", "pH", "ECN", "acid-exch", "bases-exch", "CEC", "Al-extr", "Al-exch", "Ca-exch", "K-exch", "Mg-exch", "Na-exch", "Ntot")
	if (!(var %in% tolower(knownvars))) {
		stop(paste("var should be one of:", paste(knownvars, collapse=", ")))
	}

	dpts <- c("5", "15", "20", "30", "50", "60", "100", "200")
	if (!(depth %in% dpts)) {
		stop(paste("depth must be one of:", paste(dpts, collapse=", ")))
	}
	depth <- c("0-5", "5-15", "0-20", "15-30", "20-50", "30-60", "60-100", "100-200")[depth == dpts]
	
	filename <- paste0("af_", var, "_", depth, "cm_30s.tif")
	filepath <- file.path(path, filename)
	url <- .data_url(paste0("soil/afsis/", filename))
	if (is.null(url)) return(NULL)

	.donwload_url(url, filepath, ...)
}



soil_af_elements <- function(var, path, ...) {


	if (length(var) > 1) {
		r <- lapply(var, function(v) soil_af_elements(v, path, ...))
		return(rast(r))
	}

	path <- .get_path(path, "soil_af")
	
	var <- tolower(var)
	stopifnot(var %in% c("al", "b", "ca", "cu", "fe", "k", "mg", "mn", "n", "na", "p", "ptot", "zn"))

	filename <- paste0("af_", var, "_0-30cm_30s.tif")
	filepath <- file.path(path, filename)

	url <- .data_url(paste0("soil/afsis_nuts/", filename))
	if (is.null(url)) return(NULL)

	.donwload_url(url, filepath, ...)
}


soil_af_water <- function(var, depth="30cm", path, ...) {

	var <- tolower(var)
	depth <- depth[1]
	
	if (length(var) > 1) {
		r <- lapply(var, function(v) soil_af_water(v, path, ...))
		return(rast(r))
	}

	path <- .get_path(path, "soil_af")
	stopifnot(var %in% c("awcpf23", "pwp", "crfvol", "tetas", "erzd", "tawcpf23", "tawcpf23mm"))

	if (var == "erzd") {
		filename <- "af_erzd.tif"
	} else if (depth == "erzd") {
		filename <- paste0("af_erzd_", var, ".tif")
	} else if (depth == "30cm") {
		filename <- paste0("af_30cm_", var, ".tif")	
	} else {
		stop("not a valid depth")
	}
	filepath <- file.path(path, filename)

	url <- .data_url(paste0("soil/gyga/", filename))
	if (is.null(url)) return(NULL)
	
	.donwload_url(url, filepath, ...)
}

