
isda_vars <- function() {
	old <- c("al", "bdr", "db.od", "ca", "oc", "c.tot", "cec", "clay", NA, "fe", "mg", "n.tot", "ph.h2o", "p", "k", "sand", "silt", "wpg2", "s", "texture", "zn")
	new <- c("extr_al", "bedrock", "bulkdens", "extr_ca", "org_c", "tot_c", "cec", "clay", NA, "extr_fe", "extr_mg", "tot_N", "ph.h2o", "extr_p", "extr_k", "sand", "silt", "stone", "extr_s", "texture", "extr_zn")
	isda <- c('aluminium_extractable', 'bedrock_depth', 'bulk_density', 'calcium_extractable', 'carbon_organic', 'carbon_total', 'cation_exchange_capacity', 'clay_content', 'fcc', 'iron_extractable', 'magnesium_extractable', 'nitrogen_total', 'ph', 'phosphorous_extractable', 'potassium_extractable', 'sand_content', 'silt_content', 'stone_content', 'sulphur_extractable', 'texture_class', 'zinc_extractable')
	na.omit(data.frame(old=old, new=new, isda=isda))
}


#soil_af_isda_highres <- function() {
#}

soil_af_isda <- function(var, depth=20, error=FALSE, path, virtual=FALSE, ...) {


	if (length(var) > 1) {
		r <- lapply(var, function(v) soil_af_isda(v, depth[1], error=error, path, ...))
		return(rast(r))
	}

	var <- tolower(var[1])
	vars <- c("al", "bdr", "clay", "c.tot", "ca", "db.od", "ecec.f", "fe", "k", "mg", "n.tot", "oc", "p", "ph.h2o", "sand", "silt", "s", "texture", "wpg2", "zn")
	if (!(var %in% vars)) {
		stop(paste("unknown variable. Use one of:\n", paste(vars, collapse=", ")))
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
		depth <- depth[1]
		stopifnot(as.numeric(depth) %in% c(20, 50))
		depth <- ifelse(depth == 20, "0-20cm", "20-50cm")
	}
	
	if (error) var <- paste0(var, "-error")
	
	version <- 0.13
	#stopifnot(version == 0.13)
	
	filename <- paste0("isda_", var, "_", depth, "_v", version,  "_30s.tif")

	if (virtual) {
		burl <- .data_url("soil/isdasoil/")
		if (is.null(burl)) return(NULL)
		url <- file.path(burl, filename)
		url <- paste0("/vsicurl/", url)
		return(rast(url))
	}

	path <- .get_path(path, add="soil_af_isda")
	filepath <- file.path(path, filename)

	if (!(file.exists(filepath))) {
		burl <- .data_url("soil/isda/")
		if (is.null(burl)) return(NULL)
		url <- file.path(burl, filename)
		if (!.downloadDirect(url, filepath, ...)) return(NULL)
		if (file.exists(filepath) && grepl("texture", filename)) {
			url <- file.path(burl, paste0(filename, ".aux.xml"))
			if (!.downloadDirect(url, paste0(filepath, ".aux.xml"), ...)) return(NULL)
		}
		r <- try(rast(filepath))
		if (inherits(r, "try-error")) {
			try(file.remove(filepath), silent=TRUE)
			message("download failed")
			return(NULL)
		}
	} else {
		r <- rast(filepath)
	}
	r
}


