

.cropland_africa <- function(path, ...) {

	path <- .get_path(path)
	filename <- paste0("geosurvey_cropland.tif")
	filepath <- file.path(path, filename)

	if (!(file.exists(filepath))) {
		url <- paste0(.data_url(), "landuse/", filename)
		if (!.downloadDirect(url, filepath, ...)) return(NULL)
		
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



.cropland_world <- function(path, ...) {

	path <- .get_path(path)
	filename <- paste0("WorldCover_cropland_30s.tif")
	filepath <- file.path(path, filename)

	if (!(file.exists(filepath))) {
		url <- paste0(.data_url(), "landuse/", filename)
		if (!.downloadDirect(url, filepath, ...)) return(NULL)
		
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




.cropland_glad <- function(path, year, ...) {

	path <- .get_path(path)
	if (missing(year)) {
		filename <- "glad_cropland.tif"
	} else {
		year <- as.numeric(year)
		stopifnot(year %in% c(2003, 2007, 2011, 2015, 2019))
		filename <- paste0("glad_cropland_", year, ".tif")
	}
	filepath <- file.path(path, filename)
	if (!(file.exists(filepath))) {
		url <- paste0(.data_url(), "cropland/", filename)
		if (!.downloadDirect(url, filepath, ...)) return(NULL)
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

cropland <- function(source, path, year, ...) {

	path <- .get_path(path)
	source = match.arg(trimws(tolower(source)), c("qed", "worldcover", "glad"))
	if (source == "qed") {
		.cropland_africa(path, ...)
	} else if (source == "worldcover") {
		.cropland_world(path, ...)	
	} else {
		.cropland_glad(path, year, ...)
	}
}


landcover <- function(var, path, ...) {

	path <- .get_path(path)
	
	cats <- c("trees", "grassland", "shrubs", "cropland", "built", "bare", "snow", "water", "wetland", "mangroves", "moss")	
	var <- tolower(var)
	if (!(var %in% cats)) {
		stop(paste(var, "is not a valid name"))
	}
	
	filename <- paste0("WorldCover_", var, "_30s.tif")
	filepath <- file.path(path, filename)

	if (!(file.exists(filepath))) {
		url <- paste0(.data_url(), "landuse/", filename)
		if (!.downloadDirect(url, filepath, ...)) return(NULL)
		
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


footprint <- function(year=2009, path, ...) {
	path <- .get_path(path)
	
	year <- as.character(year)
	stopifnot(year %in% c("1993", "2009"))
	filename <- paste0("wildareas-v3-", year, "-human-footprint_geo.tif")
	filepath <- file.path(path, filename)

	if (!(file.exists(filepath))) {
		url <- paste0(.data_url(), "footprint/", filename)
		if (!.downloadDirect(url, filepath, ...)) return(NULL)
		
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

