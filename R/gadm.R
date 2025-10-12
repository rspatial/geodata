
.check_gadm <- function(filename, gversion) {
	f <- paste0("https://geodata.ucdavis.edu/gadm/gadm", gversion, ".txt")
	ff <- readLines(f, warn=FALSE)
	if (!(gsub("_low", "", basename(filename)) %in% ff)) {
		return(FALSE)
	}
	TRUE
}


.gadm_download <- function(f, path, gversion, upath="pck", check=TRUE, ...) {

	path <- .get_path(path, add="gadm")
	filename <- file.path(path, f)

	if (file.exists(filename)) {
		r <- try(readRDS(filename), silent=TRUE)
		if (!inherits(r, "try-error")) {
			return(r)
		} else {
			file.remove(filename)
		}
	}
	
	durl <- .data_url()
	if (is.null(durl)) return(NULL)
	baseurl <- paste0(dirname(durl), "/gadm/gadm", gversion)
	
	if (check) {
		exists <- try(.check_gadm(filename, gversion), silent=TRUE)
		if (!inherits(exists, "try-error")) {
			if (!exists) {
				message(f, " - this file does not exist") 
				return(NULL)
			}
		}
	}
	if (upath=="") {
		theurl <- file.path(baseurl, basename(filename))		
	} else {
		theurl <- file.path(baseurl, upath, basename(filename))
	}
	if (!.downloadDirect(theurl, filename, ...)) return(NULL)
	
	
	if (file.exists(filename)) {
		r <- try(readRDS(filename), silent=TRUE)
		if (inherits(r, "try-error")) {
			file.remove(filename)
			message("something went wrong")
		} else {
			r
		}
	} else {
		message("something went wrong")
		NULL
	}
}


world <- function(resolution=5, level=0, path, version="latest", ...) {
	stopifnot(level[1] == 0)
	resolution = round(resolution[1])
	if (!(resolution %in% 0:5)) stop("not a valid resolution")
	version <- as.character(version)
#	if (version == "latest") version <- "4.1"
#	stopifnot(version[1] %in% c("3.6", "4.1"))
	if (version == "latest") version <- "3.6"
	stopifnot(version[1] %in% c("3.6"))
	fversion <- gsub("\\.", "", version)
	if (fversion == "36") {
		filename <- paste0("gadm36_adm", level, "_r", resolution, "_pk.rds")
	} else {
		filename <- paste0("gadm_", fversion, "_adm", level, "_r", resolution, ".rds")	
	}
	.gadm_download(filename, path, version[1], "", check=FALSE, ...)
}


gadm <- function(country, level=1, path, version="latest", resolution=1, ...) {

	if (length(level) > 1) {
		stop("level can only have a single value", call. = FALSE)
	}

	version <- as.character(version[1])
	if (version == "latest") version <- "4.1"
	stopifnot(version[1] %in% c("3.6", "4.0", "4.1"))

	resolution = round(resolution[1])
	stopifnot(resolution %in% 1:2)
	if ((resolution != 1) && (version < "4.1")) {
		warning("country level low resolution is only available for version >= 4.1")
		resolution <- 1
	}

	country <- unique(country)
	if (length(country) > 1) {
		x <- lapply(country, function(i) { gadm(i, level=level, path=path, version=version, resolution=resolution, ...) })
		return( do.call(rbind, x))
	}
	country <- .getCountryISO(country)
	fversion <- gsub("\\.", "", version)
	if (version[1] < "4.1") {
		filename <- paste0("gadm", fversion, "_", country, "_", level, "_pk.rds")
	} else {
		if (resolution == 1) {
			filename <- paste0("gadm", fversion, "_", country, "_", level, "_pk.rds")
		} else {
			filename <- paste0("gadm", fversion, "_", country, "_", level, "_pk_low.rds")
		}
	}
	v <- .gadm_download(filename, path, version[1], ...)
	if ((!is.null(v)) && (nrow(v) == 0)) {
		stop(paste(country, "level", level, "is not available"), call. = FALSE) 
	}
	v
}


