
.gadm_download <- function(filename, gversion, upath="pck", check=TRUE, ...) {
	
	.check_path(dirname(filename))

	if (!file.exists(filename)) {

		if (check) {
			f <- paste0("https://geodata.ucdavis.edu/gadm/gadm", gversion, ".txt")
			ff <- readLines(f)
			if (!(gsub("_low", "", basename(filename)) %in% ff)) {
				return(vect())
			}
		}
		baseurl <- paste0(dirname(.data_url()), "/gadm/gadm", gversion)
		if (upath=="") {
			theurl <- file.path(baseurl, basename(filename))		
		} else {
			theurl <- file.path(baseurl, upath, basename(filename))
		}
		.download(theurl, filename, ...)
		if (!file.exists(filename))	{ 
			message("\nCould not download file -- perhaps it does not exist") 
		}
	}	
	if (file.exists(filename)) {
		r <- readRDS(filename)
		r@crs <- "+proj=longlat +datum=WGS84"
		return( vect(r) ) 
	} else {
		return(NULL)
	}
}


world <- function(resolution=5, level=0, path, version="3.6", ...) {
	stopifnot(level[1] == 0)
	resolution = round(resolution[1])
	stopifnot(resolution %in% 1:5)
	version <- as.character(version)
	if (version == "latest") version <- "3.6"
	stopifnot(version[1] == "3.6")
	filename <- file.path(path, paste0("gadm36_adm", level, "_r", resolution, "_pk.rds"))
	.gadm_download(filename, version[1], "", check=FALSE, ...)
}


gadm <- function(country, level=1, path, version="latest", resolution=1, ...) {

	if (length(level) > 1) {
		stop("level can only have a single value", call. = FALSE)
	}
	path <- path[1]

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
		filename <- file.path(path, paste0("gadm", fversion, "_", country, "_", level, "_pk.rds"))
	} else {
		if (resolution == 1) {
			filename <- file.path(path, paste0("gadm", fversion, "_", country, "_", level, "_pk.rds"))	
		} else {
			filename <- file.path(path, paste0("gadm", fversion, "_", country, "_", level, "_pk_low.rds"))
		}
	}
	v <- .gadm_download(filename, version[1], ...)
	if (nrow(v) == 0) {
		stop(paste(country, "level", level, "is not available"), call. = FALSE) 
	}
	v
}


