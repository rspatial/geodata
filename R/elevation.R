# Get elevation data
# Author: Robert J. Hijmans (modified by Aniruddha Ghosh)
# License GPL3
# Version 0.1
# March 2016

elevation_1s <- function(lon, lat, path, ...) {

	path <- .get_path(path)
	
	stopifnot(lon >= -180 & lon <= 180)
	stopifnot(lat >= -57 & lat <= 61)

	rs <- rast(res=5, ymin=-60, ymax=60 )
	rowTile <- formatC(rowFromY(rs, lat), width=2, flag=0)
	colTile <- formatC(colFromX(rs, lon), width=2, flag=0)
	
	f <- paste("srtm_", colTile, "_", rowTile, sep="")
	tiffilename <- paste(path, "/", f, ".tif", sep="")

	if (!file.exists(tiffilename)) {
		pzip <- paste(path, "/", f, ".ZIP", sep="")
		theurl <- paste("https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/", f, ".zip", sep="")
		if (!.downloadDirect(theurl, pzip, unzip=TRUE, ...)) return(NULL)
	}
	if (file.exists(tiffilename)) {
		rs <- rast(tiffilename)
		crs(rs) <- "+proj=longlat +datum=WGS84"
		return(rs)
	} else {
		stop("file not found")
	}
}

elevation_3s <- function(lon, lat, path, ...) {

	path <- .get_path(path)
	
	stopifnot(lon >= -180 & lon <= 180)
	stopifnot(lat >= -60 & lat <= 60)

	rs <- rast(res=5, ymin=-60, ymax=60 )
	rowTile <- formatC(rowFromY(rs, lat), width=2, flag=0)
	colTile <- formatC(colFromX(rs, lon), width=2, flag=0)
	
	f <- paste("srtm_", colTile, "_", rowTile, sep="")
	tiffilename <- paste(path, "/", f, ".tif", sep="")

	if (!file.exists(tiffilename)) {
		pzip <- paste(path, "/", f, ".ZIP", sep="")
		theurl <- paste("https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/", f, ".zip", sep="")
		if (!.downloadDirect(theurl, pzip, unzip=TRUE, ...)) return(NULL)
	}
	if (file.exists(tiffilename)) {
		rs <- rast(tiffilename)
		crs(rs) <- "+proj=longlat +datum=WGS84"
		return(rs)
	} else {
		stop("file not found")
	}
}

elevation_30s <- function(country, path, mask=TRUE, subs="", ...) {

	path <- .get_path(path)
	iso3 <- .getCountryISO(country)
	if (mask) {
		mskname <- "_msk"
	} else {
		mskname<- ""
	}
	f <- paste0(iso3, "_elv", mskname, subs)
	filename <- file.path(path, paste0(f, ".tif"))
	if (!file.exists(filename)) {
		pzip <- gsub("\\.tif$", ".zip", filename)
		theurl <- .data_url(paste0("elv/", f, ".zip"))
		if (is.null(theurl)) return(NULL)
		if (!.downloadDirect(theurl, pzip, unzip=TRUE, ...)) return(NULL)
	}
	rast(filename)
}



elevation_global <- function(res, path, ...) {

	path <- .get_path(path)
	res <- as.character(res)
	stopifnot(res %in% c("2.5", "5", "10", "0.5"))
	fres <- ifelse(res=="0.5", "30s", paste0(res, "m"))
	path <- file.path(path, paste0("wc2.1_", fres, "/"))
	zip <- paste0("wc2.1_", fres, "_elev.zip")
	ff <- paste0("wc2.1_", fres, "_elev.tif")
	pzip <- file.path(path, zip)
	ff <- file.path(path, ff)
	if (!file.exists(ff)) {
		dir.create(path, showWarnings=FALSE)
		theurl <- .wc_url(paste0("base/", zip))
		if (is.null(theurl)) return(NULL)
		if (!.downloadDirect(theurl, pzip, unzip=TRUE, ...)) return(NULL)
	}
	rast(ff)
}


