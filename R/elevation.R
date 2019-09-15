# Get elevation data
# Author: Robert J. Hijmans (modified by Aniruddha Ghosh)
# License GPL3
# Version 0.1
# March 2016

elevation_3s <- function(lon, lat, path) {
	stopifnot(file.exists(path))
	stopifnot(lon >= -180 & lon <= 180)
	stopifnot(lat >= -60 & lat <= 60)

	rs <- raster(nrows=24, ncols=72, xmn=-180, xmx=180, ymn=-60, ymx=60 )
	rowTile <- rowFromY(rs, lat)
	colTile <- colFromX(rs, lon)
	if (rowTile < 10) { rowTile <- paste("0", rowTile, sep="") }
	if (colTile < 10) { colTile <- paste("0", colTile, sep="") }

	f <- paste("srtm_", colTile, "_", rowTile, sep="")
	zipfilename <- paste(path, "/", f, ".ZIP", sep="")
	tiffilename <- paste(path, "/", f, ".tif", sep="")

	if (!file.exists(tiffilename)) {
		if (!file.exists(zipfilename)) {
			theurl <- paste("http://srtm.csi.cgiar.org/SRT-ZIP/SRTM_V41/SRTM_Data_GeoTiff/", f, ".zip", sep="")
			test <- try (.download(theurl, zipfilename) , silent=TRUE)
			if (class(test) == "try-error") {
				theurl <- paste("http://droppr.org/srtm/v4.1/6_5x5_TIFs/", f, ".zip", sep="")
				test <- try (.download(theurl, zipfilename) , silent=TRUE)
				if (class(test) == "try-error") {
					theurl <- paste("ftp://xftp.jrc.it/pub/srtmV4/tiff/", f, ".zip", sep="")
					.download(theurl, zipfilename)
				}
			}
		}
		if (file.exists(zipfilename)) {
			utils::unzip(zipfilename, exdir=dirname(zipfilename))
			file.remove(zipfilename)
		}
	}
	if (file.exists(tiffilename)) {
		rs <- rast(tiffilename)
		crs(rs) <- "+proj=longlat +datum=WGS84"
		return(rs)
	} else {
		stop("file not found")
	}
}

elevation_30s <- function(country, mask=TRUE, path, ...) {
	country <- .getCountryISO(country)
	if (mask) {
		mskname <- "_msk_"
		mskpath <- "msk_"
	} else {
		mskname<-"_"
		mskpath <- ""
	}
	filename <- file.path(path, paste0(country, mskname, name, ".grd"))
	if (!file.exists(filename)) {
		zipfilename <- filename
		extension(zipfilename) <- ".zip"
		if (!file.exists(zipfilename)) {
			theurl <- paste("http://biogeo.ucdavis.edu/data/diva/", mskpath, name, "/", country, mskname, name, ".zip", sep="")
			.download(theurl, zipfilename)
			if (!file.exists(zipfilename))	{
				message("\nCould not download file -- perhaps it does not exist")
			}
		}
	}
    ff <- utils::unzip(zipfilename, exdir=dirname(zipfilename))
    file.remove(zipfilename)
	if (file.exists(filename)) {
		rs <- raster(filename)
	} else {
    #patrn <- paste(country, ".", mskname, name, ".grd", sep="")
    #f <- list.files(path, pattern=patrn)
		f <- ff[substr(ff, .nchar(ff)-3, .nchar(ff)) == ".grd"]
		if (length(f)==0) {
			warning("something went wrong")
			return(NULL)
		} else if (length(f)==1) {
			rs <- rast(f)
		} else {
			rs <- sapply(f, rast)
			rs <- lapply(rs, function(i) crs(i) <- "+proj=longlat +datum=WGS84")
			message("returning a list of SpatRaster objects")
			return(rs)
		}
	}
	crs(rs) <- "+proj=longlat +datum=WGS84"
	return(rs)
}

