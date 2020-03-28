# Get elevation data
# Author: Robert J. Hijmans (modified by Aniruddha Ghosh)
# License GPL3
# Version 0.1
# March 2016

elevation_3s <- function(lon, lat, path, ...) {
	stopifnot(file.exists(path))
	stopifnot(lon >= -180 & lon <= 180)
	stopifnot(lat >= -60 & lat <= 60)

	rs <- rast(res=5, ymin=-60, ymax=60 )
	rowTile <- formatC(rowFromY(rs, lat), width=2, flag=0)
	colTile <- formatC(colFromX(rs, lon), width=2, flag=0)
	
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

elevation_30s <- function(country, path, mask=TRUE, subs="", ...) {
	iso3 <- .getCountryISO(country)
	if (mask) {
		mskname <- "_msk"
	} else {
		mskname<- ""
	}
	f <- paste0(iso3, "_elv", mskname, subs)
	filename <- file.path(path, paste0(f, ".tif"))
	if (!file.exists(filename)) {
		zipfilename <- gsub("\\.tif$", ".zip", filename)
		if (!file.exists(zipfilename)) {
			theurl <- paste0("http://biogeo.ucdavis.edu/data/geodata/elv/", f, ".zip")
			.download(theurl, zipfilename)
			if (!file.exists(zipfilename))	{
				message("\nCould not download file -- perhaps it does not exist")
			}
		}
		ff <- utils::unzip(zipfilename, exdir=dirname(zipfilename))
		file.remove(zipfilename)
	}
	if (file.exists(filename)) {
		rs <- rast(filename)
	} else {
		stop("something went wrong")
	}
	return(rs)
}

