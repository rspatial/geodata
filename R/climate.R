
wordclim30s <- function(var, lon, lat) {
	lon <- min(180, max(-180, lon))
	lat <- min(90, max(-60, lat))
	rs <- raster(nrows=5, ncols=12, xmn=-180, xmx=180, ymn=-60, ymx=90 )
	row <- rowFromY(rs, lat) - 1
	col <- colFromX(rs, lon) - 1
	rc <- paste(row, col, sep="")
	zip <- paste(var, "_", rc, ".zip", sep="")
	zipfile <- file.path(path, zip)
	if (var  == "alt") {
		tiffiles <- paste(var, "_", rc, ".tif", sep="")
	} else if (var  != "bio") {
		tiffiles <- paste(var, 1:12, "_", rc, ".tif", sep="")
	} else {
		tiffiles <- paste(var, 1:19, "_", rc, ".tif", sep="")
	}
	theurl <- paste("http://biogeo.ucdavis.edu/data/climate/worldclim/2.1/tiles/cur/", zip, sep="")

	files <- c(paste(path, tiffiles, sep=""), paste(path, tiffiles, sep=""))
	fc <- sum(file.exists(files))

	if ( fc < length(files) ) {
		if (!file.exists(zipfile)) {
			.download(theurl, zipfile)
			if (!file.exists(zipfile))	{
			  message("\n Could not download file -- perhaps it does not exist")
			}
		} else {
			message("File not available locally. Use 'download = TRUE'")
		}
	}
	utils::unzip(zipfile, exdir=dirname(zipfile))
	st <- rast(paste(path, tiffiles, sep=""))
	return(st)	
}


worldclim <- function(var, res) {
	stopifnot(res %in% c(2.5, 5, 10))
	if (res==2.5) { res <- "2-5" }
	stopifnot(var %in% c("tmean", "tmin", "tmax", "prec", "bio", "alt"))
	path <- file.path(path, paste0("wc", res, "/"))
	dir.create(path, showWarnings=FALSE)

	zip <- paste(var, "_", res, "m.zip", sep="")
	zipfile <- paste(path, zip, sep="")
	if (var  == "alt") {
		tiffiles <- paste(var, ".tif", sep="")
	} else if (var  != "bio") {
		tiffiles <- paste(var, 1:12, ".tif", sep="")
	} else {
		tiffiles <- paste(var, 1:19, ".tif", sep="")
	}
	theurl <- paste("http://biogeo.ucdavis.edu/data/climate/worldclim/2.1/cur/", zip, sep="")
	files <- file.path(path, tiffiles)
	fc <- sum(file.exists(files))

	if ( fc < length(files) ) {
		if (!file.exists(zipfile)) {
			.download(theurl, zipfile)
			if (!file.exists(zipfile))	{
				message("\n Could not download file -- perhaps it does not exist.")
			}
		}
		utils::unzip(zipfile, exdir=dirname(zipfile))
	}
	st <- rast(files)
	return(st)
}

