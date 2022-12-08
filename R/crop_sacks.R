
.old_sacks_crops <- function() {
	m <- c("Barley.Winter.crop.calendar.nc.gz", "Barley (winter)", "Barley.crop.calendar.nc.gz", "Barley (spring)", "Cassava.crop.calendar.nc.gz", "Cassava", "Cotton.crop.calendar.nc.gz", "Cotton", "Groundnuts.crop.calendar.nc.gz", "Groundnut", "Maize.crop.calendar.nc.gz", "Maize (main season)", "Maize.2.crop.calendar.nc.gz", "Maize (2nd season)", "Millet.crop.calendar.nc.gz", "Millet", "Oats.Winter.crop.calendar.nc.gz", "Oat (winter)", "Oats.crop.calendar.nc.gz", "Oat (spring)", "Potatoes.crop.calendar.nc.gz", "Potato", "Pulses.crop.calendar.nc.gz", "Pulses", "Rapeseed.Winter.crop.calendar.nc.gz", "Rapeseed", "Rice.crop.calendar.nc.gz", "Rice (main season)", "Rice.2.crop.calendar.nc.gz", "Rice (2nd season)", "Rye.Winter.crop.calendar.nc.gz", "Rye", "Sorghum.crop.calendar.nc.gz", "Sorghum (main season)", "Sorghum.2.crop.calendar.nc.gz", "Sorghum (2nd season)", "Soybeans.crop.calendar.nc.gz", "soybean", "Sugarbeets.crop.calendar.nc.gz", "sugarbeet", "Sunflower.crop.calendar.nc.gz", "Sunflower", "Sweet.Potatoes.crop.calendar.nc.gz", "Sweetpotato", "Wheat.Winter.crop.calendar.nc.gz", "Wheat (winter)", "Wheat.crop.calendar.nc.gz", "Wheat (spring)", "Yams.crop.calendar.nc.gz", "Yam")
	m <- matrix(m, ncol=2, byrow=TRUE)
	m[,2] <- tolower(m[,2])
	m
}

.sacks_crops <- function() {
	m <- c("Barley.Winter.crop.calendar.tif", "Barley (winter)", "Barley.crop.calendar.tif", "Barley (spring)", "Cassava.crop.calendar.tif", "Cassava", "Cotton.crop.calendar.tif", "Cotton", "Groundnuts.crop.calendar.tif", "Groundnut", "Maize.crop.calendar.tif", "Maize (main season)", "Maize.2.crop.calendar.tif", "Maize (2nd season)", "Millet.crop.calendar.tif", "Millet", "Oats.Winter.crop.calendar.tif", "Oat (winter)", "Oats.crop.calendar.tif", "Oat (spring)", "Potatoes.crop.calendar.tif", "Potato", "Pulses.crop.calendar.tif", "Pulses", "Rapeseed.Winter.crop.calendar.tif", "Rapeseed", "Rice.crop.calendar.tif", "Rice (main season)", "Rice.2.crop.calendar.tif", "Rice (2nd season)", "Rye.Winter.crop.calendar.tif", "Rye", "Sorghum.crop.calendar.tif", "Sorghum (main season)", "Sorghum.2.crop.calendar.tif", "Sorghum (2nd season)", "Soybeans.crop.calendar.tif", "soybean", "Sugarbeets.crop.calendar.tif", "sugarbeet", "Sunflower.crop.calendar.tif", "Sunflower", "Sweet.Potatoes.crop.calendar.tif", "Sweetpotato", "Wheat.Winter.crop.calendar.tif", "Wheat (winter)", "Wheat.crop.calendar.tif", "Wheat (spring)", "Yams.crop.calendar.tif", "Yam")
	m <- matrix(m, ncol=2, byrow=TRUE)
	m[,2] <- tolower(m[,2])
	m
}

sacksCrops <- function() {
	.sacks_crops()[,2]
}


crop_calendar_sacks <- function(crop="", path, ...) {

	path <- .get_path(path, "calendar/sacks")

	m <- .sacks_crops()
	crop <- tolower(crop)
	if (!(crop %in% m[,2])) {
		cat("Choose one of:\n")
		print(m[,2])
	} else {
		i <- which(m[,2] == crop)
		fout <- file.path(path, m[i,1])
		if (file.exists(fout)) {
			r <- try(terra::rast(fout), silent=TRUE)
			if (inherits(r, "try-error")) {
				file.remove(fout)
			} else {
				return(r)
			}
		}
		if (!file.exists(fout)) {
			baseurl <- paste0(.data_url(), "crops/sacks2/")
			url <- paste0(baseurl, m[i,1])
			if (!.downloadDirect(url, fout, ...)) return(NULL)
		}
		r <- terra::rast(fout)
		return(r)
	}
}


.old_crop_calendar_sacks <- function(crop="", path, ...) {

	path <- .get_path(path, "sacks")
	dir.create(path, FALSE, FALSE)

	m <- .old_sacks_crops()
	if (!(crop %in% m[,2])) {
		cat("Choose one of:\n")
		print(m[,2])
	} else {
		i <- which(m[,2] == crop)
		fout <- file.path(path, m[i,1])
		fout2 <- gsub(".gz$", "", fout)
		if (!file.exists(fout2)) {
			baseurl <- paste0(.data_url(), "crops/sacks/")
			url <- paste0(baseurl, m[i,1])
			if (!.downloadDirect(url, fout, ...)) return(NULL)
			R.utils::gunzip(fout)
		}
		r <- terra::rast(fout2)
		return(r)
	}
}


crop_calendar_rice <- function(path, ...) {
	path <- .get_path(path, "calendar/rice")
	ff <- .get_from_uri("doi:10.7910/DVN/JE6R2R", path)
	fz <- grep("zip$", ff, value=TRUE)
	if (length(fz) == 0) {
		message("something went wrong")
		return(NULL)
	}
	ok <- lapply(fz, unzip)
	fs <- grep("shp$", unlist(ok), value=TRUE)
	x <- lapply(fs, vect)
	x <- svc(x)
	names(x) <- gsub(".shp$", "", basename(fs))
	x
}
