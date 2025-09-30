
.get_extents <- function(e) {
	sx <- ceiling((e$xmax - e$xmin) / 10)
	sy <- ceiling((e$ymax - e$ymin) / 10)
	r <- terra::as.polygons(terra::rast(e, nrow=sy, ncol=sx))
	lapply(1:nrow(r), function(i) ext(r[i]))
}

.powerWeather <- function(year, var, ext, community="", path) {

	path <- .get_path(path, "weather/power")

	x <- paste(round(as.vector(ext),3), collapse="x")
	this_year <- format(Sys.time(), "%Y")
	stopifnot(all((year > 1980) & (year < this_year)))
	if (length(year) == 1) {
		f <- file.path(path, paste0(var, "-", year, "-", x, ".nc"))
	} else {
		yrs <- range(year)
		f <- file.path(path, paste0(var, "-", yrs[1], "_", yrs[2], "-", x, ".nc"))	
	}
	
	baseurl <- "https://power.larc.nasa.gov/api/temporal/daily/regional?"
	if (community != "") {
		baseurl <- paste0(baseurl, "community=", community, "&")
	}
	
	tmppath <- file.path(tempdir(), paste0("nasa_", x))
	dir.create(tmppath, FALSE, FALSE)
	if (!file.exists(f)) {
		print(f); utils::flush.console()
		ee <- .get_extents(ext(ext))
		tiles <- expand.grid(year=year, ext=1:length(ee)) 		
		if (nrow(tiles) > 1) {
			fsub <- file.path(tmppath, paste0(var, "-year_", tiles[,1], "-ext_", tiles[,2], ".nc"))
		} else {
			fsub <- f
		}
		cat("tiles: ")
		for (i in 1:nrow(tiles)) {
			cat(i, " "); utils::flush.console(); if (i%%25 == 0) cat("\n")
			if (!file.exists(fsub[i])) {
				e <- ee[[tiles$ext[i]]]
				request <- paste0(baseurl, "latitude-min=", e$ymin, "&latitude-max=", e$ymax, "&longitude-min=", e$xmin, "&longitude-max=", e$xmax, "&parameters=", var, "&community=SB&start=", tiles$year[i], "0101&end=", tiles$year[i], "1231&format=NetCDF")
				g <- httr::GET(request)
				if (g$status_code != 200) {
					message(paste0("donwload failure for:\n", request))
					message(httr::content(g, "text"))
					stop()
				}
				writeBin(httr::content(g, "raw"), fsub[i])
			}
		}
		cat("\n")
		if (nrow(tiles) > 1) {
			if (length(year) == 1) {
				r <- sprc(fsub)
				r <- merge(r)
				r <- writeCDF(r, f)
			} else {
				r <- lapply(year, function(year) {
					fy <- grep(paste0("-year_", year), fsub, value=TRUE)
					merge(sprc(fy)) })
				r <- rast(r)
				r <- writeCDF(r, f)
			}
			file.remove(fsub)
		} else {
			r <- rast(f)
		}
	} else {
		r <- rast(f)	
	}
	r
}

