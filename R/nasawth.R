
get_extents <- function(e) {
	sx <- ceiling((e$xmax - e$xmin) / 10)
	sy <- ceiling((e$ymax - e$ymin) / 10)
	r <- terra::as.polygons(terra::rast(e, nrow=sy, ncol=sx))
	lapply(1:nrow(r), function(i) ext(r[i]))
}

powerWeather <- function(year, var, ext, path) {

	path <- .get_path(path, "weather/power")

	x <- paste(round(as.vector(ext),3), collapse="x")
	if (length(year) == 1) {
		f <- file.path(path, paste0(var, "-", year, "-", x, ".nc"))
	} else {
		yrs <- range(year)
		f <- file.path(path, paste0(var, "-", yrs[1], "_", yrs[2], "-", x, ".nc"))	
	}
	tmppath <- file.path(tempdir(), paste0("nasa_", x))
	dir.create(tmppath, FALSE, FALSE)
	if (!file.exists(f)) {
		print(f); flush.console()
		ee <- get_extents(ext(ext))
		tiles <- expand.grid(year=year, ext=1:length(ee)) 		
		if (nrow(tiles) > 1) {
			fsub <- file.path(tmppath, paste0(var, "-year_", tiles[,1], "-ext_", tiles[,2], ".nc"))
		} else {
			fsub <- f
		}
		cat("tiles: ")
		for (i in 1:nrow(tiles)) {
			cat(i, " "); flush.console(); if (i%%25 == 0) cat("\n")
			if (!file.exists(fsub[i])) {
				e <- ee[[tiles$ext[i]]]
				request <- paste0("https://power.larc.nasa.gov/api/temporal/daily/regional?latitude-min=", e$ymin, "&latitude-max=", e$ymax, "&longitude-min=", e$xmin, "&longitude-max=", e$xmax, "&parameters=", var, "&community=SB&start=", tiles$year[i], "0101&end=", tiles$year[i], "1231&format=NetCDF")
				g <- httr::GET(request)
				if (g$status_code != 200) {
					stop(paste0("donwload failure for:\n", request))
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

