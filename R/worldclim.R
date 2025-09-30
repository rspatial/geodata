
.wcts_month <- function(var, path, time, ...) {

	stopifnot(res %in% c("2.5", "5", "10"))
	stopifnot(var %in% c("tmin", "tmax", "prec"))

	start <- seq(1950, 2010, 10)
	end <- start + 9
	end[length(end)] <- 2024
	tm <- apply(cbind(start, end), 1, function(x) paste(x, collapse="-"))
	if (!time %in% tm) {
		stop(paste("time should be one of:", paste(tm, collapse=", ")))
	}

	path <- .get_path(path, "climate/hist")
	fname <- paste0("wc2.1_cruts4.09_", res, "_", var, "_", time, ".zip")
	turl <- .wc_url(paste0("hist/cts4.09/", fname))
	outfname <- file.path(path, fname)
	pat <- paste0("wc2.1_cruts4.09_", res, "_", var, "_", substr(time, 1, 3), ".*.tif$")
	ff <- list.files(path=path, pattern=pat, full.names=TRUE)
	if (length(ff) == 0) {
		if (!.downloadDirect(turl, outfname, unzip=TRUE, ...)) return(NULL)
		ff <- list.files(path=path, pattern=pat, full.names=TRUE)
	}
	rast(sort(ff))
}


.did_lonlat <- function(lon, lat) {
	id <- unique(cellFromXY(rast(res=5), cbind(lon,lat)))
	if (any(is.na(id))) stop("invalid coordinates (lon/lat reversed?)")
	path <- system.file(package="geodata")
	tiles <- readRDS(file.path(path, "ex/tiles.rds"))
	if (!(id %in% tiles)) {
		stop("there is no weather data for this location (not on land?)")
	}
	id
}

.did_extent <- function(e) {
	ids <- cells(rast(res=5), e)
	if (length(ids) == 0) {
		stop("area is not on earth?")		
	}
	path <- system.file(package="geodata")
	tiles <- readRDS(file.path(path, "ex/tiles.rds"))
	ids <- ids[ids %in% tiles]
	if (length(ids) == 0) {
		stop("there is no weather data for this location (not on land?)")
	}
	ids
}


.worldclim_day <- function(x, path, sds=FALSE, ...) {

	path <- .get_path(path, "climate")
	if (NCOL(x) == 2) {
		ids <- unique(.did_lonlat(x[,1], x[,2]))
	} else {
		ids <- unique(.did_extent(ext(x)))
	}
	pth <- file.path(path, "day24")
	dir.create(pth, showWarnings=FALSE)
	fname <- paste0("wcd_", ids, ".nc")
	outfname <- file.path(pth, gsub("_", "24_", fname))
	for (i in 1:length(fname)) {
		if (!file.exists(outfname[i])) {
			turl <- .wc_url(paste0("day/2024/", fname[i]))
			if (!.downloadDirect(turl, outfname[i], ...)) return(NULL)
		}
	}
	if (sds && (length(outfname) == 1)) {
		sds(outfname)
	} else {
		outfname
	}
}


.good.file.exists <- function(x, raster=TRUE) {
	if (file.exists(x)) {
		if (isTRUE(file.info(x)$size > 0)) {
			return(TRUE)
		} else {
			file.remove(x)
		}
	}
	FALSE
}


worldclim_tile <- function(var, lon, lat, path, version="2.1", ...) {
	stopifnot(var %in% c("tavg", "tmin", "tmax", "prec", "bio", "bioc", "elev", "wind", "vapr", "srad"))
	version <- as.character(version)
	stopifnot(version %in% c("2.1"))
	if (var == "bioc") var <- "bio"
	path <- .get_path(path, "climate")

	r <- terra::rast(res=30)
	id <- terra::cellFromXY(r, cbind(lon,lat))
	if (is.na(id)) stop("invalid coordinates (lon/lat reversed?)")

	pth <- file.path(path, "tiles_2.1")
	dir.create(pth, showWarnings=FALSE)

	fname <- paste0("tile_", id, "_wc2.1_30s_",	var, ".tif")
	outfname <- file.path(pth, fname)

	if (!.good.file.exists(outfname)) {
		turl <- .wc_url(paste0("tiles/tile/", fname))
		if (is.null(turl)) return(NULL)
	
		if (!.downloadDirect(turl, outfname, ...)) return(NULL)
	}
	rast(outfname)
}


worldclim_country <- function(country, var, path, version="2.1", ...) {

	stopifnot(var %in% c("tavg", "tmin", "tmax", "prec", "bio", "bioc", "elev", "wind", "vapr", "srad"))
	version <- as.character(version)
	stopifnot(version %in% c("2.1"))
	if (var == "bioc") var <- "bio"
	iso <- unique(.getCountryISO(country))

	path <- .get_path(path, "climate")
	pth <- file.path(path, "wc2.1_country")
	dir.create(pth, showWarnings=FALSE)

	fname <- paste0(iso, "_wc2.1_30s_", var, ".tif")
	outfname <- file.path(pth, fname)
	
	for (i in 1:length(fname)) {
		if (!file.exists(outfname[i])) {
			turl <- .wc_url(paste0("tiles/iso/", fname[i]))
			if (is.null(turl)) return(NULL)
			if (!.downloadDirect(turl, outfname[i], ...)) return(NULL)
		}
	}
	if (length(outfname) == 1) {
		rast(outfname)
	} else {
		sprc(outfname)
	}
}


worldclim_global <- function(var, res, path, version="2.1", ...) {

	res <- as.character(res)
	version <- as.character(version)
	stopifnot(res %in% c("2.5", "5", "10", "0.5"))
	stopifnot(var %in% c("tavg", "tmin", "tmax", "prec", "bio", "bioc", "elev", "wind", "vapr", "srad"))
	stopifnot(version %in% c("2.1"))
	if (var == "bioc") var <- "bio"
	path <- .get_path(path, "climate")

	fres <- ifelse(res=="0.5", "30s", paste0(res, "m"))
	path <- file.path(path, paste0("wc2.1_", fres, "/"))
	dir.create(path, showWarnings=FALSE)
	zip <- paste0("wc2.1_", fres, "_", var, ".zip")

	if (var  == "elev") {
		ff <- paste0("wc2.1_", fres, "_elev.tif")
	} else {
		nf <- if (var == "bio") 1:19 else formatC(1:12, width=2, flag=0)
		ff <- paste0("wc2.1_", fres, "_", var, "_", nf, ".tif")
	}
	pzip <- file.path(path, zip)
	ff <- file.path(path, ff)
	if (!all(file.exists(ff))) {
		turl <- .wc_url(paste0("base/", zip))
		if (is.null(turl)) return(NULL)

		if (!.downloadDirect(turl, pzip, ...)) return(NULL)
		fz <- try(utils::unzip(pzip, exdir=path), silent=TRUE)
		try(file.remove(pzip), silent=TRUE)
		if (inherits(fz, "try-error")) {
			message("download failed")
			return(NULL)
		}
	}
	rast(ff)
}


.cmip6_world_old <- function(model, ssp, time, var, res, path, ...) {

	res <- as.character(res)
	stopifnot(res %in% c("2.5", "5", "10"))
	stopifnot(var %in% c("tmin", "tmax", "prec", "bio", "bioc"))
	ssp <- as.character(ssp)
	stopifnot(ssp %in% c("126", "245", "370", "585"))
	stopifnot(model %in% c("BCC-CSM2-MR", "CanESM5", "CNRM-CM6-1", "CNRM-ESM2-1", "GFDL-ESM4", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MRI-ESM2-0"))
	stopifnot(time %in% c("2021-2040", "2041-2060", "2061-2080"))
	
	# some combinations do not exist. Catch these here.
	
	if (var == "bio") var <- "bioc"
	path <- .get_path(path, "climate")

	fres <- ifelse(res==0.5, "30s", paste0(res, "m"))
	path <- file.path(path, paste0("wc2.1_", fres, "/"))
	dir.create(path, showWarnings=FALSE)
	
	outf <- paste0("wc2.1_", fres, "_", var, "_", model, "_ssp", ssp, "_", time, ".tif")
	poutf <- file.path(path, outf)
	if (!file.exists(poutf)) {
		if (!file.exists(outf)) {
			url <- .wc_url(paste0("cmip6/", fres, "/", model, "/ssp", ssp, "/", outf))
			if (is.null(url)) return(NULL)
			if(!.downloadDirect(url, poutf, ...)) return(NULL)
		}
	}
	rast(poutf)
}



.cmods <- c('ACCESS-CM2', 'ACCESS-ESM1-5', 'AWI-CM-1-1-MR', 'BCC-CSM2-MR', 'CanESM5', 'CanESM5-CanOE', 'CMCC-ESM2', 'CNRM-CM6-1', 'CNRM-CM6-1-HR', 'CNRM-ESM2-1', 'EC-Earth3-Veg', 'EC-Earth3-Veg-LR', 'FIO-ESM-2-0', 'GFDL-ESM4', 'GISS-E2-1-G', 'GISS-E2-1-H', 'HadGEM3-GC31-LL', 'INM-CM4-8', 'INM-CM5-0', 'IPSL-CM6A-LR', 'MIROC-ES2L', 'MIROC6', 'MPI-ESM1-2-HR', 'MPI-ESM1-2-LR', 'MRI-ESM2-0', 'UKESM1-0-LL')

.c6url <- "https://geodata.ucdavis.edu/cmip6/"


.check_cmip6 <- function(res, var, ssp, model, time) {
	stopifnot(ssp %in% c("126", "245", "370", "585"))
	stopifnot(res %in% c("0.5", "2.5", "5", "10"))
	stopifnot(var %in% c("tmin", "tmax", "prec", "bioc"))
	if (!(model %in% .cmods)) {
		stop(paste("not a valid model, use of of:\n", paste(.cmods, collapse=", ")))
	}
	stopifnot(time %in% c("2021-2040", "2041-2060", "2061-2080", "2081-2100"))

	# some combinations do not exist. Catch these here.
    #find 30s -type f -printf "%f\n"  | cut -c11-100 > files.txt
	tmpfile <- file.path(tempdir(), "cmip6_files.txt")
	if (!file.exists(tmpfile)) {
		suppressWarnings(try(utils::download.file(paste0(.c6url, "files.txt"), tmpfile,  quiet=TRUE), silent=TRUE))
	}
	if (file.exists(tmpfile)) {
		ff <- utils::read.table(tmpfile, sep="_")
		if (nrow(ff) > 1000) {
			i <- ff[,1] == var & ff[,2] == model & ff[,3] == paste0("ssp", ssp) & ff[,4] == paste0(time, ".tif")
			if (sum(i) != 1) {
				stop("This dataset is not available")
			}
		}
	}
}



cmip6_world <- function(model, ssp, time, var, res, path, ...) {

	res <- as.character(res)
	fres <- ifelse(res==0.5, "30s", paste0(res, "m"))
	ssp <- as.character(ssp)
	if (var == "bio") var <- "bioc"
	try(.check_cmip6(res, var, ssp, model, time), silent=TRUE)
	path <- .get_path(path, "climate")
	path <- file.path(path, paste0("wc2.1_", fres, "/"))
	dir.create(path, showWarnings=FALSE)
	
	outf <- paste0("wc2.1_", fres, "_", var, "_", model, "_ssp", ssp, "_", time, ".tif")
	poutf <- file.path(path, outf)
	if (!file.exists(poutf)) {
		if (!file.exists(outf)) {
		
			durl <- paste0(.c6url, fres, "/", model, "/ssp", ssp, "/", outf)
			durl <- .data_url(durl=durl)
			if (is.null(durl)) return(NULL)
			
			if (!.downloadDirect(durl, poutf, ...)) return(NULL)
		}
		#fz <- try(utils::unzip(pzip, exdir=path, junkpaths=TRUE), silent=TRUE)
		#try(file.remove(pzip), silent=TRUE)
		#if (inherits(fz, "try-error")) { stop("unzip failed") }
	}
	rast(poutf)
}


cmip6_tile <- function(lon, lat, model, ssp, time, var, path, ...) {
	ssp <- as.character(ssp)
	if (var == "bio") var <- "bioc"
	.check_cmip6(0.5, var, ssp, model, time)
	path <- .get_path(path, "climate")
	path <- file.path(path, paste0("wc2.1_30s/"))
	dir.create(path, showWarnings=FALSE)

	r <- rast(res=30)
	ids <- cellFromXY(r, cbind(lon, lat))
	if (any(is.na(ids))) stop("invalid coordinates (lon/lat reversed?)")

	pth <- file.path(path, "wc2.1_tiles")
	dir.create(pth, showWarnings=FALSE)

	fname <- paste0("wc2.1_30s_", var, "_", model, "_ssp", ssp, "_", time, "_tile-", ids, ".tif")
	outfname <- file.path(path, fname)
	for (i in 1:length(outfname)) {
		if (!file.exists(outfname[i])) {
			turl <- paste0(.c6url, "tiles/", model, "/ssp", ssp, "/", fname[i])
			turl <- .data_url(durl=turl)
			if (!.downloadDirect(turl, outfname[i], ...)) return(NULL)
		}
	}
	if (length(outfname) == 1) {
		rast(outfname)
	} else {
		vrt(outfname)
	}
}

