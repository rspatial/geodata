
.wcurl <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/"

.wccruts <- function(lon, lat, path, ...) {
}


.get_id <- function(lon, lat) {
	r <- rast(res=5)	
	id <- cellFromXY(r, cbind(lon,lat))[1]
	if (is.na(id)) stop("invalid coordinates (lon/lat reversed?)")
	path <- system.file(package="geodata")
	tiles <- readRDS(file.path(path, "ex/tiles.rds"))
	if (!(id %in% tiles)) {
		stop("there is no weather data for this location (not on land?)")
	}
	id
}

.wcerad <- function(lon, lat, path, ...) {
	.check_path(path)
	id <- .get_id(lon, lat)	
	pth <- file.path(path, "wcdera")
	fname <- paste0("wcdera_", id, ".nc")
	outfname <- file.path(pth, fname)
	if (!file.exists(outfname)) {
		dir.create(pth, showWarnings=FALSE)
		turl <- paste0(.wcurl, "day/nc/", fname)
		if (!.downloadDirect(turl, outfname, ...)) return(NULL)
	}
	sds(outfname)
}


.wcerad21 <- function(lon, lat, path, ...) {
	.check_path(path)
	id <- .get_id(lon, lat)

	pth <- file.path(path, "wcdera")
	fname <- paste0("wcdera_", id, ".nc")
	outfname <- file.path(pth, gsub("_", "21_", fname))
	if (!file.exists(outfname)) {
		dir.create(pth, showWarnings=FALSE)
		turl <- paste0(.wcurl, "day/nc21/", fname)
		if (!.downloadDirect(turl, outfname, ...)) return(NULL)
	}
	sds(outfname)
}



worldclim_tile <- function(var, lon, lat, path, version="2.1", ...) {
	stopifnot(var %in% c("tavg", "tmin", "tmax", "prec", "bio", "bioc", "elev", "wind", "vapr", "srad"))
	version <- as.character(version)
	stopifnot(version %in% c("2.1"))
	if (var == "bioc") var <- "bio"
	.check_path(path)

	r <- rast(res=30)
	id <- cellFromXY(r, cbind(lon,lat))
	if (is.na(id)) stop("invalid coordinates (lon/lat reversed?)")

	pth <- file.path(path, "wc2.1_tiles")
	dir.create(pth, showWarnings=FALSE)

	fname <- paste0("tile_", id, "_wc2.1_30s_", var, ".tif")
	outfname <- file.path(pth, fname)

	if (!file.exists(outfname)) {
		turl <- paste0(.wcurl, "tiles/tile/", fname)
		if (!.downloadDirect(turl, outfname, ...)) return(NULL)
	}
	rast(outfname)
}


worldclim_country <- function(country, var, path, version="2.1", ...) {

	stopifnot(var %in% c("tavg", "tmin", "tmax", "prec", "bio", "bioc", "elev", "wind", "vapr", "srad"))
	version <- as.character(version)
	stopifnot(version %in% c("2.1"))
	if (var == "bioc") var <- "bio"
	iso <- .getCountryISO(country)
	
	.check_path(path)
	pth <- file.path(path, "wc2.1_country")
	dir.create(pth, showWarnings=FALSE)

	fname <- paste0(iso, "_wc2.1_30s_", var, ".tif")
	outfname <- file.path(pth, fname)
	
	if (!file.exists(outfname)) {
		turl <- paste0(.wcurl, "tiles/iso/", fname)
		if (!.downloadDirect(turl, outfname, ...)) return(NULL)
	}
	rast(outfname)
}


worldclim_global <- function(var, res, path, version="2.1", ...) {

	res <- as.character(res)
	version <- as.character(version)
	stopifnot(res %in% c("2.5", "5", "10", "0.5"))
	stopifnot(var %in% c("tavg", "tmin", "tmax", "prec", "bio", "bioc", "elev", "wind", "vapr", "srad"))
	stopifnot(version %in% c("2.1"))
	if (var == "bioc") var <- "bio"
	.check_path(path)

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
		if (!.downloadDirect(paste0(.wcurl, "base/", zip), pzip, ...)) return(NULL)
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
	.check_path(path)

	fres <- ifelse(res==0.5, "30s", paste0(res, "m"))
	path <- file.path(path, paste0("wc2.1_", fres, "/"))
	dir.create(path, showWarnings=FALSE)
	
	outf <- paste0("wc2.1_", fres, "_", var, "_", model, "_ssp", ssp, "_", time, ".tif")
	poutf <- file.path(path, outf)
	if (!file.exists(poutf)) {
		if (!file.exists(outf)) {
			url <- paste0(.wcurl, "cmip6/", fres, "/", model, "/ssp", ssp, "/", outf)
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
	.check_path(path)
	path <- file.path(path, paste0("wc2.1_", fres, "/"))
	dir.create(path, showWarnings=FALSE)
	
	outf <- paste0("wc2.1_", fres, "_", var, "_", model, "_ssp", ssp, "_", time, ".tif")
	poutf <- file.path(path, outf)
	if (!file.exists(poutf)) {
		if (!file.exists(outf)) {
			url <- paste0(.c6url, fres, "/", model, "/ssp", ssp, "/", outf)
			if (!.downloadDirect(url, poutf, ...)) return(NULL)
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
	.check_path(path)
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
			if (!.downloadDirect(turl, outfname[i], ...)) return(NULL)
		}
	}
	if (length(outfname) == 1) {
		rast(outfname)
	} else {
		vrt(outfname)
	}
}

