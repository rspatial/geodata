
.wcurl <- "http://biogeo.ucdavis.edu/data/worldclim/v2.1/"


wordclim_tile <- function(var, lon, lat, path) {
	stopifnot(var %in% c("tavg", "tmin", "tmax", "prec", "bio", "bioc", "elev"))
	if (var == "bioc") var <- "bio"
	stopifnot(dir.exists(path))

	r <- rast(res=30)
	id <- cellFromXY(r, cbind(lon,lat))
	if (is.na(id)) stop("invalid coordinates (lon/lat reversed?)")

	path <- file.path(path, "wc2.1_tiles")
	dir.create(path, showWarnings=FALSE)

	fname <- paste0("tile_", id, "_wc2.1_30s_", var, ".tif")
	outfname <- file.path(path, fname)
	if (!file.exists(outfname)) {
		turl <- paste0(.wcurl, "tiles/tile/", fname)
		download.file(turl, outfname, mode="wb")
	}
	rast(outfname)
}


wordclim_country <- function(country, var) {
	stopifnot(var %in% c("tavg", "tmin", "tmax", "prec", "bio", "bioc", "elev"))
	if (var == "bioc") var <- "bio"
	iso <- getCountryISO(country)
	stopifnot(dir.exists(path))

	path <- file.path(path, "wc2.1_country")
	dir.create(path, showWarnings=FALSE)

	fname <- paste0(iso, "_wc2.1_30s_", var, ".tif")
	outfname <- file.path(path, fname)
	if (!file.exists(outfname)) {
		turl <- paste0(.wcurl, "tiles/iso/", fname)
		download.file(turl, outfname, mode="wb")
	}
	rast(outfname)
}


worldclim_global <- function(var, res, path, ...) {

	res <- as.character(res)
	stopifnot(res %in% c("2.5", "5", "10", "0.5"))
	stopifnot(var %in% c("tavg", "tmin", "tmax", "prec", "bio", "bioc", "elev"))
	if (var == "bioc") var <- "bio"
	stopifnot(dir.exists(path))

	fres <- ifelse(res=="0.5", "30s", paste0(res, "m"))
	path <- file.path(path, paste0("wc2.1_", fres, "/"))
	dir.create(path, showWarnings=FALSE)
	zip <- paste0("wc2.1_", fres, "_", var, ".zip")

	if (var  == "alt") {
		ff <- paste0("wc2.1_", fres, "_elev.tif")
	} else {
		nf <- if (var == "bio") 1:19 else formatC(1:12, width=2, flag=0)
		ff <- paste0("wc2.1_", fres, "_", var, "_", nf, ".tif")
	}
	pzip <- file.path(path, zip)
	if (!all(file.exists(ff))) {
		download.file(paste0(.wcurl, "base/", zip), pzip, mode="wb")
		if (!file.exists(pzip)) {stop("download failed")}
		fz <- try(unzip(pzip, exdir=path))
		if (class(fz) == "try-error") {stop("download failed")}
	}
	rast(file.path(path, ff))
}


cmip6_global <- function(model, ssp, time, var, res, path) {

	res <- as.character(res)
	stopifnot(res %in% c("2.5", "5", "10"))
	stopifnot(var %in% c("tmin", "tmax", "prec", "bio", "bioc"))
	ssp <- as.character(ssp)
	stopifnot(ssp %in% c("126", "245", "370", "585"))
	stopifnot(model %in% c("BCC-CSM2-MR", "CanESM5", "CNRM-CM6-1", "CNRM-ESM2-1", "GFDL-ESM4", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MRI-ESM2-0"))
	
	# some combinations do not exist. Catch these here.
	
	if (var == "bio") var <- "bioc"
	stopifnot(dir.exists(path))

	fres <- ifelse(res==0.5, "30s", paste0(res, "m"))
	path <- file.path(path, paste0("wc2.1_", fres, "/"))
	dir.create(path, showWarnings=FALSE)
	
	zip <- paste0("wc2.1_", fres, "_", model, "_ssp", ssp, "_", time, ".zip")
	pzip <- file.path(path, zip)
	outf <- gsub("\\.zip$", ".tif", zip)
	poutf <- file.path(path, outf)
	if (!file.exists(pzip)) {
		download.file(paste0(.wcurl, "fut/", res, "/", zip), pzip, mode="wb")
		if (!file.exists(pzip)) {stop("download failed")}
		fz <- try(unzip(pzip, exdir=path))
		if (class(fz) == "try-error") {stop("download failed")}
	}
	rast(file.path(path, poutf))
}


