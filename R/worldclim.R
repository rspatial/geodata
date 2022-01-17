
.wcurl <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/"

.wccruts <- function(lon, lat, path, ...) {

}


.wcerad <- function(lon, lat, path, ...) {
	.check_path(path)
	r <- rast(res=5)
	id <- cellFromXY(r, cbind(lon,lat))
	if (is.na(id)) stop("invalid coordinates (lon/lat reversed?)")
	pth <- file.path(path, "wcdera")
	fname <- paste0("wcdera_", id, ".nc")
	outfname <- file.path(pth, fname)
	if (!file.exists(outfname)) {
		dir.create(pth, showWarnings=FALSE)
		turl <- paste0(.wcurl, "day/nc/", fname)
		.downloadDirect(turl, outfname, ...)
	}
	#vars <- c("tmin", "tmax", "prec", "srad", "vapr", "wind")
	sds(outfname)
}




worldclim_tile <- function(var, lon, lat, path, ...) {
	stopifnot(var %in% c("tavg", "tmin", "tmax", "prec", "bio", "bioc", "elev"))
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
		.downloadDirect(turl, outfname, ...)
	}
	rast(outfname)
}


worldclim_country <- function(country, var, path, ...) {

	stopifnot(var %in% c("tavg", "tmin", "tmax", "prec", "bio", "bioc", "elev"))
	if (var == "bioc") var <- "bio"
	iso <- .getCountryISO(country)
	
	.check_path(path)
	pth <- file.path(path, "wc2.1_country")
	dir.create(pth, showWarnings=FALSE)

	fname <- paste0(iso, "_wc2.1_30s_", var, ".tif")
	outfname <- file.path(pth, fname)
	
	if (!file.exists(outfname)) {
		turl <- paste0(.wcurl, "tiles/iso/", fname)
		.downloadDirect(turl, outfname, ...)
	}
	rast(outfname)
}


worldclim_global <- function(var, res, path, ...) {

	res <- as.character(res)
	stopifnot(res %in% c("2.5", "5", "10", "0.5"))
	stopifnot(var %in% c("tavg", "tmin", "tmax", "prec", "bio", "bioc", "elev"))
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
		.downloadDirect(paste0(.wcurl, "base/", zip), pzip, ...)
		fz <- try(utils::unzip(pzip, exdir=path), silent=TRUE)
		try(file.remove(pzip), silent=TRUE)
		if (inherits(fz, "try-error")) {stop("download failed")}
	}
	rast(ff)
}


cmip6_world <- function(model, ssp, time, var, res, path, ...) {

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
			.downloadDirect(url, poutf, ...)
		}
		#fz <- try(utils::unzip(pzip, exdir=path, junkpaths=TRUE), silent=TRUE)
		#try(file.remove(pzip), silent=TRUE)
		#if (inherits(fz, "try-error")) { stop("unzip failed") }
	}
	rast(poutf)
}


