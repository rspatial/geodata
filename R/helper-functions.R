
.data_url <- function(add="", durl="https://geodata.ucdavis.edu/geodata/") {
	con <- url(durl)
	check <- suppressWarnings(try(open.connection(con, open="rt", timeout=5), silent=TRUE)[1])
	suppressWarnings(try(close.connection(con), silent=TRUE))
	if (!is.null(check)) {
		suppressWarnings(
			x <- try(readLines("https://www.worldclim.org/noservice.txt", warn=FALSE), silent=TRUE)
		)
		if (!inherits(x, "try-error")) {
			message(paste(x, collapse="\n"))
		} else {
			message("The geodata server seems to be off-line")
		}
		return(NULL)
	}
	paste0(durl, add)
}


.wc_url <- function(add="") {
	.data_url(add, "https://geodata.ucdavis.edu/climate/worldclim/2_1/")
}


.check_path <- function(path, recursive=FALSE) {
	if (dir.exists(path)) {
		return(TRUE)
	}
	test <- try(dir.create(path, showWarnings=FALSE, recursive=recursive), silent=TRUE)
	if (inherits(test, "try-error")) {
		stop("path cannot be created", call.=FALSE)	
	}
	if (!dir.exists(path)) {
		stop("path does not exist", call.=FALSE)
	}
}


geodata_cache_size <- function() {
	path <- file.path(rappdirs::user_data_dir(), ".geodata")
	ff <- list.files(path, recursive=TRUE, full.names=TRUE)
	round(sum(file.size(ff)) / 1024^2, 1)
} 


clear_geodata_cache <- function(pattern="") {
	path <- file.path(rappdirs::user_data_dir(), ".geodata")
	ff <- list.files(path, recursive=TRUE, full.names=TRUE)
	if (pattern != "") {
		ff <- grep(pattern, ff, value=TRUE)
	}
	file.remove(ff)
} 

.get_path <- function(path, add) {
	if (missing(path)) {
		path <- geodata_path()
		if (path == "") {
			path <- file.path(rappdirs::user_data_dir(), ".geodata")
			dir.create(path, FALSE, TRUE)
		}
	}
	
	path <- path[1]
	if (!is.character(path)) stop("path is not a character value", call.=FALSE)
	if (is.null(path)) stop("path cannot be NULL", call.=FALSE)
	if (is.na(path)) stop("path cannot be NA", call.=FALSE)
	if (path == "") stop("path is missing", call.=FALSE)
	.check_path(path)
	path <- file.path(path, add)
	.check_path(path, TRUE)
	path.expand(path)
}


geodata_path <- function(path) {
	if (missing(path)) {
		p <- getOption("geodata_default_path", default = "")
		if (p == "") p <- Sys.getenv("GEODATA_PATH")
		if (p == "") {
			p <- file.path(rappdirs::user_data_dir(), ".geodata")
			dir.create(p, FALSE, TRUE)
		}
		return(p)
	}
	if (is.na(path)) {
		return(options(geodata_default_path=""))
	}
	path <- .get_path(path, "")
	options(geodata_default_path=path)
}


.downloadDirect <- function(url, filename, unzip=FALSE, quiet=FALSE, mode="wb", cacheOK=FALSE, remove=TRUE,  ...) {
	if (!file.exists(filename)) {
		ok <- try(
			suppressWarnings(
				utils::download.file(url=url, destfile=filename, quiet=quiet, mode=mode, cacheOK=cacheOK, ...)), silent=TRUE
		)
		if (inherits(ok, "try-error")) {
			if (file.exists(filename)) file.remove(filename)
			message("download failed")
			return(FALSE)
		}
		if (!file.exists(filename)) {
			message("download failed")
			return(FALSE)
		}
	}
	if (unzip) {
		zok <- try(utils::unzip(filename, exdir=dirname(filename)), silent=TRUE)
		if (remove) try(file.remove(filename), silent=TRUE)
		if (inherits(zok, "try-error")) {
			message("download failed")
			return(FALSE)
		}
	}
	TRUE	
}	


.donwload_url <- function(url, filepath, ...) {
	if (!(file.exists(filepath))) {
		if (.downloadDirect(url, filepath, ...)) {
			r <- try(rast(filepath), silent=TRUE)
			if (inherits(r, "try-error")) {
				try(file.remove(filepath), silent=TRUE)
				message("download failed")
				return(NULL)
			}
		} else {
			return(NULL)
		}
	} else {
		r <- rast(filepath)
	}
	r
}



