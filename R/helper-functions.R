
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


.get_path <- function(path, add="") {
	if (missing(path)) {
		path <- geodata_path()
	}
	path <- path[1]
	if (!is.character(path)) stop("path is not a character value", call.=FALSE)
	if (is.null(path)) stop("path cannot be NULL", call.=FALSE)
	if (is.na(path)) stop("path cannot be NA", call.=FALSE)
	if (path == "") stop("path is missing", call.=FALSE)
	.check_path(path)
	if (add != "") {
		path <- file.path(path, add)
		.check_path(path, TRUE)
	}
	path.expand(path)
}


geodata_path <- function(path) {
	if (missing(path)) {
		return( getOption("geodata_default_path", default = "") )
	}
	path <- .get_path(path, TRUE)
	options(geodata_default_path=path)
}



.downloadDirect <- function(url, filename, unzip=FALSE, quiet=FALSE, mode="wb", cacheOK=FALSE, ...) {
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
		try(file.remove(filename), silent=TRUE)
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




...getDataPath <- function(path) {
	.dataloc <- function() {
		stop("path does not exist")
	}
	path <- trimws(path)
	if (path=="") {
		path <- .dataloc()
	} else {
		if (substr(path, nchar(path)-1, nchar(path)) == "//" ) {
			p <- substr(path, 1, nchar(path)-2)		
		} else if (substr(path, nchar(path), nchar(path)) == "/"  | substr(path, nchar(path), nchar(path)) == "\\") {
			p <- substr(path, 1, nchar(path)-1)
		} else {
			p <- path
		}
		if (!file.exists(p) & !file.exists(path)) {
			stop("path does not exist: ", path)
		}
	}
	if (substr(path, nchar(path), nchar(path)) != "/" & substr(path, nchar(path), nchar(path)) != "\\") {
		path <- paste(path, "/", sep="")
	}
	return(path)
}


...old.download <- function(aurl, filename, quiet=FALSE, mode = "wb", cacheOK = TRUE, ...) {
	fn <- paste(tempfile(), ".download", sep="")
	res <- try(
			suppressWarnings(
				utils::download.file(url=aurl, destfile=fn, quiet=quiet, mode=mode, cacheOK=cacheOK, ...)
			)
		)
	if (inherits(res, "try-error")) {
		message("download failed" )
		return(NULL)
	}
	if (res == 0) {
		if (suppressWarnings(!file.rename(fn, filename)) ) { 
			# rename failed, perhaps because fn and filename refer to different devices
			file.copy(fn, filename)
			file.remove(fn)
		}
	} else {
		message("download failed" )
	}
}
