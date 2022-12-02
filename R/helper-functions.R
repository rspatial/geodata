
.data_url <- function() {
	"https://geodata.ucdavis.edu/geodata/"
}

.check_path <- function(path) {
	if (dir.exists(path)) {
		return(TRUE)
	}
	test <- try(dir.create(path, recursive=FALSE), silent=TRUE)
	if (inherits(test, "try-error")) {
		stop("path cannot be created")	
	}
	if (!dir.exists(path)) {
		stop("path does not exist")
	}
}


.get_path <- function(path, check=TRUE) {
	if (missing(path) || (path=="")) {
		path <- default_path()
	}
	if (path == "") stop("path is missing")
	if (check) .check_path(path)
	path
}

default_path <- function(path) {
	if (missing(path)) {
		return( getOption("geodata_detault_path", default = "") )
	}
	path <- .get_path(path, TRUE)
	options(geodata_detault_path=path)
}


.old.download <- function(aurl, filename, quiet=FALSE, mode = "wb", cacheOK = TRUE, ...) {
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

.downloadDirect <- function(url, filename, unzip=FALSE, quiet=FALSE, mode="wb", cacheOK=FALSE, ...) {
	if (!file.exists(filename)) {
		ok <- try(
			suppressWarnings(
				utils::download.file(url=url, destfile=filename, quiet=quiet, mode=mode, cacheOK=cacheOK, ...)
			)
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
		.downloadDirect(url, filepath, ...)
		r <- try(rast(filepath))
		if (inherits(r, "try-error")) {
			try(file.remove(filepath), silent=TRUE)
			message("download failed")
			return(NULL)
		}
	} else {
		r <- rast(filepath)
	}
	r
}



.dataloc <- function() {
	stop("path does not exist")
}

.getDataPath <- function(path) {
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


