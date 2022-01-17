

.check_path <- function(path) {
	if (!dir.exists(path)) {
		dir.create(path, recursive=FALSE)
	}
}


.download <- function(aurl, filename, quiet=FALSE, mode = "wb", cacheOK = TRUE, ...) {
	fn <- paste(tempfile(), ".download", sep="")
	res <- utils::download.file(url=aurl, destfile=fn, quiet=quiet, mode=mode, cacheOK=cacheOK, ...)
	if (res == 0) {
		if (suppressWarnings(!file.rename(fn, filename)) ) { 
			# rename failed, perhaps because fn and filename refer to different devices
			file.copy(fn, filename)
			file.remove(fn)
		}
	} else {
		stop("could not download the file" )
	}
}

.downloadDirect <- function(url, filename, unzip=FALSE, quiet=FALSE, mode="wb", cacheOK=TRUE, ...) {
	if (!file.exists(filename)) {
		ok <- try(
				utils::download.file(url=url, destfile=filename, quiet=quiet, mode=mode, cacheOK=cacheOK, ...)
			)
		if (inherits(ok, "try-error")) {
			if (file.exists(filename)) file.remove(filename)
			stop("download failed")	
		}
		if (!file.exists(filename)) {
			stop("download failed")
		}
	}
	if (unzip) {
		zok <- try(utils::unzip(filename, exdir=dirname(filename)), silent=TRUE)
		try(file.remove(filename), silent=TRUE)
		if (inherits(zok, "try-error")) {
			stop("download failed")
		}
	}
	TRUE	
}	


.donwload_url <- function(url, filepath, ...) {
	if (!(file.exists(filepath))) {
		.downloadDirect(url, filepath, ...)
		r <- try(rast(filepath))
		if (class(r) == "try-error") {
			try(file.remove(filepath), silent=TRUE)
			stop("download failed")
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


