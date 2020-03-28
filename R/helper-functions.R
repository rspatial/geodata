

.download <- function(aurl, filename) {
	fn <- paste(tempfile(), ".download", sep="")
	res <- utils::download.file(url=aurl, destfile=fn, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
	if (res == 0) {
		w <- getOption("warn")
		on.exit(options("warn" = w))
		options("warn"=-1) 
		if (! file.rename(fn, filename) ) { 
			# rename failed, perhaps because fn and filename refer to different devices
			file.copy(fn, filename)
			file.remove(fn)
		}
	} else {
		stop("could not download the file" )
	}
}

.dataloc <- function() {
	stop("path does not exist")
}

.getDataPath <- function(path) {
	path <- trim(path)
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


