
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



