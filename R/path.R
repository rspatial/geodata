

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


.rapp_path <- function() {
	file.path(rappdirs::user_data_dir(), "geodata")
}

.geodata_cache_size <- function() {
	path <- .rapp_path()
	ff <- list.files(path, recursive=TRUE, full.names=TRUE)
	round(sum(file.size(ff)) / 1024^2, 1)
} 



.clear_geodata_cache <- function(pattern="") {
	path <- .rapp_path()
	ff <- list.files(path, recursive=TRUE, full.names=TRUE)
	if (length(ff) == 0) return()
	if (pattern != "") {
		ff <- grep(pattern, ff, value=TRUE)
	}
	file.remove(ff)
} 

.get_path <- function(path, add) {
	if (missing(path) || is.null(path) || is.na(path) || (path == "")) {
		path <- geodata_path()
		if (path == "") {
			path <- .rapp_path()
			dir.create(path, FALSE, TRUE)
		}
	}
	
	path <- path[1]
	if (!is.character(path)) stop("path is not a character value", call.=FALSE)
#	if (is.null(path)) stop("path cannot be NULL", call.=FALSE)
#	if (is.na(path)) stop("path cannot be NA", call.=FALSE)
#	if (path == "") stop("path is missing", call.=FALSE)
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
			p <- .rapp_path()
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

