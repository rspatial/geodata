# Author: Robert J. Hijmans
# Sept 2019
# version 1
# license GPL3


simple_uri <- function(uri, reverse=FALSE) {
  
	if (reverse) {
		return(gsub("_", "/", sub("_", ":", uri))	)
	}
	
	ur <- .removeprotocol(uri)
	if (grepl("dx.doi.org/", ur)) {
		u <- gsub("dx.doi.org/", "", ur)
		u <- paste0("doi_", u)
	} else if (grepl("doi.org/", ur)) {
		u <- gsub("doi.org/", "", ur)
		u <- paste0("doi_", u)
	} else if (grepl("persistentId=doi:", ur)) {
		u <- unlist(strsplit(ur, "persistentId=doi:"))[2]
		u <- paste0("doi_", u)
	} else if (grepl("^doi:", ur)) {
		u <- gsub("^doi:", "doi_", ur)		
	} else if (grepl("persistentId=hdl:", ur)) {
		u <- unlist(strsplit(ur, "persistentId=hdl:"))[2]
		u <- paste0("hdl_", u)
	} else if (grepl("^hdl:", ur)) {
		u <- gsub("^hdl:", "hdl_", ur)		
	} else if (grepl("hdl.handle.net/", ur)) {
		u <- gsub("hdl.handle.net/", "", ur)
		u <- paste0("hdl_", u)
	} else {
		stop(paste0("Not valid unique object identifier (DOI or HDL)"))
	}
	gsub("/", "_", u)
}



.dataverse_unzip <- function(zipf, path, unzip) {
	allf <- NULL
	for (z in zipf) {
		zf <- utils::unzip(z, list=TRUE)
		zf <- zf$Name[zf$Name != "MANIFEST.TXT"]
		zf <- grep("/$", zf, invert=TRUE, value=TRUE)
		allf <- c(allf, zf)
		if (unzip) {
			ff <- list.files(path, recursive=TRUE, include.dirs=TRUE)
			there <- (zf %in% ff)
			if (!all(there)) {
				utils::unzip(z, zf[!there], exdir = path)
			}	
		}
	}
	zf <- grep("\\.pdf$", allf, value=TRUE, invert=TRUE)
	file.path(path, zf)
}


#.download_dataverse_files(u, baseu, path, uname, domain, protocol, unzip, zipf1)


.download_dataverse_files <- function(u, baseu, path, uname, unzip, zipf) {
	pid <- unlist(strsplit(u, "\\?"))[2]
	uu <- paste0(baseu, "/api/datasets/:persistentId?", pid)
	
	# the nice way
	#r <- httr::GET(uu)
	#httr::stop_for_status(r)
	#js <- httr::content(r, as = "text", encoding = "UTF-8")
	# but for cimmyt...
	tmpf <- tempfile()
	
	if (grepl("worldagroforestry", uu) || grepl("cirad.fr", uu) || grepl("cipotato", uu)) {
		# temporary fix because WorldAgroFor https cert has expired
		# not sure why for CIP on Ubuntu (cert expired)
		utils::download.file(uu, tmpf, quiet=TRUE, method="curl", extra="-k")
	} else {
		utils::download.file(uu, tmpf, quiet=TRUE)
	}
	js <- readLines(tmpf, encoding = "UTF-8", warn=FALSE)
	js <- jsonlite::fromJSON(js)
	fjs <- js$data$latestVersion$files
	jsp <- jsonlite::toJSON(js, pretty=TRUE)
	writeLines(jsp, file.path(path, paste0(uname, ".json")))
	f <- if(is.null(fjs$dataFile)) {fjs$datafile} else {fjs$dataFile}
	f$checksum <- NULL
	f$tabularTags <- NULL
	if (!is.null(f$categories)) {
		fc <- unlist(f$categories)
		if (!is.list(fc) && length(fc) == nrow(f)) {
			f$categories <- fc
		} else {
			f$categories <- NULL		
		}
	}
	fn <- file.path(path, paste0(uname, "_files.txt"))
	#try(utils::write.csv(f, fn))
	try(data.table::fwrite(f, fn))
	rest <- f$restricted
	if (!is.null(rest)) {
		f <- f[!rest, ]
		if (nrow(f) == 0) {
			stop("access to the files is restricted")
		}
		warning("access to some files is restricted")
	}
	if (nrow(f) == 0) {
		stop("no files!")
	}
	
	fsv <- "originalFileSize"
	if (is.null(f[[fsv]])) fsv <- "filesize" 	
	
	if (sum(f[[fsv]], na.rm=TRUE) < 10000000) {
		files <- paste0(f$id, collapse = ",")
		fu <- paste0(baseu, "/api/access/datafiles/", files, "?format=original")
	## temporary fix because WorldAgroFor https cert has expired
		if (grepl("worldagroforestry", fu) || grepl("cirad.fr", fu) || grepl("cipotato", fu)) {
			utils::download.file(fu, zipf, quiet=TRUE, mode="wb", method="curl", extra="-k")
		} else {
			utils::download.file(fu, zipf, mode="wb", quiet=TRUE)
		}
	} else {
		#for (i in 1:nrow(f)) {
		#	print(paste("part", i)); utils::flush.console()
		#	fu <- paste0(protocol, domain, "/api/access/datafiles/", f$id[i], "?format=original")
		#	zipi <- file.path(path, paste0(uname, "_", i, ".zip"))
		#	utils::download.file(fu, zipi, mode="wb", quiet=TRUE)
## temporary fix because WorldAgroFor https cert has expired
##			utils::download.file(fu, zipi, quiet=TRUE, mode="wb", method="curl", extra="-k")
		#	zipf <- c(zipf, zipi)
		#}
	
		f[[fsv]][is.na( f[[fsv]] )] <- 10000
		i <- 1
		zipf <- NULL
		while(TRUE) {
			print(paste("part", i)); utils::flush.console()
			cs <- cumsum( f[[fsv]] )
			k <- which (cs < 9000000)
			if (length(k) == 0) k <- 1
			files <- paste0(f$id[k], collapse = ",")
			fu <- paste0(baseu, "/api/access/datafiles/", files, "?format=original")
			zipi <- file.path(path, paste0(uname, "_", i, ".zip"))
			if (grepl("worldagroforestry", uu)  || grepl("cirad.fr", fu) || grepl("cipotato", fu)) {
## temporary fix because WorldAgroFor https cert has expired
				utils::download.file(fu, zipi, quiet=TRUE, mode="wb", method="curl", extra="-k")
			} else {
				utils::download.file(fu, zipi, mode="wb", quiet=TRUE)
			}
			f <- f[-k,]
			zipf <- c(zipf, zipi)
			if (nrow(f) == 0) break
			i <- i + 1
		}
	}	
	ff <- .dataverse_unzip(zipf, path, unzip)
	f7 <- list.files(path, pattern="\\.7z$", full.names=TRUE)
	if (length(f7) > 0) {
		for (f in f7) {
			fext <- archive::archive_extract(f, path)
			ff <- c(ff, file.path(path, fext))
		}
	}
	writeLines(c(utils::timestamp(quiet=TRUE), uu), file.path(path, "ok.txt"))
	ff
}


.download_ckan_files <- function(u, baseu, path, uname) {
	pid <- unlist(strsplit(u, "dataset/"))[2]
	uu <- paste0(baseu, "/api/3/action/package_show?id=", pid)
	y <- httr::GET(uu)
	if (y$status_code != 200) {
		return(NULL)
	}
	
	ry <- httr::content(y, as="raw")
	meta <- rawToChar(ry)
	writeLines(meta, file.path(path, paste0(uname, ".json")))
	js <- jsonlite::fromJSON(meta)
	d <- js$result$resources
	done <- TRUE
	files <- ""[0]
	for (i in 1:nrow(d)) {
		u <- file.path(baseu, "dataset", d$package_id[i], "resource", d$id[i], "download", d$name[i])
		#if (d$available[i] == "yes") { "active" ?
		outf <- file.path(path, d$name[i])
		ok <- try(utils::download.file(d$url[i], outf, mode="wb", quiet=TRUE), silent=TRUE )
		if (inherits(ok, "try-error")) {
			print("cannot download")
			done <- FALSE
		} else {
			files <- c(files, outf)
		}
	}
	writeLines(c(utils::timestamp(quiet=TRUE), uu), file.path(path, "ok.txt"))
	files
}

.download_dryad_files <- function(u, baseu, path, uname){
	pid <- gsub(":", "%3A", gsub("/", "%2F", unlist(strsplit(u, "dataset/"))[2]))
	uu <- paste0(baseu, "/api/v2/datasets/", pid)
	y <- httr::GET(uu)
	if (y$status_code != 200) {
		return(NULL)
	}
	
	ry <- httr::content(y, as="raw")
	meta <- rawToChar(ry)
	writeLines(meta, file.path(path, paste0(uname, ".json")))
	js	<- jsonlite::fromJSON(meta)
	d <- js$id
	done <- TRUE
	files <- ""[0]
	outf <- file.path(path, paste0(uname, ".zip"))
	ok <- try(utils::download.file(file.path(uu,"download"), outf, mode="wb", quiet=TRUE) )
	if (inherits(ok, "try-error")) {
		print("cannot download ", uname)
		done <- FALSE
	} else {
		files <- c(files, outf)
	}
	utils::unzip(outf, exdir = path)
	writeLines(c(utils::timestamp(quiet=TRUE), uu), file.path(path, "ok.txt"))
	files
}


.getdomain <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]
.getprotocol <- function(x) paste0(strsplit(x, "/")[[c(1, 1)]], "//")
.removeprotocol <- function(x) gsub("http://|https://|www\\.", "", x)


data_from_uri <- function(uri, path, overwrite=FALSE) {
	
	uripath=TRUE
	unzip=TRUE
	
	uname <- simple_uri(uri)
	if (uripath) path <- file.path(path, uname)
	
	if (!file.exists(file.path(path, "ok.txt"))) {
		overwrite <- TRUE
	}

	if (!(overwrite) && (file.exists(file.path(path, "ok.txt")))) {
		ff <- list.files(path, full.names=TRUE, recursive=TRUE)
		ff <- ff[!grepl(".json$", ff)]
		ff <- ff[!grepl(".pdf$", ff)]
		ff <- ff[!grepl(".doc$", ff)]
		ff <- ff[!grepl(".docx$", ff)]
		ff <- ff[basename(ff) != "ok.txt"]
		return(ff)
	}
	zipf0 <- file.path(path, paste0(uname, ".zip"))
	zipf1 <- file.path(path, paste0(uname, "_1.zip"))
	if ((!overwrite) & ((file.exists(zipf0) || file.exists(zipf1)))) {
		zipf <- list.files(path, paste0(uname, ".*zip$"), full.names=TRUE)		
		return(.dataverse_unzip(zipf, path, unzip))
	}
	
	if (grepl("^doi:", uri)) {
		uri <- gsub("^doi:", "https://dx.doi.org/", uri)
	} else if (grepl("^hdl:", uri)) {
		uri <- gsub("^hdl:", "https://hdl.handle.net/", uri)
	}
	dir.create(path, FALSE, TRUE)
	if (!file.exists(path)) {
		stop(paste("cannot create path:", path))
	}
	
	# temporary fix because WorldAgroFor https cert has expired
	httr::set_config(httr::config(ssl_verifypeer = 0L))

	# For CIRAD dataverse
	if (grepl("18167", uri)) {
	  x <- httr::GET(uri, httr::add_headers("user-agent" = "Mozilla/5.0", "Cache-Control" = "no-cache"))
	} else {
	  x <- httr::GET(uri)
	}

	if (x$status_code != 200) {
		message(paste("Dataset or resource not reachable.\nStatus code: ", x$status_code))
		return()
	}
	u <- x$url
	domain <- .getdomain(u)
	protocol <- .getprotocol(u)
	baseu <- paste0(protocol, domain)
	if (grepl("/stash/", u)) {	
		.download_dryad_files(u, baseu, path, uname)
	} else if (grepl("/dataset/", u)) {	
		.download_ckan_files(u, baseu, path, uname)
	} else {
		.download_dataverse_files(u, baseu, path, uname, unzip, zipf1)
	}
}



# uri <- "https://doi.org/10.5061/dryad.pj76g30"
# ff <- data_from_uri(uri, ".")

