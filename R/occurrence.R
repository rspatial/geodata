# Author: Robert J. Hijmans
# Date : December 2009-2011
# Version 1.0
# Licence GPL v3

# 2011-12-04
# implemented trycatch to deal with poor response from GBIF server
# suggestion and changed code provided by John Baumgartner

# 2013-01-15
# translate ISO2 codes to full country names
# add "cloc"

#2014-03-08
# new version, using the json API


.frbind <- function(x, ...) {

	if (! inherits(x, 'data.frame') ) {
		x <- data.frame(x)
	}

	d <- list(...)
	if (length(d) == 0) { return(x) }
	
	for (i in 1:length(d)) {
		
		dd <- d[[i]]
		if (! inherits(dd, 'data.frame')) {
			dd <- data.frame(dd)
		}
		
		cnx <- colnames(x)
		cnd <- colnames(dd)
		
		e <- cnx[(cnx %in% cnd)]	
		for (j in e) {
			if (class(x[,j]) != class(dd[,j])) {
				x[,j] <- as.character(x[,j])
				dd[,j] <- as.character(dd[,j])
			}
		}
		
		a <- which(!cnd %in% cnx)
		if (length(a) > 0) {
			zz <- dd[NULL, a, drop=FALSE]
			zz[1:nrow(x),] <- NA
			x <- cbind(x, zz)
		}

		b <- which(!cnx %in% cnd)
		if (length(b) > 0) {
			zz <- x[NULL, b, drop=FALSE]
			zz[1:nrow(dd),] <- NA
			dd <- cbind(dd, zz)
		}
		
		x <- rbind(x, dd)		
	}
	x
}


.ccodes <- function() {
	path <- system.file(package="geodata")
	readRDS(file.path(path, "ex/countries.rds"))
}



.getExtGBIF <- function(ext) {
	if (!is.null(ext)) { 
		e <- round(ext(ext), 5)
		global <- ext(-180,180,-90,90)
		ex <- intersect(e, global)
		if (!is.null(ex)) {
			ex <- paste0('&decimalLatitude=', ymin(e),',', ymax(e), '&decimalLongitude=', xmin(e), ',', xmax(e))
		} else {
			warning('invalid extent')
		}
	} else {
		ex <- NULL
	}
	return(ex)
} 

.fixNameGBIF <- function(genus, species) {
	genus <- trimws(genus)
	species <- trimws(species)
	gensp <- paste(genus, species)
	spec <- gsub("   ", " ", species) 
	spec <- gsub("  ", " ", spec) 	
	spec <- gsub(" ", "%20", spec)  # for genus species var. xxx
	spec <- paste(genus, '+', spec, sep='')
	return(spec)
}


sp_occurrence <- function(genus, species="", ext=NULL, args=NULL, geo=TRUE, removeZeros=FALSE, download=TRUE, ntries=5, nrecs=300, start=1, end=Inf, ...) {
	
	
	if (! requireNamespace("jsonlite")) { stop("You need to install the jsonlite package to use this function") }

	tmpfile <- paste0(tempfile(), ".json")
	ex <- .getExtGBIF(ext)
	spec <- .fixNameGBIF(genus, species)
	if (geo) { cds <- "&coordinatestatus=true" } else { cds <- "" }

	base <- "https://api.gbif.org/v1/occurrence/search?"
	
	if (!is.null(args)) {
		args <- trimws(as.character(args))
		args <- paste("&", paste(args, collapse="&"), sep="")
	}
	
	ntries <- min(max(ntries, 1), 100)

	url1 <- paste(base, "scientificname=", spec, "&limit=1", cds, ex, args, sep="")
	test <- .downloadDirect(url1, tmpfile, ...)
	json <- scan(tmpfile, what="character", quiet=TRUE, sep="\n",  encoding = "UTF-8")
	x <- jsonlite::fromJSON(json)
	if (!download) {
		if (is.null(x$count)) {
			return(0)
		} else {
			return(x$count)
		}
	} else {
		end <- ifelse(is.null(x$count), 0, x$count)
		message(end, " records found")
		if (end == 0) {
			return(NULL)
		}
		if (end > 200000) {
			stop("The number of records is larger than the maximum for download via this service (200,000)")
		}		
	}

	start <- max(1, start)
	stopifnot(start <= end)
	nrecs <- min(max(nrecs, 1), 300)
	url1 <- paste(base, "scientificname=", spec, "&limit=", format(nrecs, scientific=FALSE), cds, ex, args, sep="")
	
	g <- list()
	breakout <- FALSE
	np <- i <- 1
	while (TRUE) {
		if (start+nrecs >= end) {
			nrecs <- end - start + 1
			url1 <- paste(base, "scientificname=", spec, "&limit=", format(nrecs, scientific=FALSE), cds, ex, args, sep="")
			breakout <- TRUE
		}	
	
		aurl <- paste(url1, "&offset=", format(start-1, scientific=FALSE), sep="")
		
		if (np > 20) {
			np <- 1
			message("")
		}
		message(paste(start-1, "-", sep=""), appendLF = FALSE) 
		utils::flush.console()
		tries <- 0
		np <- np + 1
		
        #======= if download fails due to server problems, keep trying  =======#
        while (TRUE) {
			tries <- tries + 1
			if (tries > ntries) {
				warning("GBIF did not return the data in ", ntries, "  tries for:")
				print(aurl)
				breakout <- TRUE
				break
			}
			test <- .downloadDirect(aurl, tmpfile, quiet = TRUE)
			if (class(test) == "try-error") {
				print("download failure, trying again...")
			} else {
				json <- scan(tmpfile, what="character", quiet=TRUE, sep="\n",  encoding = "UTF-8")
				json <- chartr("\a\v", "  ", json)
				x <- jsonlite::fromJSON(json)
				if (is.null(x$count)) {
					x$count <- 0
					if (i == 1) {
						warning("no records found")
						break
					} else {
						break
					}
				}
				r <- x$results
				r <- r[, ! sapply(r, class) %in% c("data.frame", "list")]
				rownames(r) <- NULL
				g[[i]] <- r
				break
			}
	    }
		start <- start + nrecs
		i <- i + 1
		if (breakout) break
		if (x$endOfRecords) break
	}
	message(end) 

	message(min(end, x$count), " records downloaded")

	if (length(g) == 0) {
		return(NULL)
	} else if (length(g) == 1) {
		z <- g[[1]]
	} else {
		z <- do.call(.frbind, g)
	}
	cn <- colnames(z)
	cn <- gsub("decimalLatitude", "lat", cn)
	cn <- gsub("decimalLongitude", "lon", cn)
	cn <- gsub("stateProvince", "adm1", cn)
	cn <- gsub("county", "adm2", cn)
	cn <- gsub("countryCode", "ISO2", cn)
	cn <- gsub("country", "fullCountry", cn)
	colnames(z) <- cn

	if (("lat" %in% cn) & ("lon" %in% cn)) {
		z[,"lon"] <- gsub(",", ".", z[,"lon"])
		z[,"lat"] <- gsub(",", ".", z[,"lat"])
		z[,"lon"] <- as.numeric(z[,"lon"])
		z[,"lat"] <- as.numeric(z[,"lat"])
		k <- apply(z[ ,c("lon", "lat")], 1, function(x) isTRUE(any(x==0)))
		
		if (removeZeros) {
			if (geo) {
				z <- z[!k, ]
			} else {
				z[k, c("lat", "lon")] <- NA 
			}
		} else {
			z[k, c("lat", "lon")] <- NA 
		}
	} 
	
	if (nrow(z) > 0) {
	
		if ("ISO2" %in% cn) {
			iso <- .ccodes()
			i <- match(z$ISO2, iso[, "ISO2"])
			z$country <- iso[i, 1]
			z$country[is.na(z$ISO2)] <- NA
		}
		
		vrs <- c("locality", "adm1", "adm2", "country", "continent") 
		vrs <- vrs[vrs %in% colnames(z)]
		if (length(vrs) > 0) {
			fullloc <- trimws(as.matrix(z[, vrs]))
			fullloc <- apply(fullloc, 1, function(x) paste(x, collapse=", "))
			fullloc <- gsub("NA, ", "", fullloc)
			fullloc <- gsub(", NA", "", fullloc)
			fullloc <- gsub("\"", "", fullloc)
			z$cloc <- fullloc
		} else {
			z$cloc <- NA
		}
	}	

	z <- z[, sort(colnames(z))]
	d <- as.Date(Sys.time())
	z <- cbind(z, downloadDate=d)
	
	#	if (inherits(ext, "SpatialPolygons")) { overlay	}
	try(file.remove(tmpfile), silent=TRUE)
	
	return(z)
}

#sa <- gbif("solanum")
#sa <- gbif("solanum", "*")
#sa <- gbif("solanum", "acaule*")
#sa <- gbif("solanum", "acaule var acaule")


