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
			if (!all(class(x[,j]) == class(dd[,j]))) {
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

sp_genus <- function(genus, simple=TRUE, ...) {
	gurl <- paste0("https://api.gbif.org/v1/species/match?name=", genus)
	tmpfile <- tempfile()
	if (!.downloadDirect(gurl, tmpfile, quiet=TRUE, ...)) return(NULL)
	json <- scan(tmpfile, what="character", quiet=TRUE, sep="\n",  encoding = "UTF-8")
	try(file.remove(tmpfile), silent=TRUE)
	json <- chartr("\a\v", "  ", json)
	x <- jsonlite::fromJSON(json)
	x <- as.data.frame(x)
	key <- x[(tolower(x$rank)) == "genus", "genusKey"]

	res <- list()
	off <- 0
	i <- 1
	while (TRUE) {
		surl <- paste0("https://api.gbif.org/v1/species/search?rank=SPECIES&highertaxon_key=", key, "&offset=", off, "&limit=1000")
		tmpfile <- tempfile()
		if (!.downloadDirect(surl, tmpfile, quiet=TRUE, ...)) return(NULL)
		json <- scan(tmpfile, what="character", quiet=TRUE, sep="\n",  encoding = "UTF-8")
		try(file.remove(tmpfile), silent=TRUE)
		x <- jsonlite::fromJSON(json)
		res[[i]] <- as.data.frame(x$results)
		if ( x$endOfRecords ) break;
		i <- i + 1;
		off <- off + 1000
	}
	res <- do.call(rbind, res)
	if (simple) {
		return(sort(unique(res$species)))
	}
	res$higherClassificationMap <- NULL
	res$nomenclaturalStatus <- NULL
	res$descriptions <- NULL
	res$numOccurrences <- NULL
#	res$descriptions <- sapply(res$descriptions, function(i) paste(unlist(i), collapse="; "))
	res$habitats <- sapply(res$habitats, function(i) paste(unlist(i), collapse="; "))
	res$threatStatuses <- sapply(res$threatStatuses, function(i) paste(unlist(i), collapse="; "))
	res$vernacularNames <- sapply(res$vernacularNames, function(i) {
			if (ncol(i) > 0) {
				paste(sort(unique(i[,1])), collapse="; ")
			} else {
				""
			}
		}
	)
	res
}


sp_occurrence <- function(genus, species="", ext=NULL, args=NULL, geo=TRUE, removeZeros=FALSE, download=TRUE, ntries=5, nrecs=300, start=1, end=Inf, fixnames=TRUE, sv=FALSE, ...) {
	
	
	if (! requireNamespace("jsonlite")) { stop("You need to install the jsonlite package to use this function") }

	tmpfile <- paste0(tempfile(), ".json")
	ex <- .getExtGBIF(ext)
	spec <- .fixNameGBIF(genus, species)
	if (geo) { cds <- "&hasCoordinate=true" } else { cds <- "" }

	base <- "https://api.gbif.org/v1/occurrence/search?"
	
	if (!is.null(args)) {
		args <- trimws(as.character(args))
		args <- gsub(" ", "%20", args)
		args <- paste0("&", paste(args, collapse="&"))
	}
	
	ntries <- min(max(ntries, 1), 100)

	url1 <- paste(base, "scientificname=", spec, "&limit=1", cds, ex, args, sep="")
	if (!.downloadDirect(url1, tmpfile, quiet=TRUE, ...)) return(NULL)
	json <- scan(tmpfile, what="character", quiet=TRUE, sep="\n",  encoding = "UTF-8")
	try(file.remove(tmpfile), silent=TRUE)
	x <- jsonlite::fromJSON(json)
	if (!download) {
		if (is.null(x$count)) {
			return(0)
		} else {
			return(x$count)
		}
	}
	
	ntot <- ifelse(is.null(x$count), 0, x$count)
	if (ntot == 0) {
		message("no records found")
		return(NULL)
	}
	start <- max(1, start)
	end <- min(end, ntot)
	stopifnot(start <= end)

	if (end > 100000) {
		stop("GBIF does not allow using this service for record numbers that are > 100,000")
	}

	ntot <- (end-start)+1
	message(ntot, " records found")
	if (ntot <= 0) {
		return(NULL)
	}

	if (ntot > 100000) {
		stop("The number of records is larger than the maximum for download via this service (100,000)")
	}

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
		message(paste(format(start-1, scientific=FALSE), "-", sep=""), appendLF = FALSE) 
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
			if (!.downloadDirect(aurl, tmpfile, quiet=TRUE, ...)) return(NULL)

			json <- scan(tmpfile, what="character", quiet=TRUE, sep="\n",  encoding = "UTF-8")
			try(file.remove(tmpfile), silent=TRUE)
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
			ok <- !(sapply(r, function(i) class(i)[1]) %in% c("data.frame", "list"))
			r <- r[, ok]
			rownames(r) <- NULL
			g[[i]] <- r
			break
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
	lat <- "decimalLatitude"
	lon <- "decimalLongitude"
	cn <- colnames(z)	
	if ((lat %in% cn) & (lon %in% cn)) {
		z[,lon] <- gsub(",", ".", z[,lon])
		z[,lat] <- gsub(",", ".", z[,lat])
		z[,lon] <- as.numeric(z[,lon])
		z[,lat] <- as.numeric(z[,lat])
		k <- apply(z[ ,c(lon, lat)], 1, function(x) isTRUE(any(x==0)))
		
		if (removeZeros) {
			if (geo) {
				z <- z[!k, ]
			} else {
				z[k, c(lat, lon)] <- NA 
			}
		} else {
			z[k, c(lat, lon)] <- NA 
		}
	} 
	
	if (fixnames && (nrow(z) > 0)) {
		cn <- gsub("decimalLatitude", "lat", cn)
		cn <- gsub("decimalLongitude", "lon", cn)
		cn <- gsub("stateProvince", "adm1", cn)
		cn <- gsub("county", "adm2", cn)
		cn <- gsub("countryCode", "ISO2", cn)
		colnames(z) <- cn
		if ("ISO2" %in% cn) {
			z$fullCountry <- z$country
			iso <- .ccodes()
			i <- match(z$ISO2, iso[, "ISO2"])
			z$country <- iso[i, 1]
			z$country[is.na(z$ISO2)] <- NA
		}
		
		vrs <- c("locality", "stateProvince", "county", "country", "continent") 
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

        if (sv) {
          xcoor <- ifelse(fixnames, "lon", "decimalLongitude")
          ycoor <- ifelse(fixnames, "lat", "decimalLatitude")
          z <- vect(z, geom=c(xcoor, ycoor), keepgeom=TRUE, crs="epsg:4326")
        }

	return(z)
}

#p <- sp_occurrence("solanum", "cantense")
#pp <- sp_occurrence("solanum", "cantense", fix=F)

#sa <- gbif("solanum", "*")
#sa <- gbif("solanum", "acaule*")
#sa <- gbif("solanum", "acaule var acaule")


.run_sp_occurrence_batch <- function(genus, species="", path=".", ext=c(-180,180,-90,90), args=NULL, geo=TRUE, removeZeros=FALSE, ntries=5, nrecs=300, fixnames=TRUE, prefix=NULL, ...) {

	dir.create("gbif", FALSE, FALSE)
	r <- rast(nrow=2, ncol=2, extent=ext)
	p <- as.polygons(r)
	for (i in 1:nrow(p)) {
		itr <- paste0(prefix, i)
		fit <- file.path(path, "gbif", paste0(itr, ".rds"))
		if (!file.exists(fit)) {
			ep <- ext(p[i])
			obs <- geodata::sp_occurrence(genus, species, download=FALSE, ext=ep)
			if (obs >= 50000) {
				message(paste0(itr, ": split (", obs, ")")); utils::flush.console()
				d <- .run_sp_occurrence_batch(genus, species, ext=ep, args=args, geo=geo, removeZeros=removeZeros, ntries=ntries, nrecs=nrecs, fixnames=fixnames, prefix=itr)
			} else if (obs > 0) {
				message(paste0(itr, ":")); utils::flush.console()
				d <- NULL
				d <- geodata::sp_occurrence(genus, species, download=TRUE, ext=ep, args=args, 
					geo=geo, removeZeros=removeZeros, ntries=ntries, nrecs=nrecs, 
					fixnames=fixnames, ...)
				if (!is.null(d)) {
					saveRDS(d, fit)
				}
			} else {
				message(paste0(itr, ": no records")); utils::flush.console()
			}
		} else {
			message(paste0(itr, ": exists")); utils::flush.console()
		}
	}
}



sp_occurrence_split <- function(genus, species="", path=".", ext=c(-180,180,-90,90), args=NULL, geo=TRUE, removeZeros=FALSE, ntries=5, nrecs=300, fixnames=TRUE, prefix=NULL, sv=FALSE, ...) {

	if (is.null(prefix)) {
		prefix <- tolower(paste0(genus, "_", species, "_"))
		fout <- file.path(path, "gbif", paste0(genus, "_", species, ".rds"))
	} else {
		fout <- file.path(path, "gbif", paste0(prefix, ".rds"))
	}
	if (file.exists(fout)) {
		message("using existing file")
		out <- readRDS(fout)
		return(out)
	}

	.run_sp_occurrence_batch(genus, species, path=path, ext=ext, args=args, geo=geo, removeZeros=removeZeros, ntries=ntries, nrecs=nrecs, fixnames=fixnames, prefix=prefix, ...)

	message("combining")
	ff  <- list.files("gbif", pattern=paste0("^", genus, "_", species), full.names=TRUE)
	out <- lapply(ff, readRDS)
	i <- sapply(out, is.null)
	if (any(i)) out <- out[[!i]]
	out <- do.call(.frbind, out)
	out <- unique(out)
	saveRDS(out, fout)

	if (sv) {
          xcoor <- ifelse(fixnames, "lon", "decimalLongitude")
          ycoor <- ifelse(fixnames, "lat", "decimalLatitude")
          out <- vect(out, geom=c(xcoor, ycoor), keepgeom=TRUE, crs="epsg:4326")
        }

	return(out)
}


#x = sp_occurrence_batch("Anas", "acuta")
