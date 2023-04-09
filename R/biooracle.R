
.create_table <- function() {
	url = "https://bio-oracle.org/data/2.0/"
	ffs = readLines(url)

	ff = grep("^<li><a href=", ffs, value=T)
	ff = strsplit(ff, "> ")
	ff = sapply(ff, \(i) i[2])
	ff = gsub("</a></li>", "", ff)
	ff = grep("tif.zip$", ff, value=T)
	ff = gsub(".tif.zip$", "", ff)

	ff <- gsub("Current.Velocity", "Current_Velocity", ff)
	ff <- gsub("Cloud.cover", "Cloud_cover", ff)
	ff <- gsub("Diffuse.attenuation", "Diffuse_attenuation", ff)
	ff <- gsub("Dissolved.oxygen", "Dissolved_oxygen", ff)
	ff <- gsub("Ice.cover", "Ice_cover", ff)
	ff <- gsub("Ice.thickness", "Ice_thickness", ff)
	ff <- gsub("Primary.productivity", "Primary_productivity", ff)
	ff <- gsub("Light.bottom", "Light_bottom", ff)


	ff <- gsub("\\.Depth", "_Depth", ff)

	maketab <- function(s, n=2) {
		ss <- paste0("^", s, ".")
		i <- grep(ss, ff)
		ps = gsub(ss, "", ff[i])
		tps <- stringr::str_split(ps, "\\.", n=n)
		tps <- do.call(rbind, tps)
		tps[tps[,1] == tps[,2], 2] <- ""
		tps[,1] <- gsub("_", ".", tps[,1])
		tps[,2] <- gsub("_", ".", tps[,2])
		data.frame(group = gsub("Present.", "", s), tps)
	}

	a = maketab("Present.Surface")
	b = maketab("Present.Benthic", n=3)
	b$X1 <- NULL
	b = unique(b)
	ab = merge(a, b, by=2:3, all=T)
	ab$group.x <- NULL
	ab$group.y <- !is.na(ab$group.y)
	names(ab) <- c("var", "stat", "benthic")
	saveRDS(ab, "c:/github/rspatial/geodata/inst/ex/bior.rds")
}



bio_oracle <- function(path, var, stat, benthic=FALSE, depth="Mean", time="Present", rcp, ...) {

	path <- .get_path(path, "bio-oracle")
	
	v <- readRDS(system.file("ex/bior.rds", package="geodata"))
	
	stopifnot(time %in% c("Present", "2050", "2100"))
	if (time != "Present") {
		time <- paste0(time, "AOGCM")
		rcp <- as.character(rcp)
		stopifnot(rcp %in% c("26", "45", "60", "85"))
		rcp <- paste0("RCP", rcp)
		f <- paste0(time, ".", rcp)
	} else {
		f <- time
	}
	if (isTRUE(benthic)) {
		s <- "Benthic"
		stopifnot(depth %in% c("Min", "Mean", "Max"))
		f <- paste0(f, ".", s, ".", depth, ".Depth")
		vs <- v[v$var == var & v$stat == stat & v$benthic, ]
	} else {
		s <- "Surface"	
		f <- paste0(f, ".", s)
		vs <- v[v$var == var & v$stat == stat, ]
	}
	if (nrow(vs) == 0) {
		uv <- unique(v$var)
		if (!(var %in% uv)) {
			stop(paste("'var' should be one of:",  paste(uv, collapse=", ")))
		}
		us <- unique(v$stat)
		if (!(stat %in% us)) {
			stop(paste("'stat' should be one of:",  paste(us, collapse=", ")))
		}
		stop("this combination of 'var', 'stat' and 'benthic' is not available")
	}
	if (stat == "") {
		f <- paste0(f, ".", var, ".tif.tif")	
		zipf <- gsub(".tif$", ".zip", f)
	} else {
		f <- paste0(f, ".", var, ".", stat, ".tif")
		zipf <- paste0(f, ".zip")
	}
	
	outf <- file.path(path, f)
	if (file.exists(outf)) {
		return( rast(outf))
	}
	url  <- file.path("https://bio-oracle.org/data/2.0", basename(zipf))
	if (!.downloadDirect(url, file.path(path, zipf), unzip=TRUE, ...)) return(NULL)
	if (file.exists(outf)) {
		return( rast(outf))
	} else {
		message("something went wrong")
		return(NULL)
	}
}
	
