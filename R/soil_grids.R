	
.soil_grids_wcs <- function(var, depth, stat="mean", name="") {
	voi <- paste(var, depth, stat, sep="_")
	burl <- "https://maps.isric.org/mapserv?map=/map/" 
	wcs <- paste0(burl, voi, ".map&SERVICE=WCS&VERSION=2.0.1&DescribeCoverage")
	xm <- newXMLNode("WCS_GDAL")
	XML::newXMLNode("ServiceURL", wcs, parent=xm)
	XML::newXMLNode("CoverageName", voi, parent=xm)
	fxml <- paste0(tempfile(), ".xml")
	saveXML(xm, file = paste0(tempfile(), ".xml"))
	r <- rast(fxml)
	names(r) <- voi
	r
}	



.soil_grids_url <- function(var, depth, stat="mean", name="", vsi) {

	if (vsi) {
		sg_url <- "/vsicurl/https://files.isric.org/soilgrids/latest/data/"
	} else {
		sg_url <- "https://biogeo.ucdavis.edu/data/geodata/soil/soilgrids/"
	}
	
	var <- var[1]
	stopifnot(var %in% c("bdod", "cfvo", "clay", "nitrogen", "ocd", "phh2o", "sand", "silt", "soc", "wrb"))
	if (var == "wrb") {
		#h <- readLines("https://files.isric.org/soilgrids/latest/data/wrb/")
		#h <- grep("vrt<", h, value=TRUE)
		#h <- gsub("<li><a href=\"",  "", h)
		#h <- strsplit(h, ".vrt\"")
		#h <- sapply(h, function(i) i[1])
		h <- c('Acrisols', 'Albeluvisols', 'Alisols', 'Andosols', 'Arenosols', 'Calcisols', 'Cambisols', 'Chernozems', 'Cryosols', 'Durisols', 'Ferralsols', 'Fluvisols', 'Gleysols', 'Gypsisols', 'Histosols', 'Kastanozems', 'Leptosols', 'Lixisols', 'Luvisols', 'Nitisols', 'Phaeozems', 'Planosols', 'Plinthosols', 'Podzols', 'Regosols', 'Solonchaks', 'Solonetz', 'Stagnosols', 'Umbrisols', 'Vertisols') 
		name <- name[1]
		if (!(name %in% h)) {
			stop(paste("choose one of:", paste(h, collapse=", "))) 
		}
		u <- file.path(sg_url, var, paste0(name, ".vrt"))
	} else {
		depth <- as.character(round(depth[1]))
		if (var == "ocs") {
			stopifnot(depth == "30")	
			dd <- "0-30"
		} else {
			dpts <- c("5", "15", "30", "60", "100", "200")
			if (!(depth %in% dpts)) {
				stop(paste("depth must be one of:", paste(dpts, collapse=", ")))
			}
			dd <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")[depth == dpts]
		}
		stat <- stat[1]
		stopifnot(stat %in% c("mean", "uncertainty", "Q0.05", "Q0.5", "Q0.95"))
		if (vsi) {
			u <- file.path(sg_url, var, paste0(var, "_", dd, "cm_", stat, ".vrt"))
		} else {
			u <- file.path(sg_url, paste0(var, "_", dd, "cm_", stat, ".vrt"))		
		}
	}
	u
}

soil_world_vsi <- function(var, depth, stat="mean", name="") {
	u <- .soil_grids_url(var, depth, stat=stat, name=name, vsi=TRUE)
	rast(u)
}


soil_world <- function(var, depth, stat="mean", name="", path) {
	stopifnot(dir.exists(path))
	u <- .soil_grids_url(var, depth, stat=stat, name=name, vsi=FALSE)
	u <- gsub(".vrt$", "_30s.tif", u)
	filename <- basename(u)
	filepath <- file.path(path, filename)
	if (!file.exists(filepath)) {
		ff <- readLines("https://biogeo.ucdavis.edu/data/geodata/soil/soilgrids/files.txt")
		if (!(filename %in% ff)) {
			stop(paste("file not yet available:", filename))
		}
	}
	.donwload_url(u, filepath)
}


